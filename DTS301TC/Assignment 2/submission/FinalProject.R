##########################Set up##########################
library(readxl)
library(tidyverse)
library(stringr)
library(tidytext)
library(SnowballC)
library(ldatuning)
library(lda)
library(LDAvis)
library(tm)
library(slam)
library(topicmodels)
library(maps)
library(sf)

col_types <- c("text", "text", "text", "text", "text", "text", 
               "numeric", "numeric", "numeric", "numeric", "numeric")
data <- read_excel("TwitterDataset.xlsx", col_types = col_types)

data <- data %>%
  mutate_at(c("Favs", "RTs", "Followers"), function(x)(replace_na(x,0)))

############################T1############################
# T1-1----------------------------------------------------
RankTweets <- data %>%
  mutate(SumFavsRTs = Favs + RTs) %>%
  arrange(desc(SumFavsRTs))

RankTweets[1:10, ]

# T1-2----------------------------------------------------
RankUsers <- data %>%
  select("User Name", "Nickname", "Followers") %>%
  filter(!duplicated(Nickname)) %>%
  arrange(desc(Followers))

RankUsers[1:10, ]

# T1-3----------------------------------------------------
TimeTweets <- data %>%
  mutate(Hour = str_split(Hour,":",simplify=T)[,1]) %>%
  group_by(Hour) %>%
  summarise(Count = n())

ggplot(TimeTweets) + 
  geom_line(aes(x = Hour, y = Count, group = 1), color='#82B0D2') +
  geom_point(x = TimeTweets$Hour, y = TimeTweets$Count, color='#82B0D2') +
  labs(x = 'Hour',
       y = 'Number of Tweets',
       title = " ") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 15))

# T1-4----------------------------------------------------
## Get the states map, turn into sf object
US <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Make it a spatial dataframe, using the same coordinate system as the US spatial dataframe
CoordsData <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(US))

#.. and perform a spatial join!
sf_use_s2(FALSE)
StateTweets <- st_join(CoordsData, US)

StateTweets <- StateTweets %>% 
  drop_na(ID) %>%
  group_by(ID) %>%
  summarise(Count = n()) %>%
  arrange(ID)

us <- map_data("state")

ggplot() + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15) + 
  geom_map(data=StateTweets, map=us,
           aes(fill=Count, map_id=ID),
           color="#ffffff", size=0.15) + 
  scale_fill_continuous(low='thistle2', 
                        high='darkred', 
                        guide='colorbar') + 
  labs(x=NULL, y=NULL) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())

############################T2############################
# T2-1----------------------------------------------------
clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove Emojis
    str_remove_all('[:emoji:]') %>%
    # Remove mentions
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: "
    str_remove_all("RT:? ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove multiple spaces by single space
    str_squish() 
}

d_clean <- data %>%
  mutate_at(c("Tweet content"), clean_tweets)

head(d_clean$`Tweet content`)

# T2-2----------------------------------------------------
d_token <- d_clean %>%
  # Remove numbers
  mutate(`Tweet content` = gsub("[[:digit:]]", '', `Tweet content`)) %>%
  # Split into tokens
  unnest_tokens(word, `Tweet content`) %>%
  # Remove stop words
  anti_join(stop_words)

head(d_token$word)

############################T3############################
# Set the minimum frequency of words
minimumFrequency <- 20
# Change to corpus 
corpus <- VCorpus(VectorSource(d_token$word)) 
# Get DocumentTermMatrix
DTM <- DocumentTermMatrix(corpus, 
                          control = list(bounds = 
                                           list(global = c(minimumFrequency, Inf))))
# Remove the duplicate
DTM = DTM[unique(DTM$i), ]

# Find the best K
result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 4, to = 12, by = 2),
  control = list(seed = 12345),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Number of topics
K <- 10

set.seed(12345)
# Compute the LDA model, inference via 1000 iterations of Gibbs sampling
GibbsModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 1000, verbose = 200))
# Compute the LDA model using VEM approach
VEMModel <- LDA(DTM, K, method="VEM")

perplexity(VEMModel, DTM)
perplexity(GibbsModel, DTM)

# apply auto tidy using tidy and use beta as per-topic-per-word probabilities
topic <- tidy(GibbsModel,matrix = "beta")

# choose 15 words with highest beta from each topic
top_terms <- topic %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

# plot the topic and words for easy interpretation
plot_topic <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 15)) + 
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

plot_topic

topicmodels2LDAvis <- function(x, ...){
  post <- posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(GibbsModel))




