##########################T1##########################
# T1-1 -----------------------------------------------
# Load Data
library(readr)
ford <- read_csv("ford.csv")
# Dimensionality
dim(ford)
# Structure
str(ford)
# Summary
summary(ford)

# T1-2 -----------------------------------------------
library(ggplot2)

amount_model = aggregate(ford$price ~ model, sum, data = ford)

ggplot(amount_model) + 
  geom_bar(aes(x = model, y = `ford$price`), 
           fill = '#FA7F6F', 
           stat = 'identity', 
           width = 0.8) + 
  coord_flip() + 
  labs(x = 'Model', 
       y = 'Amount of Sales', 
       title = "Total Amount of Sales per Model") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 10))

# T1-3 -----------------------------------------------
amount_year = aggregate(ford$price ~ year, sum, data = ford)

ggplot(amount_year) + 
  geom_bar(aes(x = as.character(year), y = `ford$price`), 
           fill = '#8ECFC9', 
           stat = 'identity', 
           width = 0.8) + 
  labs(x = 'Year', 
       y = 'Amount of Sales', 
       title = "Total Amount of Sales per Year") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 5), 
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 10))

# T1-4 -----------------------------------------------
mean_model = aggregate(ford$price ~ model, mean, data = ford)

ggplot(mean_model) + 
  geom_bar(aes(x = model, y = `ford$price`), 
           fill = '#FFBE7A', 
           stat = 'identity', 
           width = 0.8) +
  coord_flip() +
  labs(x = 'Model', 
       y = 'Average Price', 
       title = "Average Car Price for Each Model") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 5), 
        axis.title = element_text(size = 10))

# T1-5 -----------------------------------------------
mean_year = aggregate(ford$price ~ year, mean, data = ford)

ggplot(mean_year) + 
  geom_bar(aes(x = as.character(year), y = `ford$price`), 
           fill = '#82B0D2', 
           stat = 'identity', 
           width = 0.8) + 
  labs(x = 'Year', 
       y = 'Average Price', 
       title = "Average Car Price per Year") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 5), 
        axis.text.y = element_text(size = 7), 
        axis.title = element_text(size = 10))


##########################T2##########################
# T2-1 -----------------------------------------------
colSums(is.na(ford))
cleaned_miss_ford = na.omit(ford)

# T2-2 -----------------------------------------------
sum(duplicated(cleaned_miss_ford))
cleaned_dup_ford = cleaned_miss_ford[!duplicated(cleaned_miss_ford),]

# T2-3 -----------------------------------------------
# Check the outliers for the numeric attributes
# Numeric attributes
num_cols <- list('year', 'mileage', 'tax', 'mpg', 'engineSize')
for (i in num_cols){
  # Box-plot the distribution
  boxplot(cleaned_dup_ford[, c(i)], main = paste(i, 'distribution'))
}

# Remove the outliers
# Create a new dataframe
cleaned_out_ford <- cleaned_dup_ford
for (i in num_cols) {
  # Get the outliers using box-plot
  outliers <- boxplot(cleaned_out_ford[[i]], plot=FALSE)$out
  # Remove the outliers in the data frame
  cleaned_out_ford <- cleaned_out_ford[-which(cleaned_out_ford[[i]] %in% outliers),]
}

# T2-4 -----------------------------------------------
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# New a data frame
cleaned_norm_ford <- cleaned_out_ford
for (i in num_cols) {
  # Apply min-max norm in each column 
  cleaned_norm_ford[c(i)] <- as.data.frame(lapply(cleaned_out_ford[c(i)], min_max_norm))
}

# T2-5 -----------------------------------------------
library(mltools)
library(data.table)

# Categorical attributes
cat_cols <- list('model', 'transmission', 'fuelType')
# New a data frame
cleaned_cat_ford <- cleaned_norm_ford
# Apply one hot in the columns
cleaned_cat_ford <- one_hot(as.data.table(cleaned_cat_ford))

# T2-6 -----------------------------------------------
# Save the dataset into a CSV file
write.csv(cleaned_cat_ford, file = "cleaned_ford.csv", row.names = F)