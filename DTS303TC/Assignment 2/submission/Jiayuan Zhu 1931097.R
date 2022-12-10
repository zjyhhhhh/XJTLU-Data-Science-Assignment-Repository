##########################Set up##########################
library(readr)
library(zoo, warn.conflicts = F)
library(tidymodels, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(modeltime)
library(timetk)
library(forecast, warn.conflicts = F)
library(lubridate, warn.conflicts = F)

data_make <- read_csv("norway_new_car_sales_by_make.csv") 
data_model <- read_csv("norway_new_car_sales_by_model.csv") 
data_month <- read_csv("norway_new_car_sales_by_month.csv") 

##########################Part A##########################
# T1------------------------------------------------------
# Year-wise total car sales
year_sales <- aggregate(Quantity ~ Year, sum, data = data_make)
year_sales

ggplot(year_sales) + 
  geom_bar(aes(x = as.character(Year), y = `Quantity`), 
           fill = '#82B0D2', 
           stat = 'identity', 
           width = 0.8) + 
  labs(x = 'Year', 
       y = 'Car Sales', 
       title = "Car Sales per Year") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 15))

# T2------------------------------------------------------
# Month-wise total car sales of 2016
M2016_sales <- aggregate(Quantity ~ Month, sum, data = data_month %>% filter(Year == 2016))
M2016_sales

p2 <- ggplot(M2016_sales) + 
  geom_bar(aes(x = Month, y = `Quantity`), 
           fill = '#8ECFC9', 
           stat = 'identity', 
           width = 0.8) + 
  labs(x = 'Month', 
       y = 'Sales', 
       title = "Car Sales per Month in 2016") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 15))

p2 + scale_x_continuous(labels = as.character(data_month$Month), 
                        breaks = data_month$Month)

# T3------------------------------------------------------
# Monthly total car sales from 2007 to 2017
month_sales <- aggregate(Quantity ~ Month, sum, data = data_month)
month_sales

p2 <- ggplot(month_sales) + 
  geom_bar(aes(x = Month, y = Quantity), 
           fill = '#FFBE7A', 
           stat = "identity", 
           width = 0.8) +
  geom_text(aes(y = Quantity, label = factor(Quantity), x = Month), color="black", size=2.5, position = position_stack(vjust = 0.95)) +
  labs(x = 'Month',
       y = 'Sales',
       title = "Car Sales per Month") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 15))

p2 + scale_x_continuous(labels = as.character(month_sales$Month),
                        breaks = month_sales$Month)

# Highest
highest <- month_sales %>% filter(Quantity == max(Quantity))
highest$Month
# Lowest
lowest <- month_sales %>% filter(Quantity == min(Quantity))
lowest$Month

# T4------------------------------------------------------
# The total amount of the sales for each manufacturer from 2007 to 2017
make_sales <- aggregate(Quantity ~ Make, sum, data = data_make)
make_sales

# Select the top ten manufacturers in terms of sales
best_makes <- make_sales[order(make_sales$Quantity, decreasing = T),][1:10,]
make_sales <- make_sales[make_sales$Make%in% best_makes$Make,]

# Plot the sales of the top manufacturers
ggplot(make_sales) + 
  geom_bar(aes(x = Make, y = Quantity), 
           fill = '#82B0D2', 
           stat = "identity", 
           width = 0.8) +
  coord_flip() +
  labs(x = 'Brand',
       y = 'Sales',
       title = "Car Sales of Top Ten Car Brands") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 15))

# T5------------------------------------------------------
# Top 10 car brands
best_makes$Make

make_sales <- data_make %>%
  filter(Make %in% best_makes$Make) %>%
  group_by(Make, Year) %>%
  summarise(Sum = sum(Quantity))

p5 <- ggplot(make_sales) + 
  geom_line(aes(x = make_sales$Year, y = make_sales$Sum, color = make_sales$Make)) +
  labs(x = 'Year',
       y = 'Sales',
       title = "Year-wise Car Sales of Top Ten Car Brands") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 15))

p5 + scale_x_continuous(labels = as.character(make_sales$Year), 
                        breaks = make_sales$Year)

# T6------------------------------------------------------
# Select the Toyata data
toyota <- data_model %>% filter(grepl("Toyota", Model))
# Add up the quantity of each model
toyota_sales <- aggregate(Quantity ~ Model, sum, data = toyota)

ggplot(toyota_sales, aes(x = " ", y = Quantity, fill = Model)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Blues")

# T7------------------------------------------------------
best_models <- data_model %>% 
  filter(Year == 2015) %>%                           # select data of 2015
  group_by(Model) %>%                                # group by models
  summarize(
    SumQuantity=sum(Quantity), Make=unique(Make)     # add up quantity of each model
  ) %>%
  group_by(Make) %>%                                 # group by makes
  mutate(
    MaxQuantityByMark = max(SumQuantity, na.rm = T)  # choose the max sum of quantity
  ) %>%
  filter(SumQuantity == MaxQuantityByMark)           # select the model with max quantity

# Output the best models and makes they belong to
best_models %>% select('Model', 'Make')

# T8------------------------------------------------------
best_models <- data_model %>%
  group_by(Model) %>%                                # group by models
  summarize(
    SumQuantity=sum(Quantity), Make=unique(Make)     # add up quantity of each model
  ) %>%
  group_by(Make) %>%                                 # group by makes
  mutate(
    MaxQuantityByMark = max(SumQuantity, na.rm = T)  # choose the max sum of quantity
  ) %>%
  filter(SumQuantity == MaxQuantityByMark)           # select the model with max quantity

# Output the models of each manufacturer has the highest sale sand makes they belong to
best_models %>% 
  select('Model', 'Make')  %>% 
  as_tibble() %>% 
  print(n=Inf)

# T9------------------------------------------------------
best_models <- data_model %>%
  group_by(Model) %>%                                # group by models
  summarize(
    SumQuantity=sum(Quantity), Make=unique(Make)     # add up quantity of each model
  ) %>%
  group_by(Make) %>%                                 # group by makes
  mutate(
    MinQuantityByMark = min(SumQuantity, na.rm = T)  # choose the min sum of quantity
  ) %>%
  filter(SumQuantity == MinQuantityByMark)           # select the model with min quantity

# Output the models of each manufacturer has the lowest sale sand makes they belong to
best_models %>% select('Model', 'Make')

# T12-----------------------------------------------------
# Compute the year-wise share of diesel car sales
diesel_percent <- data_month %>%
  group_by(Year) %>%
  summarize(DieselPercent=sum(Quantity_Diesel) / sum(Quantity)) 

diesel_percent

# Plot in line graph
p12 <- ggplot(diesel_percent) + 
  geom_line(aes(x = Year, y = DieselPercent, group = 1), color='#82B0D2') +
  geom_point(x = diesel_percent$Year, y = diesel_percent$DieselPercent, color='#82B0D2') +
  labs(x = 'Year',
       y = 'Diesel Car Sales Pecentage',
       title = "Year-wise Share of Diesel Car Sales in Total Sales") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 15))

p12 + scale_x_continuous(labels = as.character(diesel_percent$Year),
                         breaks = diesel_percent$Year)

##########################Part B##########################
# 1. Load data
# Merge column "Year" and "Month" to form a new column "Date"
data_month$Date <- as.Date(paste(data_month$Year, data_month$Month, "01", sep="-"), "%Y-%m-%d")

# Select column "Date" and "Quantity"
sales_data <- data_month %>% 
  select("Date", "Quantity") %>%     # select
  set_names(c("date", "value"))      # rename

# Check the data
glimpse(sales_data)

# Plot the sales over time
sales_data %>%
  plot_time_series(date, value, 
                   .title = "Time Series Plot",
                   .x_lab = "Date",
                   .y_lab = "Sales",
                   .interactive = F)

# 2. Split data
# Split the data set into training set and test set. Use the last year as the test set.
splits <- sales_data %>% time_series_split(assess = "1 year", cumulative = T)

# The test set is marked in read.
splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date, value, .interactive = F)

# 3. Model
# Auto ARIMA
model_arima = arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(value ~ date, training(splits))
model_arima

# Prophet
model_prophet = prophet_reg(seasonality_yearly = TRUE, 
                            seasonality_weekly = FALSE,
                            seasonality_daily = FALSE) %>% 
  set_engine("prophet") %>% 
  fit(value ~ date, training(splits))
model_prophet

# Prepocess
recipe_spec = recipe(value ~ date, training(splits)) %>% 
  step_timeseries_signature(date) %>% 
  step_fourier(date, period = 12, K =5) %>% 
  step_dummy(all_nominal())                      

recipe_spec %>% prep() %>% juice()

# Elastic Net
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))

# Prophet Boost
model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE, 
                                          seasonality_weekly = FALSE,
                                          seasonality_daily = FALSE) %>%
  set_engine("prophet_xgboost") 

# 4. Model evaluation and model selection
workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

model_table <- modeltime_table(
  model_arima, 
  model_prophet,
  workflow_fit_glmnet,
  workflow_fit_prophet_boost
) 

model_table

# Predict the test set
calibration_table = model_table %>% 
  modeltime_calibrate(testing(splits))

# Demonstrate the performance of models on the test set through various criterions
calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

# Plot the prediction results of different models on the test set
calibration_table %>%
  modeltime_forecast(actual_data = sales_data) %>%
  plot_modeltime_forecast(.interactive = FALSE)

# 5. Predict
# Calculate how many months are left until December 2020
diff_time = ceiling(as.numeric(difftime(as.Date("2020-12-01", "%Y-%m-%d"), 
                                        max(sales_data$date),units="days")) / (365.25/12))
diff_time

sales_all <- calibration_table %>%
  # Remove the models with low accuracy
  filter(.model_id != 1 & .model_id != 2) %>%
  # Refit and Forecast Forward
  modeltime_refit(sales_data) %>%
  modeltime_forecast(h = "47 months", actual_data = sales_data) %>%
  plot_modeltime_forecast(.interactive = FALSE)

sales_all

# Split the prediction of 2020
preds <- sales_all$data %>% 
  filter(.key != "actual") %>%
  filter(.index >= as.Date("2020-01-01", "%Y-%m-%d"))

# Plot the prediction of 2020 of different models
preds %>% plot_modeltime_forecast(.interactive = FALSE)

# Elastic Net
GLMNet_preds <- subset(preds, .model_id == 3) %>%
  select('.index', '.value') %>%
  set_names(c("date", "value"))

GLMNet_preds

# Prophet Boost
XGBoost_preds <- subset(preds, .model_id == 5) %>%
  select('.index', '.value') %>%
  set_names(c("date", "value"))
XGBoost_preds

# Ensemble
ensemble_preds <- aggregate(preds[c(".value")], preds[c(".index")], mean) %>%
  set_names(c("date", "value"))
ensemble_preds

