##########################T3##########################
# T3-1 -----------------------------------------------
library(caret)
library(readr)

# Load data set
data <- read_csv("cleaned_ford.csv") 

# Split into Train set and Test set
set.seed(1234)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8, 0.2))
train <- data[sample, ]
test <- data[!sample, ]

# Train
model <- train( price ~ ., 
                data = train, 
                method = 'knn' ) 

# Predict
knn_output <- predict( model, test )

# Evaluate
postResample(knn_output, test$price)

# T3-2 -----------------------------------------------
# Using K fold with k = 5
train_control <- trainControl( method = 'cv', number = 5 )

# Train
model <- train( price ~ ., 
                data = train, 
                method = 'knn',
                trControl = train_control ) 

# Predict
knn_output <- predict( model, test )

# Evaluate
postResample(knn_output, test$price)

# T3-3 -----------------------------------------------
# Using K fold with k = 5
train_control <- trainControl( method = 'cv', number = 5 )

# Set grid for grid search for tuning parameters
grid <- expand.grid( k = seq( from = 1, to = 20, by = 2 ) )

# Train
model <- train( price ~ ., 
                data = train, 
                method = 'knn',
                metric = "RMSE",
                trControl = train_control,
                tuneGrid = grid ) 

# Print training results with different parameter settings
model 

# Plot the performance with different parameter settings
plot(model) 

# T3-4 -----------------------------------------------
# Using K fold with k = 5
train_control <- trainControl( method = 'cv', number = 5 )

grid <- expand.grid( k = 7 )

# Train
S_Train_time <- proc.time()
model <- train( price ~ ., 
                data = train, 
                method = 'knn',
                metric = "RMSE",
                trControl = train_control,
                tuneGrid = grid ) 
E_Train_time <- proc.time()
# Training Time
E_Train_time - S_Train_time

# Predict
S_Pred_time <- proc.time()
knn_output <- predict( model, test )
E_Pred_time <- proc.time()
# Predicting Time
E_Pred_time - S_Pred_time

# Evaluation predictions
postResample(knn_output, test$price)

##########################T4##########################
# T4-1 -----------------------------------------------
library(dplyr, warn.conflicts = F)

sample_train <- train %>% select(price, mileage, mpg, year)

sample_test <- test %>% select(price, mileage, mpg, year)
sample_test <- sample_test[1,]

# Plot training data feature distribution
library(plotly, warn.conflicts = FALSE)

fig <- plot_ly(sample_train, 
               x = sample_train$mileage, 
               y = sample_train$mpg, 
               z = sample_train$year, 
               marker = list(size = 0.3), 
               color = I("blue"),
               type="scatter3d", 
               mode = "markers",
               name = "Training Data")
fig

# Plot training data and test feature distribution
fig <- fig %>% add_trace(sample_test,
                         x = sample_test$mileage, 
                         y = sample_test$mpg, 
                         z = sample_test$year,
                         marker = list(size = 2), 
                         color = I("red"),
                         mode = "markers",
                         type="scatter3d",
                         name = "Test Data")
fig

# K nearest neighbours
k <- 7
# Compute Distance
train_distance <- sample_train %>% mutate( distance = (sample_test$mileage - c(mileage)) ^ 2 + 
                                             (sample_test$mpg - c(mpg)) ^ 2 + 
                                             (sample_test$year - c(year)) ^ 2 ) %>% 
  arrange(distance)
# Find K nearest
neighbours <- train_distance[c(1:k),]

# Plot K nearest neighbours
fig <- fig %>% add_trace(neighbours,
                         x = neighbours$mileage, 
                         y = neighbours$mpg, 
                         z = neighbours$year,
                         marker = list(size = 1.5), 
                         color = I("green"),
                         mode = "markers",
                         type="scatter3d",
                         name = "Neighbours")
fig
