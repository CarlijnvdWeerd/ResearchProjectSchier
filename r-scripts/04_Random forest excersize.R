library(randomForest)
data("iris")
str(iris)

set.seed(123) 
library(caret)

train_indices <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train_data <- iris[train_indices, ]
test_data<- iris[-train_indices, ]

table(train_data$Species)

rf_model <- randomForest(Species ~ ., data = train_data, ntree = 500, mtry = 3, importance = TRUE)
print(rf_model)  # View the model summary

predictions <- predict(rf_model, test_data)
table(predictions, test_data$Species)  # Confusion matrix

accuracy <- sum(predictions == test_data$Species) / nrow(test_data)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

importance(rf_model)  # Print importance scores
varImpPlot(rf_model)  # Plot variable importance

tuneRF(train_data[-5], train_data$Species, stepFactor = 1.5, improve = 0.01, ntreeTry = 200)
