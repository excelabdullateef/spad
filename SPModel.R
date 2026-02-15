# Importing libraries

library(caret)
library(tidyverse)
setwd("C:/Users/user/Documents/Spf App")
# Importing the data set
spf <- read_csv("spf.csv")
spf1 <- read_csv("spf3.csv")
spf[sapply(spf, is.character)] <- lapply(spf[sapply(spf, is.character)], as.factor)
str(spf)

# Define the training control (10-fold cross-validation)
train_control <- trainControl(method = "repeatedcv", number = 100, repeats = 20)
cv_model <- train(ExScore ~ ., 
                  data = spf, 
                  method = "lm", # Keeps your Linear Regression preference
                  trControl = train_control)

cv_model1 <- train(ExScore ~ ., 
                  data = spf1, 
                  method = "lm", # Keeps your Linear Regression preference
                  trControl = train_control)
names(cv_model)
print(cv_model)
cv_model$modelInfo
final_model <- cv_model$finalModel
summary(cv_model)
?trainControl
cv_model$resample



## Random Forest Regressor + CV

train_control1 <- trainControl(method = "cv", number = 5)
rf_model <- train(ExScore ~ ., 
                  data = spf, 
                  method = "rf", 
                  trControl = train_control1,
                  importance = TRUE,
                  ntree = 500) # Standard number of trees

# 3. Check the results
print(rf_model)

# Performs stratified random split of the data set
set.seed(12)
TrainingIndex <- createDataPartition(spf$ExScore, p=0.8, list = FALSE)
TrainingSet <- spf[TrainingIndex,] # Training Set
TestingSet <- spf[-TrainingIndex,] # Test Set

write_csv(TrainingSet, "spftrain.csv")
write_csv(TestingSet, "spftest.csv")
head(spf)
TrainSet <- read.csv("spftrain.csv", header = TRUE)
TestSet <- read.csv("spftest.csv", header = TRUE)
levels(spf$ParEdLev)
# Building Linear Regression model
nrow(TestSet)
model <- lm(ExScore ~ ., data = TrainSet)
summary(model)
Exact_values <- TestSet$ExScore
Pred_values1 <- predict(model, TestSet)
mse_lm1 <- mean((Exact_values - Pred_values1)^2)
rmse_lm1 <- sqrt(mse_lm1)
mae_lm1 <- mean(abs(Exact_values - Pred_values1))
 
cat("Linear Regression Performance:\n")
cat("MSE:", round(mse_lm1, 3), "\n")
cat("RMSE:", round(rmse_lm1, 3), "\n")
cat("MAE:", round(mae_lm1, 3), "\n")

#Save model to RDS file
saveRDS(cv_model, "spfmodel.rds")
saveRDS(cv_model1, "spfmodel1.rds")
