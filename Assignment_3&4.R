# Write an R program to carry out regression study on Sacramento dataset. The prices of houses
# depend on area, number of bedrooms and bathrooms. Using ANOVA analysis, identify which model
# is best of these: prices ~ area + beds + baths, prices ~ area + beds, prices ~ area.

# Load the necessary libraries
library(modeldata)

# Load the Sacramento dataset
data(Sacramento)

# Check the dataset
head(Sacramento)

# Fit three models
model1 <- lm(price ~ sqft + beds + baths, data = Sacramento)  # Full model with area, beds, and baths
model2 <- lm(price ~ sqft + beds, data = Sacramento)          # Model with area and beds
model3 <- lm(price ~ sqft, data = Sacramento)                 # Model with area only

# Perform ANOVA to compare the models
anova_results <- anova(model3, model2, model1)

# Output the results
print(anova_results)

# Write an R program to do Naïve Bayes classification on the attached Glass 
# dataset with 80% data as training set and remaining as test. Present the 
# classification prediction results as confusion matrix.

# Load the necessary libraries
library(e1071)   # for Naive Bayes
library(caTools) # for data splitting
library(caret)   # for confusion matrix

# Load the Glass dataset
glass <- read.csv("C:\\Users\\angel\\Downloads\\Glass.csv")

# Ensure that the 'Class' column is treated as a factor
glass$Class <- as.factor(glass$Class)

# Split the data into training (80%) and test (20%) sets
set.seed(123)
split <- sample.split(glass$Class, SplitRatio = 0.80)

# Create training and test sets
train_set <- subset(glass, split == TRUE)
test_set <- subset(glass, split == FALSE)

# Train Naive Bayes model on all attributes
nb_model_all <- naiveBayes(Class ~ ., data = train_set)
pred_all <- predict(nb_model_all, test_set)
pred_all <- as.factor(pred_all)
confusion_all <- confusionMatrix(pred_all, test_set$Class)

# Train Naive Bayes model on A1 and A2
nb_model_1 <- naiveBayes(Class ~ A1 + A2, data = train_set)
pred_1 <- predict(nb_model_1, test_set)
pred_1 <- as.factor(pred_1)
confusion_1 <- confusionMatrix(pred_1, test_set$Class)

# Train Naive Bayes model on A8 and A9
nb_model_2 <- naiveBayes(Class ~ A8 + A9, data = train_set)
pred_2 <- predict(nb_model_2, test_set)
pred_2 <- as.factor(pred_2)
confusion_2 <- confusionMatrix(pred_2, test_set$Class)

# Train Naive Bayes model on A4, A5, and A6
nb_model_3 <- naiveBayes(Class ~ A4 + A5 + A6, data = train_set)
pred_3 <- predict(nb_model_3, test_set)
pred_3 <- as.factor(pred_3)
confusion_3 <- confusionMatrix(pred_3, test_set$Class)

# Comparing Accuracy across all models
accuracy_all <- confusion_all$overall['Accuracy']
accuracy_1 <- confusion_1$overall['Accuracy']
accuracy_2 <- confusion_2$overall['Accuracy']
accuracy_3 <- confusion_3$overall['Accuracy']

cat("\nAccuracy Comparison:\n")
cat("All attributes A1 to A9: ", accuracy_all, "\n")
cat("Attributes A1 and A2: ", accuracy_1, "\n")
cat("Attributes A8 and A9: ", accuracy_2, "\n")
cat("Attributes A4, A5, and A6: ", accuracy_3, "\n")

# Write an R program to do time-series study on airmiles dataset. Show the
# chart plot for the dataset and its moving averages considering n = 2. 
# Predict the values for next two years on ARIMA model.

# Load necessary libraries
library(forecast)
library(ggplot2)

# Load the airmiles dataset
data("airmiles")

# Plot the airmiles dataset
plot(airmiles, main = "Airmiles Time Series", xlab = "Time", ylab = "Miles")

# Moving averages with n = 2
ma_2 <- filter(airmiles, rep(1/2, 2), sides = 2)
lines(ma_2, col = "red", lwd = 2, lty = 2)  # Adding moving averages line to the plot

# Fit an ARIMA model to the data
arima_model <- auto.arima(airmiles)

# Forecast for the next two years
forecast_values <- forecast(arima_model, h = 24)

# Plot the forecast values
plot(forecast_values, main = "ARIMA Forecast for Airmiles", xlab = "Time", ylab = "Miles")
