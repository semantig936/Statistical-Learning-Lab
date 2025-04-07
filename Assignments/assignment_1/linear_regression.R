# Loading the data
getwd() #directory check
setwd("C:/Study/Semester_6/Statistical_Learning_Lab")
getwd()
data <- read.csv("manufacturing.csv")
head(data) #since number is specified, I took the default,

# matrix plot and correlation analysis
pairs(data)
corr_mat <- cor(data)
print(corr_mat)

# plotting the data for the sake of understanding
install.packages("corrplot")
library(corrplot)
corrplot(corr_mat, method="shade")

# Fitting the actual linear regression model
model <- lm(Quality.Rating ~ Temperature...C. + Pressure..kPa. + Material.Fusion.Metric + Material.Transformation.Metric, data = data)
summary(model)

# Error analysis and NPP
residuals <- model$residuals
qqnorm(residuals)
qqline(residuals, col="blue")

# Splitting the data into test and train sets and then performing linear regression 

# Splitting into test and train sets
set.seed(97)
test_ind <- sample(1:nrow(data), size=0.2*nrow(data))
test_data <- data[test_ind, ]
train_data <- data[-test_ind, ]

# Training using the train set and then predicting using the test set
train_model <- lm(Quality.Rating ~ Temperature...C. + Pressure..kPa. + Material.Fusion.Metric + Material.Transformation.Metric, data=train_data)
summary(train_model)
predictions <- predict(train_model, newdata = test_data)
summary(predictions)

# Calculating the RMSE
true_values <- test_data$Quality.Rating
rmse <- sqrt(mean((predictions - true_values)^2))
print(paste("RMSE: ",rmse))
