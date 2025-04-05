library(ISLR)
library(ggplot2)
library(boot)
library(caret)
set.seed(2)
# Randomly sample 196 obs out of 392 observations for training set
data(Auto)
View(Auto)
trainIndex <- createDataPartition(Auto$mpg, p = 0.8, list = FALSE)
train <- Auto[trainIndex, ]
test <- mtcars[-trainIndex, ]
# Fit Linear Model on training data
lm.fit <- lm(mpg ~ horsepower, data = train)
summary(lm.fit)
# Calculate MSE for validation set
# Make predictions on the training set
train_predictions <- predict(lm.fit, newdata = train)

# Calculate the Training MSE
train_actuals <- train$mpg
train_mse <- mean((train_actuals - train_predictions)^2)

# Print the Training MSE
print(paste("Training MSE:", train_mse))

# Make predictions on the test set
test_predictions <- predict(lm.fit, newdata = test)

# Calculate the Test MSE
test_actuals <- testData$mpg
test_mse <- mean((test_actuals - test_predictions)^2)

# Print the Test MSE
print(paste("Test MSE:", test_mse))attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

train[1:10]
length(train)
dim(Auto)
head(Auto)

lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
summary(lm.fit2)
mean((mpg - predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
summary(lm.fit3)
mean((mpg - predict(lm.fit3,Auto))[-train]^2)

library(boot)
# glm with no family arguement is same as lm
glm.fit <- glm(mpg ~ horsepower, data = Auto)
summary(glm.fit)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# Fit Linear Models of order 1-5
cv.error = rep(0,5)
cv.error                                                                                                                                                                                                                                                                                                                                                                           
for(i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error[i]<- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
plot(seq(1,5), cv.error, xlab = "Model Order", ylab = "MSE", type = "o")

boxplot( mpg ~ factor(cylinders), col = "blue")
ggplot(Auto, aes(x = factor(cylinders), y = mpg, fill = factor(cylinders))) + geom_boxplot()


# K fold CV
set.seed(17)
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error.10[i]<- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

dim(Portfolio)
## Bootstrapping

alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)




head(Portfolio)

alpha.fn(Portfolio, sample(100,100, replace = T))

library(boot)
boot(Portfolio, alpha.fn, R = 1000)

boot.fn = function(data, index){
    lm.fit <- lm(mpg ~ horsepower, data = data, subset = index)
    return (coef(lm.fit))
}

boot.fn(Auto, 1:392)
boot(Auto, boot.fn, 1000)
