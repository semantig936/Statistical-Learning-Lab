library(ISLR2)
library(ggplot2)
library(dplyr)
library(boot)
library(MASS)

# Randomly divide in training and test data
data("Default")
head(Default)
cor(Default)
num_data <- Default[, sapply(Default, is.numeric)]  # Select numeric columns
cor_matrix <- cor(num_data, use = "complete.obs")    # Compute correlation matrix
print(cor_matrix)
install.packages("corrplot")  # Install the package
library(corrplot)             # Load the package
corrplot(cor_matrix, method = "circle", addCoef.col = "black")  # Visualize the correlation matrix

#Logistic Regression
glm.fits=glm(default ~ student + balance,
             data=Default ,family =binomial )
summary (glm.fits)
coef(glm.fits)
library(stats)
log_likelihood <- logLik(glm.fits)
log_likelihood
summary (glm.fits)$coef
summary (glm.fits)$coef [,4]
glm.probs =predict (glm.fits,Default, type ="response")
glm.probs [1:10]
contrasts(Default$default)
glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")  # Convert to binary predictions
table(glm.pred,Default$default)
mean(glm.pred == Default$default)
#Splitting the dataset
trn <- sample(dim(Default)[1], 8000)
trn
Default_train <- Default[trn,]
Default_test <-Default[-trn,]
Default_test <- Default_test[, -1]
head(Default_test)

dim(Default_train)
dim(Default_test)

# fit logistic regression model 
glm.fit <- glm(default ~ student + balance,
               data = Default_train, family = binomial)

# predict using test data set
pred <- predict(glm.fit, Default_test, type = "response")
pred[1:8]

# classify the prediction in default = "Yes" or "No"
pred_class <- ifelse(pred >=0.5, "Yes", "No")
pred_class[1:5]

# Create confusion matrix
table(Default[-trn,]$default, pred_class)

# Test accuracy
mean(pred_class ==Default[-trn,]$default )

contrasts(Default$default)

?predict.glm

?cv.glm

summary(glm.fit)

head(Default)
attach(Default)
# Exploratory data analysis using boxplot
boxplot(balance ~ default)
boxplot(income ~ default)
ggplot(Default, aes(x = default, 
                    y = balance, fill = default))+ geom_boxplot()

ggplot(Default, aes(x = income, 
                    y = balance, color = default))+ geom_point()

# contingency table among two categorical variables
table(default,student)


# Fit logistic Regression Model

glm.def <- glm(default ~ student + balance + income, 
               data = Default, family = binomial)

# Cross-validation
# Create cost function for cross-validation
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cv.error <- cv.glm(Default, glm.def, cost, K = 10)
cv.error$delta

summary(glm.def)
# second order model
attach(Default)
glm.def1 <- glm(default ~ student + poly(balance,2), 
               data = Default, family = binomial)

summary(glm.def1)


dim(Default)
table(default)
cost <- function(default, pi = 0) mean(abs(default-pi) > 0.5)
# Modeling with 1-5 orders of balance variable
cv.error = rep(0,5)
cv.error                                                                                                                                                                                                                                                                                                                                                                           
for(i in 1:5){
  glm.fit <- glm(default ~ student + poly(balance,i), 
                 data = Default, family = binomial)
  cv.error[i]<- cv.glm(Default, glm.fit, cost, K = 100)$delta[1]
}
cv.error
plot(seq(1:5), cv.error, xlab = "Model Order", 
     ylab = "Avg Error", type = "o")

?cv.glm

# Using stepAIC function for model selection
glm.def <- glm(default ~ student + balance + income, 
               data = Default, family = binomial)

best_fit <- stepAIC(glm.def, direction = "both")

cv.error <- cv.glm(Default, best_fit, cost, K=100)
cv.error$delta

summary(best_fit)
