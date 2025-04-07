setwd("C:/Study/Semester_6/Statistical_Learning_Lab")
getwd()
data <- read.csv("diabetes.csv")
head(data)

cor(data)
str(data)
corr_matrix <- cor(data, use = "complete.obs")
print(corr_matrix)
library(corrplot)
corrplot(corr_matrix, method = "circle", addCoef.col = "black")

install.packages("ggplot2", dependencies=TRUE)
install.packages("GGally")

library(ggplot2)
library(GGally)
for (col in names(data)[-which(names(data) == "Outcome")]) {
  print(ggplot(data, aes(x = as.factor(Outcome), y = get(col))) +
          geom_boxplot() +
          labs(title = paste("Box Plot of", col, "by Outcome"),
               x = "Outcome", y = col) +
          theme_minimal())
}
pairs(data, col = data$Outcome)
ggpairs(data, aes(color = factor(Outcome), alpha = 0.5))

set.seed(97)
test_ind <- sample(1:nrow(data), size = 0.2*nrow(data))
test_data <- data[test_ind, ]
train_data <- data[-test_ind, ]

m1 <- glm(Outcome ~ ., data=train_data, family = binomial)
summary(m1)
coef(m1)
logLik(m1)
deviance(m1)

prob <- predict(m1, newdata=test_data, type = "response")
class <- ifelse(prob >= 0.5, 1, 0)

install.packages("caret")
library(caret)
confusion_matrix <- table(Predicted = class, Actual = test_data$Outcome)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)
precision <- confusion_matrix[2,2] / sum(confusion_matrix[,2])  
recall <- confusion_matrix[2,2] / sum(confusion_matrix[2,])    
f1_score <- 2 * (precision * recall) / (precision + recall)  
print(f1_score)

m2 <- glm(Outcome ~ Pregnancies + Glucose + BMI, data = train_data, family = binomial)
summary(m2)
logLik(m2)
deviance(m2)
