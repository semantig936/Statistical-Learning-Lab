#changing the directory
setwd("C:/Study/Semester_6/Statistical_Learning_Lab")
getwd()

#loading the data set
data <- read.csv("diabetes.csv")
head(data)

#obtaining a summary about the different parameters as well as the outcome
summary(data)

#obtaining and printing the correlation matrix
corr_matrix <- cor(data)
print(corr_matrix)

#obtaining the box plot
install.packages("reshape2") 
library(reshape2) #this library will plot box plots for all parameters at one 
library(ggplot2)

data_long <- melt(data, id.vars = "Outcome")
ggplot(data_long, aes(x = as.factor(Outcome), y = value, fill = as.factor(Outcome))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots of Predictors by Diabetes Outcome",
       x = "Diabetes Outcome",
       y = "Value") +
  scale_fill_manual(values = c("blue", "red"))

#obtaining the scatter plot
install.packages("GGally") # to get all scatter plots at once
library(GGally)
ggpairs(data, aes(color = as.factor(Outcome)))

#installing the package MASS (because I don't think it has been installed)
install.packages("MASS")
library(MASS)

#splitting into train and test data
set.seed(97)
sample.size <- floor(0.8*nrow(data))
train.indices <- sample(seq_len(nrow(data)), size = sample.size)
train.data <- data[train.indices, ]
test.data <- data[-train.indices, ]

#fitting an lda
lda.model <- lda(Outcome ~ ., data=train.data)
print(lda.model)

#predicting for the test data from the lda model fitted
lda.pred <- predict(lda.model, test.data)

# Printing few predictions as well as the posterior probabilities for each class
head(lda.pred$class)  
head(lda.pred$posterior)  

#getting the confusion matrix
library(caret)

#converted them to factor because they had to be of the same "level"
test.data$Outcome <- as.factor(test.data$Outcome)  
lda.pred$class <- as.factor(lda.pred$class)

#Confusion matrix
conf_mat <- confusionMatrix(lda.pred$class, test.data$Outcome)
print(conf_mat)

#extracting accuracy from the confusion matrix
accuracy <- conf_mat$overall["Accuracy"]
print(accuracy)

#obtaining the precision and recall (we need them for the confusion matrix)
precision <- as.numeric(conf_mat$byClass["Precision"])
recall <- as.numeric(conf_mat$byClass["Recall"])

# Compute F1-score
F1_score <- 2 * (precision * recall) / (precision + recall)
print(F1_score)

install.packages("class")
library(class)

#fitting the QDA model and predicting the class on the test data
qda.model <- qda(Outcome ~ ., data = train.data)
print(qda.model)
qda_pred <- predict(qda.model, test.data)

#extracting predicted class labels
qda_class <- qda_pred$class
qda_class <- as.factor(qda_class)

#the confusion matrix, accuracy and F1 score of QDA
qda_conf_matrix <- confusionMatrix(qda_class, test.data$Outcome)
print(qda_conf_matrix)
qda_accuracy <- qda_conf_matrix$overall["Accuracy"]
print(qda_accuracy)
qda_precision <- as.numeric(qda_conf_matrix$byClass["Precision"])
qda_recall <- as.numeric(qda_conf_matrix$byClass["Recall"])
qda_f1_score <- 2 * (qda_precision * qda_recall) / (qda_precision + qda_recall)
print(qda_f1_score)


#obtaining the X and y for KNN
X.train <- train.data[, -which(names(train.data) == "Outcome")]
X.test <- test.data[, -which(names(test.data) == "Outcome")]
train.data$Outcome <- as.factor(train.data$Outcome)
test.data$Outcome <- as.factor(test.data$Outcome)
y.train <- train.data$Outcome
y.test <- test.data$Outcome

#KNN with K = 5 (and then printing the result)
knn.pred <- knn(train = X.train, test = X.test, cl = y.train, k = 5)
head(knn.pred)

# Computing confusion matrix, accuracy and F1 score for KNN
knn_conf_matrix <- confusionMatrix(knn.pred, y.test)
print(knn_conf_matrix)
knn_accuracy <- knn_conf_matrix$overall["Accuracy"]
print(knn_accuracy)
knn_precision <- as.numeric(knn_conf_matrix$byClass["Precision"])
knn_recall <- as.numeric(knn_conf_matrix$byClass["Recall"])
knn_f1_score <- 2 * (knn_precision * knn_recall) / (knn_precision + knn_recall)
print(knn_f1_score)


#installing packages for the ROC curve
install.packages("pROC")
library(pROC)

#predictions using the LDA and the QDA models (probabilities for having diabetes)
lda_probs <- predict(lda.model, test.data)$posterior[, 2]
qda_probs <- predict(qda.model, test.data)$posterior[, 2]

#Computing the ROC for the LDA and QDA
lda_roc <- roc(test.data$Outcome, lda_probs)
qda_roc <- roc(test.data$Outcome, qda_probs)

# Plotting the ROC curves
plot(lda_roc, col = "blue", main = "ROC Curve for LDA and QDA", lwd = 2)
plot(qda_roc, col = "red", add = TRUE, lwd = 2)
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2) 

#KNN taking k values 1 to 20
k_values <- 1:20 #initializing vector
accuracy_values <- c()
f1_values <- c()

#looping through each value, training KNN model and calculating accuracy and F1 score for each k
for (k in k_values) {
  knn_pred <- knn(train = X.train, test = X.test, cl = y.train, k = k)
  conf_matrix <- confusionMatrix(knn_pred, y.test)
  accuracy <- conf_matrix$overall["Accuracy"]
  accuracy_values <- c(accuracy_values, as.numeric(accuracy))
  precision <- as.numeric(conf_matrix$byClass["Precision"])
  recall <- as.numeric(conf_matrix$byClass["Recall"])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  f1_values <- c(f1_values, f1_score)
}

#Creating a data frame for plotting
results_df <- data.frame(K = k_values, Accuracy = accuracy_values, F1_Score = f1_values)

#Plot the accuracy and F1-score
ggplot(results_df, aes(x = K)) +
  geom_line(aes(y = Accuracy, color = "Accuracy"), size = 1) +
  geom_line(aes(y = F1_Score, color = "F1 Score"), size = 1) +
  labs(title = "KNN Performance: Accuracy & F1-score vs. K",
       x = "Number of Neighbors (K)",
       y = "Score") +
  scale_color_manual(name = "Metric", values = c("Accuracy" = "blue", "F1 Score" = "red")) +
  theme_minimal()








