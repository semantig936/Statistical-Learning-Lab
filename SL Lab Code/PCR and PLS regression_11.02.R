#Principal Component Regression
library(ISLR)
library(dplyr)
library(tidyr)
library(pls)  #in this pcr() function present
Hitters = na.omit(Hitters) # Omit empty rows

res <- cor(Hitters, 
                method = "pearson") #(will not give results due to presence of non numeric columns)

# Select only numeric columns
numeric_hitters <- Hitters %>% select_if(is.numeric)

# Compute correlation matrix 
res <- cor(numeric_hitters, method = "pearson")
res

# Plot correlation matrix
# Basic Heatmap
heatmap(res, 
        col = colorRampPalette(c("blue", "white", "red"))(20), 
        scale = "none", 
        margins = c(5, 5), 
        main = "Correlation Heatmap")


set.seed(2)
pcr_fit = pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
#Scale is used to standardize each predictor prior to generating the principal components
#validation="CV" causes pcr() to compute the ten-fold cross-validation error
summary(pcr_fit)
#the pcr() CV value gives root mean squared error to get MSE we have to square this quantity.
#plot cross-validation score. The MSEP plot the MSE value
validationplot(pcr_fit, val.type = "MSEP")
# % variance explained the amount of information about the predictors or the response that is captured using  M principal components

set.seed(1)


train = Hitters %>%
  sample_frac(0.5)

test = Hitters %>%
  setdiff(train)

pcr_fit2 = pcr(Salary~., data = train, scale = TRUE, validation = "CV")
validationplot(pcr_fit2, val.type = "MSEP")

#test MSE
x_train = model.matrix(Salary~., train)[,-1]
x_test = model.matrix(Salary~., test)[,-1]

y_train = train %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

y_test = test %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

pcr_pred = predict(pcr_fit2, x_test, ncomp=7)
mean((pcr_pred-y_test)^2)



#Partial Least Square
set.seed(1)
pls_fit = plsr(Salary~., data = train, scale = TRUE, validation = "CV")

summary(pls_fit)
validationplot(pls_fit, val.type = "MSEP")
pls_pred = predict(pls_fit, x_test, ncomp = 2)
mean((pls_pred - y_test)^2)
pls_fit2 = plsr(Salary~., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls_fit2)

