library(ISLR)
head(Credit)
data()
head(Caravan)
library(pls)
head(Hitters)
pcr.fit <- pcr(Salary ~., data = Hitters, ncomp = 5, scale = TRUE, validation = "CV", segments = 5)
dim(Hitters)
summary(pcr.fit)
pcr.fit$model
validationplot(pcr.fit, val.type = "MSEP")
?pcr
pcr.fit$coefficients
pcr.fit$residuals
plot(pcr.fit)
