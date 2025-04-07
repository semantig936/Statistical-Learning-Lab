#getting to the current working directory
setwd("C:/Study/Semester_6/Statistical_Learning_Lab")
getwd()

#reading and printing the data
data <- read.csv("manufacturing.csv")
head(data)

#varying the degree of polynomial on temperature
library(ggplot2)
models <- list()
degrees <- 1:5

for (d in degrees) {
  formula <- as.formula(paste("Quality.Rating ~ poly(Temperature...C.,", d, ", raw=TRUE)"))
  models[[d]] <- lm(formula, data = data)
}

library(boot)

#initialization
cv_results <- data.frame(Degree=integer(), LOOCV=numeric(), k5=numeric(), k10=numeric())

#performing LOOCV and k-fold CV (doing it for different degrees)
for (d in degrees) {
  formula <- as.formula(paste("Quality.Rating ~ poly(Temperature...C.,", d, ", raw=TRUE)"))
  model <- glm(formula, data = data)
  
  loocv_error <- cv.glm(data, model)$delta[1]
  k5_error <- cv.glm(data, model, K=5)$delta[1]
  k10_error <- cv.glm(data, model, K=10)$delta[1]
  
  cv_results <- rbind(cv_results, data.frame(Degree=d, LOOCV=loocv_error, k5=k5_error, k10=k10_error))
}

#Displaying the results in a table
install.packages("pander")
library(pander)
pander(cv_results) 

# plotting the results
cv_results_long <- reshape2::melt(cv_results, id="Degree")

ggplot(cv_results_long, aes(x=Degree, y=value, color=variable)) +
  geom_line() + geom_point() +
  labs(title="Cross-Validation MSE for Different Polynomial Degrees",
       x="Polynomial Degree", y="MSE", color="CV Method") +
  theme_minimal()

#trying out for linear combinations of different variables(excluding interactions)
library(reshape2)

# Define all variable combinations
var_combinations <- list(
  "Temp" = c("Temperature...C."),
  "Temp-Press" = c("Temperature...C.", "Pressure..kPa."),
  "Temp-MatFus" = c("Temperature...C.", "Material.Fusion.Metric"),
  "Temp-MatTrans" = c("Temperature...C.", "Material.Transformation.Metric"),
  "Temp-Press-MatFus" = c("Temperature...C.", "Pressure..kPa.", "Material.Fusion.Metric"),
  "Temp-Press-MatTrans" = c("Temperature...C.", "Pressure..kPa.", "Material.Transformation.Metric"),
  "Temp-MatFus-MatTrans" = c("Temperature...C.", "Material.Fusion.Metric", "Material.Transformation.Metric"),
  "Temp-Press-MatFus-MatTrans" = c("Temperature...C.", "Pressure..kPa.", "Material.Fusion.Metric", "Material.Transformation.Metric")
)

cv_results <- data.frame(Model=character(), LOOCV=numeric(), k5=numeric(), k10=numeric())

for (model_name in names(var_combinations)) {
  formula <- as.formula(paste("Quality.Rating ~", paste(var_combinations[[model_name]], collapse = " + ")))
  model <- glm(formula, data = data)
  
  loocv_error <- cv.glm(data, model)$delta[1]
  k5_error <- cv.glm(data, model, K=5)$delta[1]
  k10_error <- cv.glm(data, model, K=10)$delta[1]
  
  cv_results <- rbind(cv_results, data.frame(Model=model_name, LOOCV=loocv_error, k5=k5_error, k10=k10_error))
}

#printing the results
print(cv_results)
pander(cv_results)

cv_results_long <- melt(cv_results, id="Model")

ggplot(cv_results_long, aes(x=Model, y=value, color=variable)) +
  geom_line(aes(group=variable)) + 
  geom_point(size=3) +
  labs(title="Cross-Validation MSE for Different Predictor Combinations",
       x="Predictor Combinations", y="MSE", color="CV Method") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))  # Rotate labels for readability



#Bootstrapping

#generating the random variables that follow Gaussian distribution
set.seed(123) 
data <- rnorm(50, mean = 50, sd = sqrt(2))

bootstrap_means <- numeric(100)
bootstrap_vars <- numeric(100)

#random sampling with replacement
for (i in 1:100) {
  sample_data <- sample(data, size = 20, replace = TRUE) 
  bootstrap_means[i] <- mean(sample_data)  
  bootstrap_vars[i] <- var(sample_data) 
}

#estimating mean and variance from the 
boot_mean_estimate <- mean(bootstrap_means)
boot_var_estimate <- mean(bootstrap_vars)

cat("Estimated Population Mean from Bootstrap Samples:", boot_mean_estimate, "\n")
cat("Estimated Population Variance from Bootstrap Samples:", boot_var_estimate, "\n")


#plotting the bootstrapping means and variances (just for my own understanding)

p1 <- ggplot(data.frame(Means = bootstrap_means), aes(x = Means)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Bootstrap Sampling Distribution of the Mean",
       x = "Sample Mean", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(data.frame(Variances = bootstrap_vars), aes(x = Variances)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Bootstrap Sampling Distribution of the Variance",
       x = "Sample Variance", y = "Frequency") +
  theme_minimal()

print(p1)
print(p2)

