rm(list = ls())
options(digits = 3)

# Load data
data <- read.table("diet.txt", header = TRUE)

# Create response variable
data$weight_loss <- data$preweight - data$weight6weeks

# Convert variables
data$diet   <- as.factor(data$diet)
data$gender <- as.factor(data$gender)
data$height <- as.numeric(data$height)
data$age    <- as.numeric(data$age)

# Remove missing values (important because 2 gender values are NA)
data2 <- na.omit(data)

# Fit final model from Exercise 2D
model <- lm(weight_loss ~ diet * gender + height + age, data = data2)

summary(model)

# Compute average values for numeric covariates
mean_age <- mean(data2$age)
mean_height <- mean(data2$height)

# Create prediction grid (all diet × gender combinations)
newdata <- expand.grid(
  diet = levels(data2$diet),
  gender = levels(data2$gender),
  age = mean_age,
  height = mean_height
)

# Predict mean weight loss (with 95% CI)
pred <- predict(model, newdata = newdata, interval = "confidence")

results <- cbind(newdata, pred)

# Sort by highest predicted loss
results <- results[order(-results$fit), ]

results

