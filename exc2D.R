
# these two plots do indicate that both height and age do not influence 
# weight_loss.
data$gender <- as.factor(data$gender)
data$height <- as.numeric(as.character(data$height))
data$age <- as.numeric(as.character(data$age))

plot(data$age, data$weight_loss,
     col = data$diet,
     pch = 19,
     xlab = "Age",
     ylab = "Weight loss")
legend("topright", legend = levels(data$diet),
       col = 1:length(levels(data$diet)),
       pch = 19)

plot(data$height, data$weight_loss,
     col = data$diet,
     pch = 19,
     xlab = "Height",
     ylab = "Weight loss")
legend("topright", legend = levels(data$diet),
       col = 1:length(levels(data$diet)),
       pch = 19)

plot(data$age, data$weight_loss,
     col = data$gender,
     pch = 19,
     xlab = "Age",
     ylab = "Weight loss")
legend("topright", legend = levels(data$gender),
       col = 1:length(levels(data$gender)),
       pch = 19)

plot(data$height, data$weight_loss,
     col = data$gender,
     pch = 19,
     xlab = "Height",
     ylab = "Weight loss")
legend("topright", legend = levels(data$gender),
       col = 1:length(levels(data$gender)),
       pch = 19)
model1 <- lm(weight_loss ~ diet + gender + height + age, data = data)
anova(model1)
model_int <- lm(weight_loss ~ diet * gender + height + age, data = data)
anova(model_int)
anova(model1, model_int)

