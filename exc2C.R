# perform two-way ANOVA on the data

# assign the considered datavariables to an array 
weight_loss <- data$weight_loss
gender <- data$gender
diet <- data$diet

# boxplot among diet group and gender variable
boxplot(weight_loss ~ gender, data = data,
        col = c("lightblue","lightpink"),
        xlab = "Gender",
        ylab = "Weight Loss",
        main = "Weight Loss by Gender")

boxplot(weight_loss ~ diet, data = data,
        col = c("yellow","lightgreen","purple"),
        xlab = "Diet Group",
        ylab = "Weight Loss",
        main = "Weight Loss by Diet Group")

# interaction plots between variables 
interaction.plot(diet, gender, weight_loss,
                 col = c("lightblue","lightpink"),
                 xlab = "Gender",
                 ylab = "Weight Loss",
                 main = "Weight Loss by Gender")
interaction.plot(gender, diet, weight_loss,
                 col = c("red","green","purple"),
                 xlab = "Diet Group",
                 ylab = "Weight Loss",
                 main = "Weight Loss by Diet Group")

# perform the two-way ANOVA testing 
gender_factor = as.factor(gender)
diet_factor = as.factor(diet)

weight_loss_aov <- lm(weight_loss~diet_factor*gender_factor);
anova(weight_loss_aov)

aggregate(weight_loss ~ diet, data = data, mean)


