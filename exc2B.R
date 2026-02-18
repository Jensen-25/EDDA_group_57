# Step 1) Visualizations
# create boxplots for the 3 separate diet groups
source("exc2A.R")
boxplot(weight_loss ~ diet, data = data,
        col = c("lightblue","lightgreen","lightpink"),
        xlab = "Diet Group",
        ylab = "Weight Loss",
        main = "Weight Loss by Diet Group")

# Step 2) perform ANOVA-test
data$diet <- factor(data$diet)
model <- aov(weight_loss ~ diet, data = data)
summary(model)

# H0 is rejected, since p = 0.00323 < 0.05! So there is a significant 
# difference between the separate groups. 

# Step 3) one-sample t-test per diet
t.test(data$weight_loss[data$diet == 1], mu = 0, alternative = "greater")
t.test(data$weight_loss[data$diet == 2], mu = 0, alternative = "greater")
t.test(data$weight_loss[data$diet == 3], mu = 0, alternative = "greater")

# all t-test do imply that the mean is greater than 0
# Diet group 3 has the best result w.r.t. weight loss



# Can a t-test be related to the above ANOVA test? Can the 
#Kruskal-Wallis test be applied for this situation?

kruskal.test(weight_loss ~ diet, data = data)

