
# read the txt file as table and make the output in the right format

data <- read.table('diet.txt')
colnames(data) <- as.character(unlist(data[1, ]))
data <- data[-c(1), ]

# compute weight loss for every entry
data$weight_loss <- as.numeric(data$preweight) - as.numeric(data$weight6weeks)

weight_loss = data$weight_loss
weight6weeks = as.numeric(data$weight6weeks)
preweight = as.numeric(data$preweight)
head(data)

# exc 2a)
# Step 1) assumptions:
print("The experiments are paired due to the experimental design
      and every weight measre is independent of each other!")

# Asses for normality of the residual variable weight.loss
normality_diet_data <- qqnorm(weight_loss,
       main = 'Normal Q-Qplot weight loss')
qqline(weight_loss, col = "red")

# Check for outliers
boxplot(weight_loss, 
        main = "Boxplot of Weight Loss", 
        ylab = "Weight lost (kg)",
        col = "lightblue")
boxplot.stats(weight_loss)$out # no outliers 

# Step 2) Visualizations

# datapoints under the red line do imply weight loss and datapoints above the line not.
plot(preweight, weight6weeks,
     xlab = "Preweight",
     ylab = "Weight after 6 weeks",
     main = "Preweight vs Weight after 6 weeks",
     col = 'purple')

abline(0,1, col="red")

# Step 3) Paired t-test 
result = t.test(preweight, weight6weeks, paired = TRUE, alternative = "two.sided")
result

 # print(result[["p.value"]]) <- 1.172236e-21, so the null hypothesis 
# is rejected and thus there is a significant difference in weight when taking a diet.


