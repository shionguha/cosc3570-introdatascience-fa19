#Connor Diggins
#COSC 3570 Project Code

#library
library(ggplot2)

#import dataset
insurance <- read.csv("Diggins-006133167.csv", header = TRUE)

#cleaning data
insurance$children <- as.character(insurance$children)
insurance$children[insurance$children == '0'] <- "no"
insurance$children[insurance$children == '1'] <- "yes"
insurance$children[insurance$children == '2'] <- "yes"
insurance$children[insurance$children == '3'] <- "yes"
insurance$children[insurance$children == '4'] <- "yes"
insurance$children[insurance$children == '5'] <- "yes"

#statistical output
summary(insurance)
summary(lm(charges ~ bmi + smoker + children + age + sex + region, data = insurance))

#first graph
ggplot(insurance, aes(x=bmi, y=charges, col=smoker)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + scale_color_manual(breaks = c("no","yes"), values=c("orange", "purple")) + xlab("BMI") + ylab("Insurance Charges") + ggtitle("Insurance Charges vs BMI", subtitle = "Based on Smoking")

#second graph
ggplot(insurance, aes(x=bmi, y=charges, col=sex)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_manual(breaks = c("male", "female"), values=c("pink", "blue")) + facet_grid(region ~ .) + xlab("BMI") + ylab("Insurance Charges") + ggtitle("Insurance Charges vs BMI", subtitle = "Based on Region")

#third graph
insurance$bmi <- factor(insurance$bmi)
ggplot(data=insurance, aes(x=bmi,y=charges, group=1)) +
  geom_point() +
  facet_grid(sex ~ .)