#Data cleaning/combining

BatterStats <- read.csv(file="Downloads/BatterStats(Game).csv", header=TRUE, sep=",")
BatterStats
class(BatterStats)
head(BatterStats)
print(head(BatterStats))
colnames(BatterStats)[colnames(BatterStats)=="X2B"] <- "2B"
colnames(BatterStats)[colnames(BatterStats)=="X3B"] <- "3B"
PitcherStats <- read.csv(file="Downloads/PitchingStats.csv", header=TRUE, sep=",")
head(PitcherStats)
Salaries <- read.csv(file="Downloads/MLBSalaries.csv", header=TRUE, sep=",")
head(Salaries)
colnames(Salaries) <- as.character(unlist(Salaries[1,]))
Salaries = Salaries[-1, ]
head(Salaries)
BatterStats = BatterStats[,-1]
head(BatterStats)
PitcherStats=PitcherStats[,-1]
head(PitcherStats)
Salaries = Salaries[,-1]
head(Salaries)
library(tidyverse)
library(reshape2)
SalariesSeparate <- separate(Salaries, col = "YEARS", into = c("TOTAL YEARS", "EXACT YEARS"), sep = 2)
head(SalariesSeparate)
SalariesSeparate2 <- separate(SalariesSeparate, col = "EXACT YEARS", into = c("FIRST YEAR", "LAST YEAR"), sep = 5)
head(SalariesSeparate2)
write.csv(BatterStats, file = "NewBatterStats.csv")
write.csv(PitcherStats, file = "NewPitcherStats.csv")
write.csv(SalariesSeparate2, file = "NewSalaries.csv")

options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)

# Init Ggplot
ggplot(BatterStats, aes(x=AVG, y=HR))  # area and poptotal are columns in 'midwest'

#basic scatterplot
ggplot(BatterStats, aes(x=AVG, y=HR)) + geom_point()


g <- ggplot(BatterStats, aes(x=AVG, y=HR)) + geom_point() + geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands

plot(g)
g1 <- g + coord_cartesian(xlim=c(.200,.350))  # zooms in
plot(g1)

g1 + labs(title="Batting Average vs. Homeruns", subtitle="From Batter Stats", y="Home Runs", x="Batting Average", caption="BA to HR data")     

ggplot(BatterStats, aes(x=AVG, y=HR)) + 
        geom_point(col="steelblue", size=3) +   # Set static color and size for points
        geom_smooth(method="lm", col="firebrick") +  # change the color of line
        coord_cartesian(xlim=c(.200,.350)) + 
        labs(title="Batting Average vs. Homeruns", subtitle="From Batter Stats", y="Home Runs", x="Batting Average", caption="BA to HR data")   

gg <- ggplot(BatterStats, aes(x=AVG, y=HR)) + # Set color to vary based on state categories.
        geom_point(aes(col=Pos), size=3) +
        geom_smooth(method="lm", col="firebrick", size=1) + 
        coord_cartesian(xlim=c(.200,.350)) + 
        scale_x_continuous(breaks=seq(.200, 0.350, 0.020), labels = sprintf("%.3f", seq(.200, 0.350, 0.020))) + 
        scale_y_continuous(breaks=seq(0, 60, 10)) +
        labs(title="Batting Average vs. Homeruns", subtitle="From Batter Stats", y="Home Runs", x="Batting Average", caption="BA to HR data")   
plot(gg)

df.facet_data <- read.csv("Downloads/NewBatterStats.csv")
df.facet_data$Team <- factor(df.facet_data$Team)

ggplot(data=df.facet_data, aes(x=df.facet_data$Team,y=HR, group=Pos)) +
        geom_line()

ggplot(data=df.facet_data, aes(x=Team,y=HR, group=1)) +
        geom_line() +
        facet_grid(Pos ~ .)


ggplot(data=df.facet_data, aes(x=Pos,y=HR)) +
        geom_bar(stat="identity") +
        facet_wrap(~Team) +
        ggtitle("Home Runs by Position by Team") +
        theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
        theme(axis.text.x = element_text(angle=90))

df.facet_data2 <- read.csv("Downloads/NewBatterStats.csv")
df.facet_data2$Team <- factor(df.facet_data2$Team)

ggplot(data=df.facet_data2, aes(x=df.facet_data2$Team,y=AVG, group=Pos)) +
        geom_line()

ggplot(data=df.facet_data2, aes(x=Team,y=AVG, group=1)) +
        geom_line() +
        facet_grid(Pos ~ .)

gg2 <- ggplot(data=df.facet_data2, aes(x=Pos,y=AVG)) +
        geom_bar(stat="identity") +
        facet_wrap(~Team) +
        ggtitle("Batting Average by Position by Team") +
        theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
        theme(axis.text.x = element_text(angle=90))

gg2 + coord_cartesian(ylim=c(.230, .350))+
        scale_y_continuous(breaks=seq(.230, 0.350, 0.020), labels = sprintf("%.3f", seq(.230, 0.350, 0.020)))

SalaryStats<-read.csv(file="Downloads/MLBSalaries.csv", header=TRUE, sep=",")
head(SalaryStats) 
colnames(SalaryStats) <- as.character(unlist(SalaryStats[1,]))
SalaryStats = SalaryStats[-1, ]
head(SalaryStats)
SalaryStats = SalaryStats[,-1]
head(SalaryStats)
library(tidyverse)
library(reshape2)
SalaryStats <- separate(SalaryStats, col = "YEARS", into = c("YEARS", "EXACT_YEARS"), sep = 2)
head(SalaryStats)  

#basic scatterplot
ggplot(PitcherStats, aes(x=AVG., y=ERA))


g <- ggplot(PitcherStats, aes(x=AVG., y=ERA)) + geom_point() + geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands

plot(g)
g1 <- g + labs(title="Avg Against Vs ERA", subtitle="From PitcherStats dataset", y="ERA", x="Avg. Against")
plot(g1)

ggplot(PitcherStats, aes(x=AVG., y=ERA)) + 
        geom_point(col="steelblue", size=3) +   # Set static color and size for points
        geom_smooth(method="lm", col="firebrick") +  # change the color of line
        labs(title="Avg Against Vs ERA", subtitle="From PitcherStats dataset", y="ERA", x="Avg. Against")

gg5<-ggplot(PitcherStats, aes(x=AVG., y=ERA)) + 
        geom_point(aes(col=W), size=3) +   # Set static color and size for points
        geom_smooth(method="lm", col="firebrick") +  # change the color of line
        labs(title="Avg Against Vs ERA", subtitle="From PitcherStats dataset", y="ERA", x="Avg. Against")
plot(gg5)

gg5 + scale_x_continuous(breaks=seq(.150, 0.350, 0.03), labels = sprintf("%.3f", seq(.150, 0.350, 0.03))) +
        scale_y_continuous(breaks=seq(2, 6, 0.5), labels = sprintf("%.2f", seq(2, 6, 0.5)))

gg5 + scale_colour_brewer(palette = "Set1")



options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)

g <- ggplot(PitcherStats, aes(x=AVG., y=ERA)) + geom_point() + geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands

plot(g)
g1 <- g + labs(title="Avg Against Vs ERA", subtitle="From PitcherStats dataset", y="ERA", x="Avg. Against")
plot(g1)

ggplot(PitcherStats, aes(x=AVG., y=ERA)) + 
        geom_point(col="steelblue", size=3) +   # Set static color and size for points
        geom_smooth(method="lm", col="firebrick") +  # change the color of line
        labs(title="Avg Against Vs ERA", subtitle="From PitcherStats dataset", y="ERA", x="Avg. Against")

gg5<-ggplot(PitcherStats, aes(x=AVG., y=ERA)) + 
        geom_point(aes(col=L), size=3) +   # Set static color and size for points
        geom_smooth(method="lm", col="firebrick") +  # change the color of line
        labs(title="Avg Against Vs ERA", subtitle="From PitcherStats dataset", y="ERA", x="Avg. Against")
plot(gg5)

gg5 + scale_x_continuous(breaks=seq(.150, 0.350, 0.03), labels = sprintf("%.3f", seq(.150, 0.350, 0.03))) +
        scale_y_continuous(breaks=seq(2, 6, 0.5), labels = sprintf("%.2f", seq(2, 6, 0.5)))

gg5 + scale_colour_brewer(palette = "Set1")





options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)

ggplot(BatterStats, aes(x=SO, y=HR)) + geom_point()


g <- ggplot(BatterStats, aes(x=SO, y=HR)) + geom_point() + geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands

plot(g)
g1 <- g + coord_cartesian(xlim=c(30,200))  # zooms in
plot(g1)

g1 + labs(title="Strikeouts vs. Homeruns", subtitle="From Batter Stats", y="Home Runs", x="Strikeouts", caption="SO to HR data")     

ggplot(BatterStats, aes(x=SO, y=HR)) + 
        geom_point(col="steelblue", size=3) +   # Set static color and size for points
        geom_smooth(method="lm", col="firebrick") +  # change the color of line
        coord_cartesian(xlim=c(30,200)) + 
        labs(title="Batting Average vs. Homeruns", subtitle="From Batter Stats", y="Home Runs", x="Batting Average", caption="BA to HR data")   

gg <- ggplot(BatterStats, aes(x=SO, y=HR)) + # Set color to vary based on state categories.
        geom_point(aes(col=Pos), size=3) +
        geom_smooth(method="lm", col="firebrick", size=1) + 
        coord_cartesian(xlim=c(20,200)) + 
        scale_x_continuous(breaks=seq(20, 200,20 )) + 
        scale_y_continuous(breaks=seq(0, 60, 10)) +
        labs(title="Strikeouts vs. Homeruns", subtitle="From Batter Stats", y="Home Runs", x="Strikeouts", caption="SO to HR data")   
plot(gg)


write.csv(BatterStats, file = "NewBatterStats.csv")
write.csv(PitcherStats, file = "NewPitcherStats.csv")
write.csv(SalariesSeparate2, file = "NewSalaries.csv")
View(BatterStats)
View(PitcherStats)
View(SalariesSeparate2)

colnames(SalariesSeparate2)[colnames(SalariesSeparate2)=="NAME"] <- "Player"
BatterStats[c("SALARY","TOTAL YEARS", "FIRST YEAR","LAST YEAR","TOTAL VALUE","AVG ANNUAL")] <- NA
View(BatterStats)
BatterStats$SALARY <- SalariesSeparate2$SALARY

BatterStats[nrow(BatterStats) +61,] <- NA #Create empty rows to add pitchers
View(BatterStats)
drop <- c("SALARY","TOTAL YEARS", "FIRST YEAR", "LAST YEAR", "TOTAL VALUE", "AVG ANNUAL")
BatterStats <- BatterStats[,!(names(BatterStats) %in% drop)] #Temporarily drop Salary variables
View(BatterStats)
BatterStats[c("W","L", "ERA","G","GS","SV","SVO","IP","HAL","RAL","ER","HRA","BBA","SOA","AVG.","WHIP")] <- NA #Add pitcher variable
View(BatterStats)
BatterStats[c("SALARY","TOTAL YEARS", "FIRST YEAR","LAST YEAR","TOTAL VALUE","AVG ANNUAL")] <- NA #Add Salary variables
View(BatterStats)

CombinedStats <- BatterStats
View(CombinedStats)

write.csv(CombinedStats, file = "CombinedStatsNA.csv")

myvars <- c("Player","Team")
newdata <- PitcherStats[myvars]
head(newdata)
CombinedStats[249:309,1:2] <- newdata
View(CombinedStats)

CombinedStats[249:250,1] <- PitcherStats[1:2,1]
View(CombinedStats)

CombinedStats[249:309,20:35] = PitcherStats[1:61,3:18] #transfer pitcher data to respective cells
View(CombinedStats)

myvars2 <- c("Player")
newdata2 <- PitcherStats[myvars2]
CombinedStats[249:309,1] <- newdata2
View(CombinedStats)

colnames(PitcherStats)[colnames(PitcherStats)=="Player"] <- "Pitcher"
CombinedStats[249:309,1] <- PitcherStats[1:61,1]
View(CombinedStats)

AllStats <-data.frame(Player=NA,Team=NA,Position=NA) #Create empty data frame
View(AllStats)

AllStats[nrow(AllStats) +308,] <- NA #Create empty rows to add combined data
View(AllStats)

AllStats[c("G.","AB","R","H","2B","3B","HR","RBI","BB","")]

plot(HR ~ AVG, data = BatterStats, col = "grey", pch = 20,
     main = "Home Runs vs Batting Average") #Plot HR to AVG

fit_1 = lm(HR ~ AVG, data = BatterStats)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)
library(lmtest)
bptest(fit_1)
cor(AVG,HR)

qqnorm(resid(fit_1), main = "Normal Q-Q Plot, Home Runs to Batting Average", col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd = 2)

plot(RBI ~ HR, data = BatterStats, col = "grey", pch = 20,
     main = "Runs Batted In vs Home Runs") #Plot RBI to HR
fit_2=lm(RBI ~ HR, data = BatterStats)
abline(fit_2, col = "darkorange", lwd = 3)

summary(fit_2)
library(lmtest)
bptest(fit_2)

plot(SO ~ HR, data = BatterStats, col = "grey", pch = 20,
     main = "Strikeouts vs Home Runs") #Plot SO to HR
fit_3=lm(SO ~ HR, data = BatterStats)
abline(fit_3, col = "darkorange", lwd = 3)

summary(fit_3)
library(lmtest)
bptest(fit_3)

plot(OBP ~ AVG, data = BatterStats, col = "grey", pch = 20,
     main = "On Base Percentage vs Batting Average") #Plot OBP to AVG
fit_4=lm(OBP ~ AVG, data = BatterStats)
abline(fit_4, col = "darkorange", lwd = 3)

summary(fit_4)
library(lmtest)
bptest(fit_4)

plot(R ~ OBP, data = BatterStats, col = "grey", pch = 20,
     main = "Runs vs OBP") #Plot Runs to OBP
fit_5=lm(R ~ OBP, data = BatterStats)
abline(fit_5, col = "darkorange", lwd = 3)

summary(fit_5)
library(lmtest)
bptest(fit_5)

qqnorm(resid(fit_5), main = "Normal Q-Q Plot, Runs to OBP", col = "darkgrey")
qqline(resid(fit_5), col = "dodgerblue", lwd = 2)

plot(W ~ ERA, data = PitcherStats, col = "grey", pch = 20,
     main = "Wins vs Earned Run Average") #Plot Wins to ERA
fit_6=lm(W ~ ERA, data = PitcherStats)
abline(fit_6, col = "darkorange", lwd = 3)

summary(fit_6)
library(lmtest)
bptest(fit_6)

qqnorm(resid(fit_6), main = "Normal Q-Q Plot, Wins to ERA", col = "darkgrey")
qqline(resid(fit_6), col = "dodgerblue", lwd = 2)

plot(L ~ ERA, data = PitcherStats, col = "grey", pch = 20,
     main = "Losses vs Earned Run Average") #Plot Losses to ERA
fit_7=lm(L ~ ERA, data = PitcherStats)
abline(fit_7, col = "darkorange", lwd = 3)

summary(fit_7)
library(lmtest)
bptest(fit_7)

plot(IP ~ HR, data = PitcherStats, col = "grey", pch = 20,
     main = "Innings Pitched vs Home Runs Allowed") #Plot IP to HR
fit_8=lm(IP ~ HR, data = PitcherStats)
abline(fit_8, col = "darkorange", lwd = 3)

summary(fit_8)
library(lmtest)
bptest(fit_8)

plot(WHIP ~ AVG., data = PitcherStats, col = "grey", pch = 20,
     main = "Walks & Hits per Innings Pitched vs Batting Average Against") #Plot WHIP to AVG AG
fit_9=lm(WHIP ~ AVG., data = PitcherStats)
abline(fit_9, col = "darkorange", lwd = 3)

summary(fit_9)
library(lmtest)
bptest(fit_9)

plot(IP ~ W, data = PitcherStats, col = "grey", pch = 20,
     main = "Innings Pitched vs Wins") #Plot IP to W
fit_10=lm(IP ~ W, data = PitcherStats)
abline(fit_10, col = "darkorange", lwd = 3)

summary(fit_10)
library(lmtest)
bptest(fit_10)

qqnorm(resid(fit_10), main = "Normal Q-Q Plot, Innings Pitched to Wins", col = "darkgrey")
qqline(resid(fit_10), col = "dodgerblue", lwd = 2)

colnames(SalariesSeparate2)[colnames(SalariesSeparate2)=="TOTAL YEARS"] <- "TY"
colnames(SalariesSeparate2)[colnames(SalariesSeparate2)=="FIRST YEAR"] <- "FY"
colnames(SalariesSeparate2)[colnames(SalariesSeparate2)=="LAST YEAR"] <- "LY"
colnames(SalariesSeparate2)[colnames(SalariesSeparate2)=="TOTAL VALUE"] <- "TV"
colnames(SalariesSeparate2)[colnames(SalariesSeparate2)=="AVG ANNUAL"] <- "AVGANNUAL"

plot(SALARY ~ TY, data = SalariesSeparate2, col = "grey", pch = 20,
     main = "Total Value vs Total Years") #Plot Total Value to Total Years
fit_11=lm(SALARY ~ TY, data = PitcherStats)
abline(fit_11, col = "darkorange", lwd = 3)

summary(fit_11)
library(lmtest)
bptest(fit_11)

typeof(BatterStats$`TOTAL VALUE`)
typeof(SalariesSeparate2$SALARY)

CombinedStats[1,36] = SalariesSeparate2[102,4]
View(CombinedStats)
typeof(CombinedStats$SALARY)

PitcherStatsSalaries <- read.csv(file="Downloads/PitcherStats_Salaries.csv", header=TRUE, sep=",")
View(PitcherStatsSalaries) #Import pitcher stats combined with pitcher salaries

plot(SALARY ~ ERA, data = PitcherStatsSalaries, col = "grey", pch = 20,
     main = "Salary vs ERA") #Plot Salary to ERA
fit_12=lm(SALARY ~ ERA, data = PitcherStatsSalaries)
abline(fit_12, col = "darkorange", lwd = 3)

summary(fit_12)
library(lmtest)
bptest(fit_12)

qqnorm(resid(fit_12), main = "Normal Q-Q Plot, Salary to ERA", col = "darkgrey")
qqline(resid(fit_12), col = "dodgerblue", lwd = 2)



plot(SALARY ~ SO, data = PitcherStatsSalaries, col = "grey", pch = 20,
     main = "Salary vs Strikeouts") #Plot Salary to Strikeouts
fit_14=lm(SALARY ~ SO, data = PitcherStatsSalaries)
abline(fit_14, col = "darkorange", lwd = 3)

summary(fit_14)
library(lmtest)
bptest(fit_14)

qqnorm(resid(fit_12), main = "Normal Q-Q Plot, Salary to ERA", col = "darkgrey")
qqline(resid(fit_12), col = "dodgerblue", lwd = 2)

plot(IP ~ NUMYEARS, data = PitcherStatsSalaries, col = "grey", pch = 20,
     main = "Innings Pitched vs # of Years Under Contract") #Plot Innings Pitched to Years Under Contract
fit_15=lm(IP ~ NUMYEARS, data = PitcherStatsSalaries)
abline(fit_15, col = "darkorange", lwd = 3)

summary(fit_15)
library(lmtest)
bptest(fit_15)

BatterStatsSalaries <- read.csv(file="Downloads/BatterStats_Salaries.csv", header=TRUE, sep=",")
View(BatterStatsSalaries) #Import pitcher stats combined with pitcher salaries

colnames(BatterStatsSalaries)[colnames(BatterStatsSalaries)=="X2B"] <- "2B"
colnames(BatterStatsSalaries)[colnames(BatterStatsSalaries)=="X3B"] <- "3B" #Rename doubles and triples columns
View(BatterStatsSalaries)

BatterStatsSalaries <- subset(BatterStatsSalaries, select = -c(RK) ) #drop Rank variable
View(BatterStatsSalaries)

plot(SALARY ~ AVG, data = BatterStatsSalaries, col = "grey", pch = 20,
     main = "Salary vs Batting Average") #Plot Salary to Batting Average
fit_16=lm(SALARY ~ AVG, data = BatterStatsSalaries)
abline(fit_16, col = "darkorange", lwd = 3)

summary(fit_16)
library(lmtest)
bptest(fit_16)

qqnorm(resid(fit_16), main = "Normal Q-Q Plot, Salary to Batting Average", col = "darkgrey")
qqline(resid(fit_16), col = "dodgerblue", lwd = 2)

plot(SALARY ~ OBP, data = BatterStatsSalaries, col = "grey", pch = 20,
     main = "Salary vs On Base Percentage") #Plot Salary to OBP
fit_17=lm(SALARY ~ OBP, data = BatterStatsSalaries)
abline(fit_17, col = "darkorange", lwd = 3)

summary(fit_17)
library(lmtest)
bptest(fit_17)

plot(SALARY ~ H, data = BatterStatsSalaries, col = "grey", pch = 20,
     main = "Salary vs Hits") #Plot Salary to Hits
fit_18=lm(SALARY ~ H, data = BatterStatsSalaries)
abline(fit_18, col = "darkorange", lwd = 3)

summary(fit_18)
library(lmtest)
bptest(fit_18)

plot(SALARY ~ HR, data = BatterStatsSalaries, col = "grey", pch = 20,
     main = "Salary vs Home Runs") #Plot Salary to Home Runs
fit_19=lm(SALARY ~ HR, data = BatterStatsSalaries)
abline(fit_19, col = "darkorange", lwd = 3)

summary(fit_19)
library(lmtest)
bptest(fit_19)

qqnorm(resid(fit_19), main = "Normal Q-Q Plot, Salary to Home Runs", col = "darkgrey")
qqline(resid(fit_19), col = "dodgerblue", lwd = 2)

plot(NUMYEARS ~ AVG, data = BatterStatsSalaries, col = "grey", pch = 20,
     main = "Number of Years vs Batting Average") #Plot Number of Years to Batting Avg
fit_20=lm(NUMYEARS ~ AVG, data = BatterStatsSalaries)
abline(fit_20, col = "darkorange", lwd = 3)

summary(fit_20)
library(lmtest)
bptest(fit_20)

plot(NUMYEARS ~ OBP, data = BatterStatsSalaries, col = "grey", pch = 20,
     main = "Number of Years vs On Base Percentage") #Plot Number of Years to OBP
fit_21=lm(NUMYEARS ~ OBP, data = BatterStatsSalaries)
abline(fit_21, col = "darkorange", lwd = 3)

summary(fit_21)
library(lmtest)
bptest(fit_21)

plot(NUMYEARS ~ H, data = BatterStatsSalaries, col = "grey", pch = 20,
     main = "Number of Years vs Hits") #Plot Number of Years to Hits
fit_22=lm(NUMYEARS ~ H, data = BatterStatsSalaries)
abline(fit_22, col = "darkorange", lwd = 3)

summary(fit_22)
library(lmtest)
bptest(fit_22)

plot(NUMYEARS ~ HR, data = BatterStatsSalaries, col = "grey", pch = 20,
     main = "Number of Years vs Home Runs") #Plot Number of Years to Home Runs
fit_23=lm(NUMYEARS ~ HR, data = BatterStatsSalaries)
abline(fit_23, col = "darkorange", lwd = 3)

summary(fit_23)
library(lmtest)
bptest(fit_23)

plot(NUMYEARS ~ SO, data = PitcherStatsSalaries, col = "grey", pch = 20, #Back to PitcherStatsSalaries
     main = "Number of Years vs Strikeouts") #Plot Number of Years to Strikeouts
fit_24=lm(NUMYEARS ~ SO, data = PitcherStatsSalaries)
abline(fit_24, col = "darkorange", lwd = 3)

summary(fit_24)
library(lmtest)
bptest(fit_24)

plot(NUMYEARS ~ ERA, data = PitcherStatsSalaries, col = "grey", pch = 20, #Back to PitcherStatsSalaries
     main = "Number of Years vs ERA") #Plot Number of Years to ERA
fit_25=lm(NUMYEARS ~ ERA, data = PitcherStatsSalaries)
abline(fit_25, col = "darkorange", lwd = 3)

summary(fit_25)
library(lmtest)
bptest(fit_25)

plot(NUMYEARS ~ W, data = PitcherStatsSalaries, col = "grey", pch = 20, #Back to PitcherStatsSalaries
     main = "Number of Years vs Wins") #Plot Number of Years to Wins
fit_26=lm(NUMYEARS ~ W, data = PitcherStatsSalaries)
abline(fit_26, col = "darkorange", lwd = 3)

summary(fit_26)
library(lmtest)
bptest(fit_26)

plot(SALARY ~ IP, data = PitcherStatsSalaries, col = "grey", pch = 20, #Back to PitcherStatsSalaries
     main = "Salary vs Innings Pitched") #Plot Salary to Innings Pitched
fit_27=lm(SALARY ~ IP, data = PitcherStatsSalaries)
abline(fit_27, col = "darkorange", lwd = 3)

summary(fit_27)
library(lmtest)
bptest(fit_27)

#Visualizations
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
ggplot(data=BatterStatsSalaries, aes(x=Pos,y=R)) +
        geom_bar(stat="identity") +
        facet_wrap(~Team) +
        ggtitle("Runs by Team") +
        theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
        theme(axis.text.x = element_text(angle=90))

ggplot(data=BatterStatsSalaries, aes(x=Team,y=SALARY)) +
        geom_bar(stat="identity") +
        ggtitle("Team Payroll") +
        theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
        theme(axis.text.x = element_text(angle=90))
