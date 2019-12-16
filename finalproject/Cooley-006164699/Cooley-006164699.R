install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(ggalt)

raw_df <- read.csv("./Cooley-006164699.csv")

# remove unneccesary rows (data with more than 4 NA's)
df <- raw_df[rowSums(is.na(raw_df)) < 3,]

# remove unneccesary columns (data that doesn't give us insights into the story our data is telling)
df <- subset(df, select = -c(Points, SG.OTT, SG.APR, SG.ARG, Average.SG.Putts, Average.SG.Total))

# rearrange columns for better representation of data
df <- df[, c("Player.Name", "Year", "Top.10", "Wins", "Rounds", "Money", "Fairway.Percentage", 
             "Avg.Distance", "gir", "Average.Putts", "Average.Scrambling", "Average.Score")]

# rename colums for a more cohesive look of headers
df <- df %>% 
  rename(
    Greens.In.Reg = gir,
    Avg.Putts = Average.Putts,
    Avg.Scrambling = Average.Scrambling,
    Avg.Score = Average.Score
  )

# replace all NA values in the Wins and Top.10 columns with 0's
df$Wins[is.na(df$Wins)] <- 0
df$Top.10[is.na(df$Top.10)] <- 0

# remove rows with no money made
df <- df[-c(42, 714, 902, 1288),]


## VISUALIZATION #1 - Greens In Regulation vs. Average Score ##
# linear regression + summary
fit.girVScore <- lm(Avg.Score ~ Greens.In.Reg, data=df)
summary(fit.girVScore)

# plot
ggplot(data=df, aes(x=Greens.In.Reg, y=Avg.Score)) +
  geom_point(alpha = 0.85, color = "darkgreen") +
  geom_smooth(method=lm, color = "black") +
  labs(title="Greens In Regulation vs. Average Score", 
       x="Greens In Regulation",
       y="Average Score",
       caption = "Source: PGA Tour Dataset")


## VISUALIZATION #2 - Average Putts vs. Average Score ##
# linear regression + summary
fit.puttsVScore <- lm(Avg.Score ~ Avg.Putts, data=df)
summary(fit.puttsVScore)

# plot
ggplot(data=df, aes(x=Avg.Putts, y=Avg.Score)) +
  geom_point(alpha = 0.85, color = "#56b4e9") +
  geom_smooth(method=lm, color = "#e69f03", fill="black") +
  labs(title="Average Putts vs Average Score", 
       x="Average Putts", 
       y="Average Score",
       caption = "Source: PGA Tour Dataset")


# Order data by top 10 finishes decending
df.compData <- df[order(-df$Top.10),]

# Grab top 5 data points
df.compData <- df.compData %>% top_n(5, Top.10)

# took top golfers after looking at df.compData because I couldn't figure out how to automatically do it.
golferNames <- c("Jordan Spieth", "Dustin Johnson", "Matt Kuchar", "Luke Donald")
selectGolfers <- df[df$Player.Name %in% golferNames,]

# Order golfers by name
selectGolfers <- selectGolfers[order(selectGolfers$Player.Name),]

# this shows that greens in reg is not always the best predictor, However, it is a good predictor for Matt Kuchar.
ggplot(selectGolfers, aes(x=Greens.In.Reg, y=Avg.Score)) +
  geom_point(aes(x=Greens.In.Reg, y=Avg.Score, color = factor(Year)),
             data = selectGolfers,
             size = 3) +
  geom_text(aes(label=Year),hjust=0, vjust=0) +
  geom_smooth(method=lm, color="black") +
  scale_color_discrete(name = "Year") +
  labs(title="Greens In Regulation vs Average Score", 
       x="Greens In Regulation", 
       y="Average Score",
       caption = "Source: PGA Tour Dataset") +
  facet_grid(Player.Name ~ .)

#linear regression of gir to predict score among the top golfers
fit.score.gir.t5 <- lm(Avg.Score ~ Greens.In.Reg, data=df.compData)
summary(fit.score.gir.t5)

#multiple regression of gir and putts to predict score among the top golfers
fit.score.girPutts.t5 <- lm(Avg.Score ~ Greens.In.Reg + Avg.Putts, data=df.compData)
summary(fit.score.girPutts.t5)

# this shows how elite these golfers are among the rest.
ggplot(df, aes(x=Greens.In.Reg, y=Avg.Score)) +
  geom_point(alpha = 0.85, color = "blue") +
  geom_smooth(method=lm, color= "magenta", fill="black") +
  geom_point(aes(x=Greens.In.Reg, y=Avg.Score, color = factor(Player.Name)),
             data = df.compData,
             size = 3) +
  scale_color_discrete(name = "Player Name") +
  labs(title="Greens In Regulation vs. Average Score", 
       x="Greens In Regulation", 
       y="Average Score",
       caption = "**Dustin Johnson has two data points as he had 2 years in the top 5 of the dataset.\nSource: PGA Tour Dataset")

#multiple regression of gir and putts to predict score
fit.score.girPutts.total <- lm(Avg.Score ~ Greens.In.Reg + Avg.Putts, data=df)
summary(fit.score.girPutts.total)  

## Original Visualization
ggplot(df, aes(x=Avg.Score, y=Fairway.Percentage)) + 
  geom_point(aes(col=Wins,size=Wins)) + 
  scale_color_gradient(low="blue", high="orange") +
  facet_wrap(~ Year) +
  labs(subtitle="Fairway % vs Avg Score", 
       y="Fairway %", 
       x="Average Score", 
       title="Time Analysis", 
       caption = "Source: PGA Tour Dataset")

