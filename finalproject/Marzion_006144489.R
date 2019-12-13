library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(lmtest)

pbp <- read_csv("Marzion_006144489.csv")

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  )

pbp_air <- pbp_rp %>% filter(!is_na(air_yards)) %>% mutate(differential = air_yards - ydstogo)

plot(epa ~ differential, xlab= "Yards thrown behind or ahead of first down", ylab = "EPA", data = pbp_air, col = "black", pch = 20,
     main = "EPA based on how far throw is compared to first down")
fit_1 = lm(epa ~ differential, data = pbp_air)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)
bptest(fit_1)

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

firstdownpass <- pbp_rp %>%
  filter(!is.na(air_yards) & down == 1) %>%
  group_by(posteam) %>%
  summarize(airydsfirst = mean(air_yards), successfirstpass = mean(success)) %>%
  arrange(airydsfirst)

seconddownpass <- pbp_rp %>%
  filter(!is.na(air_yards) & down == 2) %>%
  group_by(posteam) %>%
  summarize(airydssecond = mean(air_yards), successsecondpass = mean(success)) %>%
  arrange(airydssecond)

thirddownpass <- pbp_rp %>%
  filter(!is.na(air_yards) & down == 3) %>%
  group_by(posteam) %>%
  summarize(airydsthird = mean(air_yards), successthirdpass = mean(success)) %>%
  arrange(airydsthird)

thirddistance <- pbp_rp %>%
  filter(down == 3) %>%
  group_by(posteam) %>%
  summarize(ydstogothird = mean(ydstogo)) %>%
  arrange(ydstogothird)

teamstats <- merge(firstdownpass, thirddownpass)

teamstats <- merge(teamstats, seconddownpass)

teamstats <- merge(teamstats, thirddistance)

plot(ydstogothird ~ airydsfirst, xlab= "Average distance of first down passes", ylab = "Average yards to go on third down", data = teamstats, col = "black", pch = 20,
     main = "How first down aggressiveness correlates to third down easiness")
fit_1 = lm(ydstogothird ~ airydsfirst, data = teamstats)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)
bptest(fit_1)

earlydownair <- pbp_rp %>%
  filter(!is.na(air_yards), down == 1 | down == 2) %>%
  group_by(posteam) %>%
  summarize(airydsfirstsecond = mean(air_yards)) %>%
  arrange(airydsfirstsecond)

teamstats <- merge(teamstats, earlydownair)

plot(ydstogothird ~ airydsfirstsecond, xlab="Average distance of first and second down passes", ylab="Average yards to go on third down", data = teamstats, col = "black", pch = 20,
     main = "How first and second down aggressiveness correlates to third down easiness")
fit_1 = lm(ydstogothird ~ airydsfirstsecond, data = teamstats)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)
bptest(fit_1)

differential <- pbp_rp %>%
  filter(!is.na(air_yards)) %>%
  group_by(posteam) %>%
  summarize(avgdifferential = mean(air_yards - ydstogo), success = mean(success), avgairyards = mean(air_yards), avgtogo = mean(ydstogo)) %>%
  arrange(avgdifferential)

plot(success ~ avgdifferential, xlab="Yards thrown beyond or behind first down", ylab="Success Rate", data = differential, col = "black", pch = 20,
     main = "Throwing Past First Down vs Success Rate")
fit_1 = lm(success ~ avgdifferential, data = differential)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)
bptest(fit_1)

differential %>%
  ggplot(aes(x = avgtogo, y = avgairyards)) +
  geom_hline(yintercept = mean(differential$avgairyards), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(differential$avgtogo), color = "red", linetype = "dashed") +
  geom_point(aes(col = differential$success), size = 2) +
  labs(x = "Average Yards to First Down",
           y = "Average Throw Distance",
           title = "Success Rate based on where throw is compared to first down", color = "success rate") +
  theme_bw()

differential %>%
  ggplot(aes(x = avgairyards, y = success)) +
  geom_hline(yintercept = mean(differential$success), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(differential$avgairyards), color = "red", linetype = "dashed") +
  geom_point(col = "black", size = 2) +  geom_smooth(method="lm", col="firebrick", size=2) +
  labs(x = "Average Throw Distance",
       y = "Success Rate",
       title = "Success Rate based on average throw distance") +
  theme_bw()

plot(success ~ avgairyards, xlab="Average Throw Distance", ylab="Success Rate", data = differential, col = "black", pch = 20,
     main = "Throw Distance vs Success Rate")
fit_1 = lm(success ~ avgairyards, data = differential)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)

earlydownpass <- pbp_rp %>%
  filter(wp>.20 & wp<.80 & down<=2 & half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(earlydownpassrate = mean(pass)) %>%
  arrange(earlydownpassrate)

firsthalfpass <- pbp_rp %>%
  filter(wp>.20 & wp<.80 & qtr<=2 & half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(firsthalfpassrate = mean(pass)) %>%
  arrange(firsthalfpassrate)

secondhalf <- pbp_rp %>%
  filter(wp>.20 & wp<.80 & qtr>=3 & half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(secondhalfpassrate = mean(pass), secondhalfsuccess = mean(success)) %>%
  arrange(secondhalfsuccess)

thirddownoverall <- pbp_rp %>%
  filter(down == 3) %>%
  group_by(posteam) %>%
  summarize(thirddownsuccessovr = mean(success), thirddownpassrate = mean(pass)) %>%
  arrange(thirddownsuccessovr)

seconddownoverall <- pbp_rp %>%
  filter(down == 2) %>%
  group_by(posteam) %>%
  summarize(seconddownsuccessovr = mean(success), seconddownpassrate = mean(pass)) %>%
  arrange(seconddownsuccessovr)

firstdownoverall <- pbp_rp %>%
  filter(down == 1) %>%
  group_by(posteam) %>%
  summarize(firstdownsuccessovr = mean(success), firstdownpassrate = mean(pass)) %>%
  arrange(firstdownsuccessovr)

teamstats <- merge(teamstats, earlydownpass)
teamstats <- merge(teamstats, firsthalfpass)
teamstats <- merge(teamstats, secondhalf)
teamstats <- merge(teamstats, thirddownoverall)
teamstats <- merge(teamstats, firstdownoverall)
teamstats <- merge(teamstats, seconddownoverall)

plot(thirddownsuccessovr ~ earlydownpassrate, xlab="Percentage of first and second down plays that are passes", ylab="Third down success rate", data = teamstats, col = "black", pch = 20,
     main = "Early down play calling vs 3rd down success rate")
fit_1 = lm(thirddownsuccessovr ~ earlydownpassrate, data = teamstats)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)

plot(thirddownsuccessovr ~ thirddownpassrate, xlab="Percentage of third down plays that are passes", ylab="Third down success rate", data = teamstats, col = "black", pch = 20,
     main = "Pass rate vs success rate on third down")
fit_1 = lm(thirddownsuccessovr ~ thirddownpassrate, data = teamstats)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)
bptest(fit_1)

plot(successthirdpass ~ airydsthird, xlab="Average distance of third down throws", ylab="Third down success rate", data = teamstats, col = "black", pch = 20,
     main = "Distance on third down throws vs success rate on those throws")
fit_1 = lm(successthirdpass ~ airydsthird, data = teamstats)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)

plot(successsecondpass ~ airydssecond, xlab="Average distance of second down throws", ylab="Second down success rate", data = teamstats, col = "black", pch = 20,
     main = "Distance on second down throws vs success rate on those throws")
fit_1 = lm(successsecondpass ~ airydssecond, data = teamstats)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)

plot(successfirstpass ~ airydsfirst, xlab="Average distance of first down throws", ylab="First down success rate", data = teamstats, col = "black", pch = 20,
     main = "Distance on first down throws vs success rate on those throws")
fit_1 = lm(successfirstpass ~ airydsfirst, data = teamstats)
abline(fit_1, col = "darkorange", lwd = 3)

summary(fit_1)
