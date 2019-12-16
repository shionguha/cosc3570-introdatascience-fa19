
## ------------------------------------------------------------------------
library(ggplot2)
library("rjson")


## ------------------------------------------------------------------------
schoolDataJSON <- fromJSON(file="Speake-006134681.json")

schoolDataJSON <- lapply(schoolDataJSON, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
})
schoolDataFrame <- as.data.frame(schoolDataJSON, stringsAsFactors=FALSE)


## ------------------------------------------------------------------------
schoolDataFrame = as.data.frame(t(as.matrix(schoolDataFrame)))
schoolDataFrame = tibble::remove_rownames(schoolDataFrame)


## ------------------------------------------------------------------------
schoolDataFrame <- schoolDataFrame[,colSums(is.na(schoolDataFrame))<nrow(schoolDataFrame)]

schoolDataFrame <- subset(schoolDataFrame, select=-c(primaryPhoto,primaryPhotoThumb,rankingDisplayName,ranking,schoolType,rankingType,nonResponderText))

names(schoolDataFrame) <- gsub(x = names(schoolDataFrame), pattern = "-", replacement = "")  


## ------------------------------------------------------------------------
g <- ggplot(data=subset(schoolDataFrame, !is.na(costafteraid)), aes(x=as.numeric(levels(tuition))[tuition], y=as.numeric(levels(costafteraid))[costafteraid], color=institutionalControl, group=institutionalControl))

g <- g + geom_point() + geom_smooth(method="lm")

g <- g + scale_x_continuous(name="Tuition") + scale_y_continuous(name="Cost After Aid")

g <- g + ggtitle("Tuition vs Cost After Aid", subtitle="From US News University Statistics") + theme_classic() + labs(color="Institutional Control")
plot(g)


## ------------------------------------------------------------------------
g1 <- ggplot(data=subset(schoolDataFrame, !is.na(costafteraid)), aes(as.numeric(levels(tuition))[tuition], as.numeric(levels(costafteraid))[costafteraid]))

g1 <- g1 + geom_point() + facet_wrap(~ cut_number(as.numeric(levels(percentreceivingaid))[percentreceivingaid], 4))

g1 <- g1 + geom_smooth(method="lm") + scale_x_continuous(name="Tuition") + scale_y_continuous(name="Cost After Aid") + ggtitle("Tuition vs Cost After Aid by Percent Receiving Aid", subtitle="From US News University Statistics") + theme_classic()

plot(g1)


## ------------------------------------------------------------------------
library(maps)
library(tidyverse)
library(viridis)
library(ggthemes)
us_states <- map_data("state")

state_abbs <- tibble(state = str_to_lower(state.name), abb = state.abb)

schoolDataFrame_m <- subset(schoolDataFrame, !is.na(acceptancerate)) %>% 
    group_by(state) %>% 
    summarise(meanTuition=mean(as.numeric(levels(tuition))[tuition]))

schoolDataFrame_m <- left_join(schoolDataFrame_m, state_abbs, by = c("state" = "abb")) %>% rename(id = state)

schoolDataFrame_m <- left_join(schoolDataFrame_m, us_states, by = c("state.y" = "region"))

g2 <- ggplot(data=schoolDataFrame_m, aes(x=long, y=lat, group=group,fill=meanTuition))

g2 <- g2 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

g2 <- g2 + scale_fill_gradient(low = "white", high = "#CB454A") +
        ggtitle("Average Tuition by State", subtitle="From US News University Statistics")  + theme_map() + labs(fill="Cost")

plot(g2)


## ------------------------------------------------------------------------
library(zipcode)
data(zipcode)

schoolDataFrame_z <- subset(schoolDataFrame, select=+c(actavg,satavg,enrollment,sortName,zip,acceptancerate,rankingDisplayScore,percentreceivingaid,costafteraid,rankingSortRank,hsgpaavg,tuition,displayName,overallRank,institutionalControl))

schoolDataFrame_z <- left_join(schoolDataFrame_z, zipcode, by = c("zip" = "zip"))


## ------------------------------------------------------------------------
#removed incomplete instances
schoolDataFrame_z <- schoolDataFrame_z[complete.cases(schoolDataFrame_z),]
schoolDataFrame_z$actavg <- as.numeric(as.character(schoolDataFrame_z$actavg))
schoolDataFrame_z$satavg <- as.numeric(as.character(schoolDataFrame_z$satavg))
schoolDataFrame_z$rankingSortRank <- as.numeric(as.character(schoolDataFrame_z$rankingSortRank))
schoolDataFrame_z$acceptancerate <- as.numeric(as.character(schoolDataFrame_z$acceptancerate))
schoolDataFrame_z$costafteraid <- as.numeric(as.character(schoolDataFrame_z$costafteraid))
costAfterAidModel <- lm(costafteraid ~ acceptancerate + rankingSortRank + actavg + satavg, data = schoolDataFrame_z)
summary(costAfterAidModel)
summary(costAfterAidModel)$coefficients
#none of the regressors are that significant, rank is slightly


## ------------------------------------------------------------------------
rankModel <- lm(rankingSortRank ~ actavg + satavg + acceptancerate, data = schoolDataFrame_z)
summary(rankModel)
#rank is extremely correlated to actavg, satavg, and acceptance rate (no surprise there)


## ------------------------------------------------------------------------
schoolDataFrame_z$enrollment <- as.numeric(as.character(schoolDataFrame_z$enrollment))
schoolDataFrame_z$tuition <- as.numeric(as.character(schoolDataFrame_z$tuition))
schoolDataFrame_z$percentreceivingaid <- as.numeric(as.character(schoolDataFrame_z$percentreceivingaid))
schoolDataFrame_z$hsgpaavg <- as.numeric(as.character(schoolDataFrame_z$hsgpaavg))

model <- lm(costafteraid ~ actavg + satavg + enrollment + tuition + acceptancerate + percentreceivingaid + rankingSortRank + hsgpaavg + institutionalControl, data = schoolDataFrame_z)
summary(model)


## ------------------------------------------------------------------------
model <- lm(costafteraid ~ tuition + percentreceivingaid, data = schoolDataFrame_z)
summary(model)


## ------------------------------------------------------------------------
g1 <- ggplot(data=subset(schoolDataFrame, !is.na(costafteraid)), aes(as.numeric(levels(tuition))[tuition], as.numeric(levels(costafteraid))[costafteraid], color=institutionalControl, group=institutionalControl))

g1 <- g1 + geom_point() + facet_wrap(~ cut_number(as.numeric(levels(percentreceivingaid))[percentreceivingaid], 4))

g1 <- g1 + geom_smooth(method="lm") + scale_x_continuous(name="Tuition") + scale_y_continuous(name="Cost After Aid") + ggtitle("Tuition vs Cost After Aid by Percent Receiving Aid", subtitle="From US News University Statistics") + theme_classic() + labs(color="Institutional Control")

plot(g1)


## ------------------------------------------------------------------------
schoolDataFrame$avgHelp <- ((1 - (as.numeric(schoolDataFrame$costafteraid) / as.numeric(schoolDataFrame$tuition))) * 100)

schoolDataFrame[schoolDataFrame < 0] <- 0

schoolDataFrame_z <- left_join(schoolDataFrame_z, schoolDataFrame, by = c("displayName" = "displayName"))


## ------------------------------------------------------------------------
g <- ggplot(data=schoolDataFrame_z, aes(x=as.numeric(percentreceivingaid.y), y=as.numeric(avgHelp)))

g <- g + geom_point() + geom_smooth(method="lm")

g <- g + scale_x_continuous(name="Percent Receiving Aid") + scale_y_continuous(name="Average Percent of Tuition Covered by Aid")

g <- g + ggtitle("Average Percent of Tuition Covered by Aid vs Percent Receiving Aid", subtitle="From US News University Statistics") + theme_classic() + labs(color="Institutional Control")
plot(g)

