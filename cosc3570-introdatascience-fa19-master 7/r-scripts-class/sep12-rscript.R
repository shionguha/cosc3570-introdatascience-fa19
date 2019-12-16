library(tidyverse)
library(reshape2)

names(weather)[1] <- "id"
names(weather)[2] <- "year-month"
names(weather)[3] <- "measurement"

weather <- separate(weather, col = "year-month", into = c("year", "month"), sep = 4)

weather <- subset(weather, year == 1955)

#MELTING DATA to get all the days under a single column (One column for one variable)

cleanData <- melt(weather, id = 1:4)

cleanData <- melt(rawData, id = 1:4)

cleanData$day <- as.integer(str_replace(cleanData$variable, "d", ""))
cleanData$date <- as.Date(ISOdate(cleanData$year, cleanData$month, cleanData$day))

cleanData$year <- NULL
cleanData$month <- NULL
cleanData$day<- NULL
cleanData$variable <- NULL

cleanData <- cleanData[c("id", "date", "measurement", "value")]
cleanData <- arrange(cleanData, date, measurement)

cleanData <- na.omit(cleanData, cols="date")

#CASTING DATA

casted_data <- dcast(cleanData, id + date ~ measurement, value.var = "value")