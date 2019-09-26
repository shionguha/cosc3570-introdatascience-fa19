y <- c(1,2,3,NA)

is.na(y) # returns a vector (F F F T)

x <- c(1,2,NA,3)
mean(x) # returns NA
mean(x, na.rm=TRUE) # returns 2


# list rows of data that have missing values
weather_raw[!complete.cases(weather_raw),]


# create new dataset without missing data
newdata <- na.omit(weather_raw)
View(newdata)


library(mice)
library(VIM)


data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

data <- data[-c(5,6)]
summary(data)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)

aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

marginplot(data[c(1,2)])

tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

tempData$imp$Ozone

tempData$meth

completedData <- complete(tempData,1)

#visualizing imputed data
xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)

densityplot(tempData)

stripplot(tempData, pch = 20, cex = 1.2)


