# Setup
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
data("midwest", package = "ggplot2")  # load the data
# midwest <- read.csv("http://goo.gl/G1K41K") # alt source 

# Init Ggplot
ggplot(midwest, aes(x=area, y=poptotal))  # area and poptotal are columns in 'midwest'

#basic scatterplot
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()


g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
plot(g)

# Delete the points outside the limits
g + xlim(c(0, 0.1)) + ylim(c(0, 1000000))   # deletes points
# g + xlim(0, 0.1) + ylim(0, 1000000)   # deletes points

# Zoom in without deleting the points outside the limits. 
# As a result, the line of best fit is the same as the original plot.
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in
plot(g1)

# Add Title and Labels
g1 + labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

#or do the following
g1 + ggtitle("Area Vs Population", subtitle="From midwest dataset") + xlab("Area") + ylab("Population")

#change color and size of data points to static
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(col="steelblue", size=3) +   # Set static color and size for points
  geom_smooth(method="lm", col="firebrick") +  # change the color of line
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

#change color to reflect categories
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
plot(gg)

gg + theme(legend.position="None")  # remove legend

gg + scale_colour_brewer(palette = "Set1")  # change color palette

#see color palettes
library(RColorBrewer)
head(brewer.pal.info, 10)  # show 10 palettes

#change axis texts and ticks
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))

#change axis and labels
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11])

# Reverse X Axis Scale
gg + scale_x_reverse()

#setting custom axis texts
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) + 
  scale_y_continuous(breaks=seq(0, 1000000, 200000), labels = function(x){paste0(x/1000, 'K')})

#looking at pre-built themes
?theme_bw

# method: Adding theme Layer itself.
gg + theme_bw() + labs(subtitle="BW Theme")
gg + theme_classic() + labs(subtitle="Classic Theme")
