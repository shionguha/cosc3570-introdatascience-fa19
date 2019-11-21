library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

data(efc)
theme_set(theme_sjplot())

# create binary response
y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)

# create data frame for fitting model
df <- data.frame(
  y = to_factor(y),
  sex = to_factor(efc$c161sex),
  dep = to_factor(efc$e42dep),
  barthel = efc$barthtot,
  education = to_factor(efc$c172code)
)

# set variable label for response
set_label(df$y) <- "High Negative Impact"

# fit model
m1 <- glm(y ~., data = df, family = binomial(link = "logit"))

plot_model(m1)

plot_model(m1, vline.color = "red")

plot_model(m1, sort.est = TRUE)

summary(m1)

plot_model(m1, order.terms = c(6, 7, 1, 2, 3, 4, 5))

plot_model(m1, transform = NULL)

plot_model(m1, show.values = TRUE, value.offset = .3)