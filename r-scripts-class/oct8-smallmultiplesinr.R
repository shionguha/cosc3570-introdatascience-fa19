df.facet_data <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/12/facet_dummy_data.csv"))
df.facet_data$month <- factor(df.facet_data$month, levels=month.abb)

ggplot(data=df.facet_data, aes(x=df.facet_data$month,y=sales, group=region)) +
  geom_line()

ggplot(data=df.facet_data, aes(x=month,y=sales, group=1)) +
  geom_line() +
  facet_grid(region ~ .)


ggplot(data=df.facet_data, aes(x=region,y=sales)) +
  geom_bar(stat="identity") +
  facet_wrap(~month) +
  ggtitle("Small Multiples in R") +
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90))

