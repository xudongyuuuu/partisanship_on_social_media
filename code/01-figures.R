
library(dplyr)
library(ggplot2)
library(showtext)
library(lubridate)
font_add_google("Libre Franklin", "Franklin")
showtext_auto()

dat <- read.csv("../data/data.csv", colClasses = "character")


## create figure 3
tweets_by_id <- dat %>%
  count(user_id_str)

pdf("../figures/figure3.pdf", width = 8, height = 5)

ggplot(tweets_by_id, aes(x = n)) +
  geom_density() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text=element_text(family = "Franklin")) +
  scale_x_continuous(limits=c(0, 2000), breaks = seq(0, 2000, by = 250))

dev.off()



## create figure 4

tweets_summary <- dat %>%
  count(target, attitude) %>%
  mutate(target = ifelse(target == "inparty", "In-party", "Out-party"),
         attitude = recode(attitude, 
                           "negative" = "Negative",
                           "neutral" = "Neutral",
                           "positive" = "Positive"))
tweets_summary

pdf("../figures/figure4.pdf", width = 8, height = 5)

ggplot(tweets_summary, aes(x = attitude, y = n)) +
  geom_col() +
  facet_wrap(~target) +
  geom_text(aes(label = n), vjust = -0.5) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text=element_text(family = "Franklin")) +
  scale_y_continuous(limits=c(0, 95000), breaks = seq(0, 90000, by = 20000))

dev.off()




