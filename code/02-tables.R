

library(dplyr)
library(lubridate)
dat <- read.csv("../data/data.csv", colClasses = "character")

dat$tweet.Favorite.Count <- as.numeric(dat$tweet.Favorite.Count)
dat$followers_count <- as.numeric(dat$followers_count)
dat$following_count <- as.numeric(dat$following_count)
dat$retweet_count <- as.numeric(dat$retweet_count)


dat <- dat %>%
  mutate(created.at = ymd_hms(created.at),
         created.at_h = hour(created.at),
         created.at_h_cat = case_when(created.at_h < 4 ~ "t1",
                                      created.at_h >= 4 & created.at_h < 8 ~ "t2",
                                      created.at_h >= 8 & created.at_h < 12 ~ "t3",
                                      created.at_h >= 12 & created.at_h < 16 ~ "t4",
                                      created.at_h >= 16 & created.at_h < 20 ~ "t5",
                                      created.at_h >= 20 & created.at_h < 24 ~ "t6"),
         created.at_2018 = ifelse(created.at >= ymd_hms("2018-11-06 23:59:59"), "after", "before"))

####################################################  H2 (Table 1) #################################################### 
# create ideo extremity

# dw
dat_dw_ex <- dat %>%
  select(bioguide_id, nominate_dim1) %>%
  group_by(bioguide_id) %>%
  summarise(nominate = mean(as.numeric(nominate_dim1)))

dat_dw_ex <- dat_dw_ex[-531, ] # remove the NA row
dat_dw_ex <- dat_dw_ex %>%
  mutate(nominate_ex = abs(as.numeric(nominate) - mean(as.numeric(nominate))),
         nominate_ex = scales::rescale(nominate_ex)) %>%
  select(-nominate)


# barbera's score
dat_tweet_ex <- dat %>%
  select(user_id_str, ideology_theta) %>%
  group_by(user_id_str) %>%
  summarise(ideology_theta = mean(as.numeric(ideology_theta)))

dat_tweet_ex <- dat_tweet_ex %>%
  filter(!is.na(ideology_theta)) %>%
  mutate(ideo_theta_ex = abs(as.numeric(ideology_theta) - mean(as.numeric(ideology_theta))),
         ideo_theta_ex = scales::rescale(ideo_theta_ex)) %>%
  select(-ideology_theta)

# merge both with the main dataset
dat <- left_join(dat, dat_dw_ex, by = "bioguide_id")
dat <- left_join(dat, dat_tweet_ex, by = "user_id_str")



# H2, test 1, dw
dat_outparty <- dat %>%
  filter(target == "outparty") %>%
  mutate(attitude = ifelse(attitude == "negative", 1, 0))

glm_h2_dw <- glm(attitude ~ nominate_ex + log1p(followers_count) + log1p(following_count) + created.at_h_cat, data = dat_outparty, family = "binomial")
summary(glm_h2_dw)
exp(coef(glm_h2_dw))
DescTools::PseudoR2(glm_h2_dw, which = c("Nagelkerke", "CoxSnell"))

# H2, test 1, barbera's score
glm_h2_tweet <- glm(attitude ~ ideo_theta_ex + log1p(followers_count) + log1p(following_count) + created.at_h_cat, data = dat_outparty, family = "binomial")
summary(glm_h2_tweet)
exp(coef(glm_h2_tweet))
DescTools::PseudoR2(glm_h2_tweet, which = c("Nagelkerke", "CoxSnell"))

# H2, test 2, dw
dat_inp_outn <- dat %>%
  filter((target == "inparty" & attitude == "positive") | (target == "outparty" & attitude == "negative")) %>%
  select(user_id_str, party, attitude, ideo_theta_ex, nominate_ex, bioguide_id)
dat_follow_by_account <- dat %>%
  group_by(user_id_str) %>%
  slice(1) %>%
  select(user_id_str, following_count, followers_count)

dat_inp_outn <- left_join(dat_inp_outn, dat_follow_by_account, by = "user_id_str")
# calculate the ratio of tweets sent by a politician that are negative toward the outparty to tweets positive toward the inparty
dat_inp_outn_dw <- dat_inp_outn %>%
  group_by(user_id_str, nominate_ex, following_count, followers_count, party) %>%
  summarise(ratio = mean(attitude == "negative")) %>%
  ungroup()

lm_h2_dw <- lm(ratio ~ nominate_ex + log1p(followers_count) + log1p(following_count), data = dat_inp_outn_dw)
summary(lm_h2_dw)
summary(lm_h2_dw)$r.squared


# H2, test 2, barbera's score
dat_inp_outn_tweet <- dat_inp_outn %>%
  group_by(user_id_str, ideo_theta_ex, following_count, followers_count, party) %>%
  summarise(ratio = mean(attitude == "negative")) %>%
  ungroup()

lm_h2_tweet <- lm(ratio ~ ideo_theta_ex + log1p(followers_count) + log1p(following_count), data = dat_inp_outn_tweet)
summary(lm_h2_tweet)
summary(lm_h2_tweet)$r.squared





#################################################### H3 (Table 2) #################################################### 
# H3, test1
glm_h3 <- glm(attitude ~ factor(party, levels = c("Republican", "Democrat"))+ log1p(followers_count) + log1p(following_count) + created.at_h_cat, data = dat_outparty, family = "binomial")
summary(glm_h3)
exp(coef(glm_h3))
DescTools::PseudoR2(glm_h3, which = c("Nagelkerke", "CoxSnell"))

# H3, test2
dat_inp_outn_party <- dat_inp_outn %>%
  group_by(user_id_str, party, following_count, followers_count) %>%
  summarise(ratio = mean(attitude == "negative")) %>%
  ungroup()

lm_h3 <- lm(ratio ~ factor(party, levels = c("Republican", "Democrat")) + log1p(followers_count) + log1p(following_count), data = dat_inp_outn_party)
summary(lm_h3)
summary(lm_h3)$r.squared


#################################################### H3 - 2018 midterm (Table 3) #################################################### 
# H3 - tests the effects of the midterm election
glm_h3_midterm <- glm(attitude ~ factor(party, levels = c("Republican", "Democrat")) * factor(created.at_2018, levels = c("before", "after")) + log1p(followers_count) + log1p(following_count) + created.at_h_cat, data = dat_outparty, family = "binomial")
summary(glm_h3_midterm)
exp(coef(glm_h3_midterm))
DescTools::PseudoR2(glm_h3_midterm, which = c("Nagelkerke", "CoxSnell"))


dat_inp_outn2018 <- dat %>%
  filter((target == "inparty" & attitude == "positive") | (target == "outparty" & attitude == "negative")) %>%
  select(user_id_str, party, attitude, ideo_theta_ex, nominate_ex, bioguide_id, created.at_2018) %>%
  left_join(., dat_follow_by_account, by = "user_id_str") %>%
  group_by(user_id_str, party, following_count, followers_count, created.at_2018) %>%
  summarize(ratio = mean(attitude == "negative")) %>%
  ungroup()

dat_inp_outn2018_long <- tidyr::pivot_wider(
  dat_inp_outn2018,
  names_from = created.at_2018,
  values_from = ratio
)

t.test(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Democrat",]$after, dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Democrat",]$before, paired = TRUE)
mean(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Democrat",]$after, na.rm = T); sd(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Democrat",]$after, na.rm = T)
mean(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Democrat",]$before, na.rm = T); sd(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Democrat",]$before, na.rm = T)

t.test(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Republican",]$after, dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Republican",]$before, paired = TRUE)
mean(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Republican",]$after, na.rm = T); sd(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Republican",]$after, na.rm = T)
mean(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Republican",]$before, na.rm = T); sd(dat_inp_outn2018_long[dat_inp_outn2018_long$party == "Republican",]$before, na.rm = T)


#################################################### RQ1 (Table 4) #################################################### 

dat <- dat %>%
  mutate(target.attitude = paste0(target, attitude))

dat_2levels <- dat %>%
  filter(target.attitude == "inpartypositive" | target.attitude == "outpartynegative")

## likes
lm_rq1_likes <- lm(log1p(tweet.Favorite.Count) ~ target.attitude + party + log1p(followers_count) + log1p(following_count) + topic_label + created.at_h_cat, data = dat_2levels)
summary(lm_rq1_likes)
summary(lm_rq1_likes)$r.squared

## shares
lm_rq1_shares <- lm(log1p(retweet_count) ~ target.attitude + party + log1p(followers_count) + log1p(following_count) + topic_label + created.at_h_cat, data = dat_2levels)
summary(lm_rq1_shares)
summary(lm_rq1_shares)$r.squared
