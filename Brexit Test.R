# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500

Remain_pop <- N*p
se_pop <- sqrt((1-p)*p/N) * N
x_hat <- p
se_x_hat <-sqrt((1-p)*p/N) *2

brexit_polls <- brexit_polls %>% mutate(x_hat = (spread+1)/2)
p <- mean(brexit_polls$spread)

brex <- brexit_polls[1,]
spread <- brex$spread
x_hat <- brex$x_hat
size <- brex$samplesize
p <- (spread+1) /2
Q <- qnorm(0.975) * sqrt(p*(1-p)/size)
x_hat - Q
x_hat
qnorm(0.975)
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat * (1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         lower = spread - 1.96 * se_spread,
         upper = spread + 1.96 * se_spread,
         hit = (-0.038 > lower) & (-0.038 < upper))

sorted <- june_polls %>% group_by(pollster) %>%
  summarise(hits = mean(hit), n = n()) %>% arrange(-hits)

box <- june_polls %>% ggplot(aes(spread, poll_type)) + geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type <- combined_by_type %>% mutate(
  lower = spread - 1.96 * (2*sqrt(p_hat * (1-p_hat)/N)),
  upper = spread + 1.96 * (2*sqrt(p_hat * (1-p_hat)/N)),
  differen = upper-lower)

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)$p.value

# online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]

# statistically significant
chisq.test(brexit_chisq)$p.value < 0.05

# convert to data frame
chisq_df <- as.data.frame(brexit_chisq)

online_true <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "TRUE"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "FALSE"]

online_odds <- online_true/online_false
online_odds
brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>%
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method= "loess", span = 0.3)