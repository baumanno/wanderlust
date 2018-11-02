library(tidyverse)
library(glue)

user <- "monocasa"

# brings ego_filtered and alters_filtered into scope
ego_filtered <- NULL
alters_filtered <- NULL

load(glue("output/{user}_ego-filtered.rda"))
load(glue("output/{user}_alters-filtered.rda"))

ego_filtered <-
  ego_filtered %>%
  select(date, topic, p_ego = prop)

alters_filtered <-
  alters_filtered %>%
  select(date, topic, p_alters = prop)

df <- merge(ego_filtered, alters_filtered, by = c("date", "topic")) %>% 
  mutate(
    pa = p_ego * p_alters
  )

dates <- df %>% group_by(topic) %>% filter(p_ego == 0) %>% arrange(date)

ggplot(df, mapping = aes(x = date, y = value, colour = variable)) +
  geom_line(mapping = aes(y = pa, colour = "pa")) +
  geom_line(mapping = aes(y = p_ego, colour = "p_ego")) +
  geom_line(mapping = aes(y = p_alters, colour = "p_alters")) +
  geom_vline(data = dates, aes(xintercept = date), size = .3) +
  facet_wrap(~topic) +
  scale_x_date(date_labels = "%m-%y") +
  theme(axis.text.x = element_text(angle = 90))

df %>% filter(!(p_ego == 0 & p_alters == 0)) %>% 
ggplot(mapping = aes(x = p_ego, y = p_alters)) +
  geom_point(mapping = aes(colour = topic), alpha = .3) +
  geom_smooth(mapping = aes(colour = topic), method = "lm") +
  geom_abline() +
  facet_wrap(~ topic)
