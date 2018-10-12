library(tidyverse)
library(glue)
library(gridExtra)
library(igraph)
library(lubridate)

source("R/network_construction/build_from_edgelist.R")
source("R/network_measures/katz_powell.R")
source("R/network_measures/weighted_mutuality.R")
source("R/network_measures/krackhardt.R")

years <- 2007:2018
months <- 1:12
users <- c("monocasa")#, "formido", "cavedave", "IronWolve")

# cross product of dates
df <- cross_df(list(month = months, year = years, user = users))
# filter out months not covered in collected data
df <- df[-which(df$month < 11 & df$year == 2007),]
df <- df[-which(df$month > 2 & df$year == 2018),]

edgelist_names <-
  glue_data(df, "data/{user}/edgelist-{year}-{month}.csv")

topic_metadata <- 
  glue_data(df, "data/{user}/alters-topics-{year}-{month}.csv")

# read each edgelist into a list item...
edgelists <- lapply(edgelist_names, read.csv)

topic_metadata_df <- lapply(topic_metadata, function(x) {
  t <- read_csv(x)
  t$subreddit <- NULL
  t$maxcount <- NULL
  t <- rbind(t, list(author = users, topic = -1))
  t
})

# ... and construct a network from it
list_of_graphs <- build_egonet_from_edgelist(edgelists, topic_metadata_df)

# name each graph by the date it represents
names(list_of_graphs) <- edgelist_names

save(list_of_graphs, file = "output/list_of_graphs.rda")

graph_store <- as.tibble(df) %>%
  transmute(
    month, year,
    author = user,
    vertices = sapply(list_of_graphs, vcount),
    katz_powell = sapply(list_of_graphs, katz_powell_mutuality),
    weighted = mapply(weighted_mutuality, list_of_graphs, author)
  )

save(graph_store, file = "output/graph_store.rda")

a <-
  ggplot(graph_store, mapping = aes(x = make_date(year, month), y = vertices)) +
  geom_point(alpha=1/10) +
  geom_smooth()
b <-
  ggplot(graph_store, mapping = aes(x = make_date(year, month), y = katz_powell)) +
  geom_point(aes(colour = katz_powell), alpha = 1/10) +
  geom_smooth()
c <-
  ggplot(graph_store, mapping = aes(x = make_date(year, month), y = weighted)) +
  geom_point(aes(colour = weighted), alpha = 1/10) +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1))
g_a <- grid.arrange(a, b, c, nrow = 3)

d <-
  ggplot(graph_store, mapping = aes(x = vertices, y = katz_powell)) +
  geom_point(colour = "blue", alpha = 1/10) +
  geom_smooth()
e <-
  ggplot(graph_store, mapping = aes(x = vertices, y = weighted)) +
  geom_point(colour = "blue", alpha = 1/10) +
  geom_smooth()

g_b <- grid.arrange(d, e, nrow = 2)

grid.arrange(g_a, g_b, ncol = 2)
