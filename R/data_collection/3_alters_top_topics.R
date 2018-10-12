#!/usr/bin/env Rscript

###########################################################################
# Collect the topics an ego's alters are most active in
#
# The alters are identified via the edgelists, which should have been
# pre-computed at this point. The number of topics is restricted to 1, since
# we are not interested in the topic distribution of the alters, but rather
# in the proportion of users that are active in a topic.
#
# Usage:
#  ./3_alter_top_topics.R <ego> <year> <month>
#
# Bash one-liner with parallel distribution across months of a year:
#  for i in $(seq 2007 2018); do seq 1 12 | parallel -j4 ./3_alter_top_topics.R formido $i {}; done
#
###########################################################################

library(DBI)
library(glue)
library(data.table)
suppressPackageStartupMessages(library(tidyverse))

# 1: parse command line arguments -----------------------------------------
args <- commandArgs(trailingOnly = TRUE)

user <- if (length(args) >= 1) {
  args[1]
} else {
  stop("No user provided")
}

year <- if (length(args) >= 2) {
  args[2]
} else {
  stop("No year provided")
}

month <- if (length(args) >= 3) {
  args[3]
} else {
  stop("No month provided")
}

bypass_cache <- if (length(args) >= 4) {
  as.logical(args[4])
} else {
  FALSE
}

is_debug <- if (length(args) >= 5) {
  as.logical(args[5])
} else {
  FALSE
}

# 2: setup global state -----------------------------------------------------

# where data is stored
data_dir <- glue("~/Masterarbeit 2/_cases/{user}")

# location of database file
db_path <- "/run/media/oliver/Elements SE/reddit.db"

if(bypass_cache) message("Bypassing cache")
if(is_debug) message("Debug mode ON")

# 3: fetch the most active topic for a user -------------------------------

fetch_topic_for_alters <- function(year, month, author, alters) {
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # WAL-mode is a lot faster
  r <- dbExecute(con, "PRAGMA journal_mode=WAL")
  
  date_partition <- glue("date-part-{year}-{month}")
  
  if (length(alters) == 0) {
    alters <- c("")
  }
  
  sql <- glue_sql(
    "
SELECT 
  *
FROM (
  SELECT
    `year`,
    `month`,
    `author`,
    `subreddit`,
    MAX(count) as maxcount
  FROM (
    SELECT
      `author`,
      `subreddit`,
      COUNT(`subreddit`) as count
    FROM
      {`date_partition`}
    WHERE
      `author` IN ({alters*})
    GROUP BY
      author,
      subreddit
  )
  GROUP BY
    `author`
)
LEFT JOIN
  subreddit_topics
USING(`subreddit`)
",
    .con = con
  )

  if (is_debug) print(sql)
  
  # Use wal mode for increased performance, especially when running
  # alongside other DB-scripts
  dbExecute(con, "PRAGMA journal_mode=wal")
  
  # Find top subreddit for a user.
  # Aggregates the subreddits over a month and takes the max count.
  q <- dbSendQuery(con, sql)
  
  # fetch all results from DB
  res <- dbFetch(q, n = -1)
  
  dbClearResult(q)
  dbDisconnect(con)
  
  # return something meaningful even if no topic was found
  if (nrow(res) == 0) {
    message(glue("Empty result in {year}-{month}"))
    #return(NA)
  }
  
  if(is_debug) print(res)
  
  # return only the topic number
  return(res)
}

run <- function(user, year, month) {
  outfile <- glue("{data_dir}/alters-topics-{year}-{month}.csv")
  
  if (file.exists(outfile) && bypass_cache != TRUE) {
    stop(glue("[{year}-{month}] File exists and we should respect cache. Exiting."))
  }
  
  filename <- glue("{data_dir}/edgelist-{year}-{month}.csv")
  
  edgelist <- fread(filename, colClasses=c("source"="character", "sink"="character"))
  
  # alters <-
  #   tibble("author" = unique(c(edgelist$source, edgelist$sink))) %>%
  #   filter(author != user)# %>%
  #   # mutate(topic = vapply(author, function(x) {
  #   #   fetch_topic_for_user(year, month, x)
  #   # }, integer(1))
  #   )
  alters <- unique(c(edgelist$source, edgelist$sink))
  alters <- alters[alters != user]
  
  alters_with_topics <- fetch_topic_for_alters(year, month, user, alters)
  
  fwrite(alters_with_topics, outfile)
  message(glue("wrote {nrow(alters_with_topics)} rows to {outfile}"))
}

run(user, year, month)
