#!/usr/bin/env Rscript

###########################################################################
# Collect the top topics for a user
#
# For a given date, the top <X> topics for a user are retrieved. The data
# is stored in ./_cases/<ego>/<year>/ego-topics-<month>.csv
#
# Usage:
#  ./2_ego_top_topics.R <ego> <year> <month> <num_topics>
#
# Bash one-liner with parallel distribution across months of a year:
#  for i in $(seq 2007 2018); do seq 1 12 | parallel -j4 ./2_ego_top_topics.R formido $i {}; done
#
###########################################################################

library(DBI)
library(glue)
library(data.table)

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

num_topics <- if (length(args) >= 4) {
  message(glue("Will retrieve {args[4]} topics"))
  args[4]
} else {
  5
}

bypass_cache <- if (length(args) >= 5) {
  message("Bypassing cached data")
  TRUE
} else {
  FALSE
}

# 2: setup global state -----------------------------------------------------

# where data is stored
data_dir <- glue("~/Masterarbeit 2/_cases/{user}")

# location of database file
db_path <- "/run/media/oliver/Elements SE/reddit.db"

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# WAL-mode is a lot faster
r <- dbExecute(con, "PRAGMA journal_mode=WAL")

# 3: fetch top subreddits for ego ------------------------------------------

fetch_top_subreddits_from_db <- function(user, year, month, num) {
  date_partition <- glue("date-part-{year}-{month}")
  
  sql <- glue_sql("
SELECT
  `year`,
  `month`,
  `author`,
  `subreddit`,
  `topic`,
  `count`
FROM
  (
  SELECT
    *,
    COUNT(`subreddit`) AS count
  FROM
    {`date_partition`}
  WHERE
    `author` = {user}
    AND
    `year` = {year}
    AND
    `month` = {month}
  GROUP BY
    `subreddit`
  ORDER BY
    `count` DESC
  LIMIT
    {num})
LEFT JOIN
  subreddit_topics
USING(`subreddit`)
                  ", .con = con)
  
  q <- dbSendQuery(con, sql)
  res <- dbFetch(q, n = -1)
  dbClearResult(q)
  dbDisconnect(con)
  
  print(res)
  
  return(res)
}

# 4: wrap everything up ---------------------------------------------------

run <- function(user, year, month, num_topics) {
  outfile <- glue("{data_dir}/ego-topics-{year}-{month}.csv")
  
  if (file.exists(outfile) && bypass_cache != TRUE) {
    stop("File exists and we should respect cache. Exiting.")
  }
  
  if (!dir.exists(data_dir)) {
    message(glue("Creating data directory {data_dir}"))
    dir.create(data_dir, recursive = TRUE, mode = "0755")
  }
  
  df <- fetch_top_subreddits_from_db(user, year, month, num_topics)
  
  fwrite(df, outfile)
  message(glue("wrote {nrow(df)} rows to {outfile}"))
}

run(user, year, month, num_topics)
