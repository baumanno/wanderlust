#!/usr/bin/env Rscript

###########################################################################
# Collect data about the alters to an ego
#
# The following data is collected from the reddit.db:
#  - raw interactions, i.e. two-column data with dyads participating in an
#    interaction; can be considered the edge-list to a multi-digraph;
#    stored in ./_cases/<ego>/<year>/alters-<month>-edgelist.csv
#
# Usage:
#  ./1_edgelists.R <ego> <year> <month>
#
# Bash one-liner with parallel distribution across months of a year:
#  for i in $(seq 2007 2018); do seq 1 12 | parallel -j4 ./1_edgelists.R formido $i {}; done
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

# 3: fetch the data from DB -----------------------------------------------

fetch_edgelist_from_db <- function(user, year, month) {
  
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # WAL-mode is a lot faster
  r <- dbExecute(con, "PRAGMA journal_mode=WAL")
  
  date_partition <- glue("date-part-{year}-{month}") 
  sql <-
    glue_sql("
            SELECT 
              `t_sink`.author AS sink,
              `t_sink`.parent_id,
              `t_sink`.id,
              `t_source`.parent_id,
              `t_source`.author AS source,
              `t_source`.id
            FROM 
              {`date_partition`} t_sink
              JOIN
              {`date_partition`} t_source
              ON
                `t_sink`.id = substr(`t_source`.parent_id, 4)
            WHERE
              source = {user} 
              OR
              sink = {user}",
             .con = con)
    
  # execute query on DB and fetch result
  q <- dbSendQuery(con, sql)
  res <- dbFetch(q, n = -1) # fetch ALL rows
  dbClearResult(q)
  
  dbDisconnect(con)
  
  return(res)
}


# 4: parse the result into a dataframe ------------------------------------

edgelist_to_dataframe <- function(df) {
  df %>%
    filter(source != "[deleted]" & sink != "[deleted]") %>% 
    select(source, sink)
}

# 5: wrap it all up -------------------------------------------------------
  
run <- function(user, year, month) {
  
  outfile <- glue("{data_dir}/edgelist-{year}-{month}.csv")
  
  if(file.exists(outfile) && bypass_cache != TRUE) {
    stop("File exists and we should respect cache. Exiting.")
  }
  
  if(!dir.exists(data_dir)) {
    message(glue("Creating data directory {data_dir}"))
    dir.create(data_dir, recursive = TRUE, mode = "0755")
  }
  
  df <- fetch_edgelist_from_db(user, year, month)
  df <- edgelist_to_dataframe(df)
  
  fwrite(df, outfile)
  message(glue("wrote {nrow(df)} rows to {outfile}"))
}

run(user, year, month)
