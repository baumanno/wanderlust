library(purrr)
library(dplyr)
library(glue)
library(readr)
library(assertthat)

#' Read a file containing snapshot data
#'
#' For a given user and date range, read the file specified by path.
#' This constructs a cross-product of all year and month values and
#' constructs the path to the snapshot files.
#'
#' @param user the user for which to read the data
#' @param date the dates for which to read a snapshot
#' @param path the path to the file, containing placeholders that are
#'             interpreted by glue, e.g. \code{{user}}.
#' @param cols a column specification for the dataframe
#'
#' @return a tibble containing the data
#' @export
#'
#' @examples
read_snapshot <-
  function(user,
           date,
           path,
           cols) {
    assert_that(is.string(user), msg = "Username must be a string")
    assert_that(is_vector(date), msg = "Date-range must be a vector")
    assert_that(is.string(path), msg = "Path to data must be string")
    assert_that(!is_null(cols), msg = "Missing column specification")
    
    # the datafiles are usually indexed by YEAR-MONTH
    date <- strftime(date, "%Y-%m")
    
    df <- data_frame(date = date, "author" = user)
    
    df %>%
      mutate(
        year = str_extract(date, "\\d{4}"),
        # capture only the month, which comes after a dash (-)s
        month = str_extract(date, "(?<=-)\\d{2}"),
        data =  map(glue_data(., path),
                    read_csv,
                    col_types = cols),
        date = make_date(year, month) 
      ) %>% 
      select(
        year, month, date,
        author,
        data
      )
  }

#' Read an edgelist snapshot
#'
#' This is a wrapper around the more general \code{read_snapshot()} returning
#' edgelists. An edgelist snapshot captures the ego network for a given date,
#' i.e. what users an ego has interacted with in a given month.
#'
#' @param user
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_edgelist <-
  function(user, ...) {
    read_snapshot(
      user = user,
      path = "/home/oliver/Masterarbeit/data/{user}/edgelist-{date}.csv",
      cols = cols(source = col_character(),
                  sink = col_character()),
      ...
    )
  }

#' Read snapshot containing top topics for the ego
#'
#' This is a wrapper around the more general \code{read_snapshot()} returning
#' the top topics an ego has participated in.
#'
#' @param user
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_ego_topics <-
  function(user, ...) {
    # helper function for removing dates from the stored snapshot
    drop_cols <- function(df) {
      df %>%
        select(-year, -month, -author)
    }
    
    read_snapshot(
      user = user,
      path = "/home/oliver/Masterarbeit/data/{user}/ego-topics-{date}.csv",
      cols = cols(
        year = col_integer(),
        month = col_integer(),
        author = col_character(),
        subreddit = col_character(),
        topic = col_integer(),
        count = col_integer()
      ),
      ...
    ) %>%
      mutate(data = map(data, drop_cols))
  }


#' Read snapshot containing topics of an ego' alters
#'
#' This is a wrapper around the more general \code{read_snapshot()} returning
#' the ego's alters and the one topic they were most active in on a given date.
#'
#' @param user
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_alters_topics <-
  function(user, ...) {
    read_snapshot(
      user = user,
      path = "/home/oliver/Masterarbeit/data/{user}/alters-topics-{date}.csv",
      cols = cols(
        author = col_character(),
        subreddit = col_character(),
        maxcount = col_integer(),
        topic = col_integer()
      ),
      ...
    )
  }
