library(purrr)
library(dplyr)
library(glue)
library(readr)

#' Read a file containing snapshot data
#'
#' For a given user and date range, read the file specified by path.
#' This constructs a cross-product of all year and month values and
#' constructs the path to the snapshot files.
#'
#' @param user the user for which to read the data
#' @param years the years to be included in the data
#' @param months the months for which to read the snapshot
#' @param path the path to the file, containing placeholders that are
#'             interpreted by glue, e.g. \code{{user}}.
#'
#' @return a tibble containing the data
#' @export
#'
#' @examples
read_snapshot <-
  function(user,
           years = 2007:2018,
           months = 1:12,
           path,
           cols) {
    # cross-product of all dates in the given range
    cross_df(list(
      month = months,
      year = years,
      author = user
    )) %>%
      mutate(data =  map(glue_data(., path),
                         read_csv, col_types = cols))
  }

#' Read an edgelist snapshot
#'
#' This is a wrapper around the more general \code{read_snapshot()} returning
#' edgelists. An edgelist snapshot captures the ego network for a given date,
#' i.e. what users an ego has interacted with in a given month.
#'
#' @param user
#' @param years
#' @param months
#'
#' @return
#' @export
#'
#' @examples
read_edgelist <-
  function(user, ...) {
    read_snapshot(
      user = user,
      path = "data/{user}/edgelist-{year}-{month}.csv",
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
#' @param years
#' @param months
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
      path = "data/{user}/ego-topics-{year}-{month}.csv",
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
#' @param years
#' @param months
#'
#' @return
#' @export
#'
#' @examples
read_alters_topics <-
  function(user, ...) {
    read_snapshot(
      user = user,
      path = "data/{user}/alters-topics-{year}-{month}.csv",
      cols = cols(
        author = col_character(),
        subreddit = col_character(),
        maxcount = col_integer(),
        topic = col_integer()
      ),
      ...
    )
  }
