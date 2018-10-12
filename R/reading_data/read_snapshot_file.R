#' Read a data file containing snapshot data
#' 
#' Given a username and path, this function constructs
#' the full path to the snapshot-file. This file is read 
#' and the resulting tibble returned in a list over all dates 
#' in the range.
#' The path is interpreted with `glue_data` and 
#' thus can contain parameters enclosed in `{}`.
#' The optional column specification allows fine-tuning of the
#' data-types.
#'
#' @param user 
#' @param path 
#' @param cols 
#'
#' @return
#' @export
#'
#' @examples
read_monthly_snapshot_data <- function(user, path, cols = NULL) {
  
  years <- 2007:2018
  months <- 1:12
  
  df <- cross_df(list(
    month = months,
    year = years,
    user = user
  ))
  
  # we only want to look at data from 11-2007 to 02-2018
  df <- df[-which(df$month < 11 & df$year == 2007),]
  df <- df[-which(df$month > 2 & df$year == 2018),]
  
  filelist <- glue_data(df, path)
  
  lapply(filelist, function(fname) {
    frame <- read_csv(fname, col_types = cols)
    
    # For some data, the year and month can be missing, 
    # so we attempt to extract them from the filename.
    if (!"year" %in% colnames(frame) && !"month" %in% colnames(frame)) {
      
      d <- fname %>% 
        str_extract(. , "\\d{4}-\\d{1,2}") %>% 
        str_split(. , "-")
      
      frame$year <- as.integer(rep_len(d[[1]][1], nrow(frame)))
      frame$month <- as.integer(rep_len(d[[1]][2], nrow(frame)))
    }
    
    frame
  })
}