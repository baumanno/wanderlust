library(data.table)
library(assertthat)
library(glue)

#' Reads a word-to-ID mapping
#'
#' read_wordmap reads the "wordmap.txt" file returned from GibbsLDA++ which
#' maps every word in the voabulary to an integer ID.
#'
#' @param path the path to the file
#' @param ... additional arguments passed to data.table::fread
#'
#' @return a data.table with columns \code{word} and \code{id}
#' @export
#'
#' @examples
#' wordmap <- read_wordmap("wordmap.txt")
read_wordmap <- function(path, ...) {
  assert_that(is.readable(path), msg = glue("Given path {path} is not readable"))
  
  column_names <- c("word", "id")
  fread(
    file = path,
    # columns are space-separated
    sep = " ",
    # file does not contain a header with column names
    header = FALSE,
    # first line contains total amount of words
    skip = 1,
    col.names = column_names,
    ...
  )
}

#' Reads a word-topic-distribution-file
#'
#' GibbsLDA++ returns a file called "<model-name>.phi" containing the word-topic
#' distribution P(word|topic). Each line is a document, each column a word.
#' Column numbers correspond directly to word-IDs mapped in "wordmap.txt".
#' Row numbers correspond to topic numbers returned from the algorithm.
#'
#' @param path  the path to <model-name>.phi
#' @param ... additional arguments passed to data.table::fread
#'
#' @return a data.table with ntopic rows and nwords columns
#' @export
#'
#' @examples
#' dt <- read_word_topic_distribution("model-final.phi")
read_word_topic_distribution <- function(path, ...) {
  assert_that(is.readable(path), msg = glue("Given path {path} is not readable"))
  
  fread(file = path,
        sep = " ",
        header = FALSE,
        ...)
}

#' Returns the top words for a topic model
#'
#' Uses the word-topic-distribution to determine the \code{n} words with maximum
#' probability for a document and maps them to real words using the wordmap.
#'
#' @param wtm a matrix with the word-topic-distribution
#' @param wm a dataframe containing the word-ID-mapping
#' @param n the number of words to return
#' @param decreasing whether to return most or least likely words
#'
#' @return a matrix with ndoc rows and n columns
#' @export
#'
#' @examples
#' # Not run:
#' wtm <- read_word_topic_distribution("model-final.phi")
#' wm <- read_wordmap("wordmap.txt")
#'
#' # return the 10 top words
#' top_words(wtm, wm, 10, decreasing = TRUE)
#'
#' # return the 10 least likely words
#' top_words(wtm, wm, 10, decreasing = FALSE)
top_words <-
  function(wtm,
           wm,
           n,
           topics = NULL,
           decreasing = TRUE) {
    assert_that(nrow(wtm) > 0, msg = "Input matrix contains no documents")
    assert_that(ncol(wtm) > 0, msg = "Input matrix contains no words")
    
    assert_that(ncol(wm) == 2, msg = "Wordmap should have exactly two columns")
    assert_that(nrow(wm) == ncol(wtm), msg = "Wordmap should contain as many rows as word-topic-matrix has columns")
    
    assert_that(is.count(n), msg = "Number of top words must be positive integer")
    
    assert_that(all(topics > 0), msg = "topics must be vector of positive IDs")
    
    assert_that(is.flag(decreasing), msg = "Sort order must be given as boolean")
    
    if (length(topics) > 0) {
      wtm <- wtm[topics,]
    }
    
    n_words <- c(1:n)
    
    top_words_ind <- t(apply(
      X = wtm,
      MARGIN = 1,
      FUN = order,
      decreasing = decreasing
    )[n_words, ]) - 1
    
    # we must subtract 1 at the end, as wordmap-indices start at 0, but R's indexing
    # starts at 1
    
    words <- wm[match(top_words_ind, wm$id), ]$word
    
    matrix(words, nrow = nrow(top_words_ind))
  }
