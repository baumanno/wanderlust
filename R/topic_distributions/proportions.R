#' Monthly proportions of ego posts
#' 
#' Computes how many ego-posts, relative to all posts in that month, were made
#' in a topic.
#'
#' @param data the dataframe containing monthly data on posts 
#'
#' @return the input, augmented with proportions
#' @export
#'
#' @examples
post_proportions <- function(data = ego_topics) {
  data %>%
    unnest() %>%
    spread(key = topic,
           value = count,
           fill = 0) %>%
    gather(
      key = topic,
      value = count,
      factor_key = TRUE,
      -year,
      -month,
      -date,
      -author,
      -subreddit
    ) %>%
    group_by(date, topic) %>%
    summarise(num_posts = sum(count)) %>%
    mutate(prop = num_posts / sum(num_posts)) %>%
    ungroup()
}

#' Monthly proportions of alters
#' 
#' Computes how many alters, relative to the whole network, post in a topic. 
#' Topic membership is determined by where the most posts are made by an alter.
#' 
#' Data is filtered:
#' - topic must have minimum of 2 users in a month
#' - topic must be set, i.e. no NA 
#'
#' @param data the dataframe containing monthly data on alters 
#'
#' @return the input, augmented with proportions
#' @export
#'
#' @examples
user_proportions <- function(data = alters_topics) {
  alters_topics %>%
    unnest() %>%
    group_by(date, topic) %>%
    summarise(num_users = n()) %>%
    filter(num_users > 1, !is.na(topic)) %>%
    spread(key = topic,
           value = num_users,
           fill = 0) %>%
    gather(key = topic,
           value = num_users,
           factor_key = TRUE,
           -date) %>%
    mutate(prop = num_users / sum(num_users)) %>%
    ungroup()
}