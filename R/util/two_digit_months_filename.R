library("fs")
library("glue")
library("stringr")
library("purrr")

user <- readline("Please enter a username: ")
path <- glue("data/{user}")

(files <- dir_ls(path = path, regexp = "ego-*"))

walk(files, function(f) {
  d <- str_extract(f, "\\d{4}-\\d{1,2}")
  d <- c(str_split(d, "-")[[1]])
  d <- make_date(d[1], d[2])
  new <- strftime(d, "%Y-%m")
  file.rename(f, glue("{path}/ego-topics-{new}.csv"))
})

system(glue("ls {path}"))
