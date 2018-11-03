library("fs")
library("glue")
library("stringr")
library("purrr")
library("parallel")

users <- c("monocasa", "formido", "cavedave", "IronWolve")

mclapply(users, function(user) {
  path <- glue("data/{user}")  
  
  files <- dir_ls(path = path, regexp = "\\d{4}-\\d{1,2}")
  file_move(files, str_replace(files, "-(\\d{1})\\.", "-0\\1\\."))
})