library(httr)
library(jsonlite)
library(sjmisc)
library(stringr)
library(config)
source("config.R")

read_list <- function(filepath) {
  con <- file(filepath, "r")
  items <- 0
  while (TRUE) {
    name <- readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    user_lookup(name)
    items <- items + 1
    if (items >= requests) { # nolint: object_usage_linter.
        print(noquote("Sleeping for 60 seconds to prevent ratelimiting..."))
        Sys.sleep(cooldown) # nolint: object_usage_linter.
        items <- 0
    }
  }
  close(con)
}

user_lookup <- function(name) {
  post_body <- sprintf("username=%s", name)

  cookie <- sprintf("XSRF-TOKEN=%s; osu_session=%s", xsrf, session) # nolint: object_usage_linter, line_length_linter.

  r <- POST("https://osu.ppy.sh/users/check-username-availability",
  body = post_body,
  add_headers(.headers = c(
  "Content-Type" = "application/x-www-form-urlencoded",
  "User-Agent" = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:95.0) Gecko/20100101 Firefox/95.0", # nolint: line_length_linter.
  "Cookie" = cookie,
  "X-CSRF-Token" = xsrf, # nolint: object_usage_linter.
  "X-Requested-With" = "XMLHttpRequest")))

  data <- fromJSON(rawToChar(r$content))

  if (data$available != "FALSE") {
    if (verbose) {
      print(sprintf("%s is available now", data$username))
    }
    cat(data$username,file="available.txt",sep="\n",append=TRUE)
  } else if (dropping && str_contains(data$message, "will")) { # nolint: object_usage_linter, line_length_linter.
    time <- length(data$message) - 42
    if (verbose) {
      print(sprintf("%s is available %s",
      data$username, str_sub(data$message, start = -time)))
    }
    txt <- sprintf("%s %s", data$username, str_sub(data$message, start = -time)) # nolint: line_length_linter.
    cat(txt,file="available.txt",sep="\n",append=TRUE)
  } else if (data$available == "FALSE" && verbose) {
    print(sprintf("%s is unavailable", data$username))
  }
}

main <- function() {
    if (file.exists("list.txt")) {
      if (file.exists("available.txt")) {
        read_list("list.txt")
      } else {
        file.create("available.txt")
        read_list("list.txt")
      }
    } else {
        print("No list of names, exiting...")
        quit()
    }
}

main()