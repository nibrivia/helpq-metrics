# !/bin/R

library(dplyr)
library(readr)
library(purrr)

stu_q <- read_csv("student-queue.csv")
sta_q <- read_csv("staff-queue.csv")

anon_cols <- c("username", "staff_username", "staff")
translation_tables <- list()

vec_to_id <- function(xs, x_name = deparse(substitute(xs))) {
  uxs <- unique(xs)
  translation <- data.frame(id = seq_len(length(uxs)))
  translation[[x_name]] <- uxs

  translation <- translation %>%
    mutate(id = sample(n()),
           id = ifelse(is.na(.[[x_name]]), NA, id)) %>%
    arrange(id)

  translation
}

sta_q %>%
  full_join(y = vec_to_id(.$staff, "staff")) %>%
  as_tibble() %>%
  select(-staff) %>%
  write_csv("staff-queue-anon.csv")

stu_q %>%
  full_join(y = vec_to_id(.$username, "username"), by = "username") %>%
  full_join(y = vec_to_id(.$staff_username, "staff_username"), by = "staff_username", suffix = c(".stu", ".sta")) %>%
  as_tibble() %>%
  select(-username, -staff_username) %>%
  write_csv("student-queue-anon.csv")
