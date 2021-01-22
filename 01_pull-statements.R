library(tidyverse)
library(rvest)
library(stringdist)

site <- "https://adem.cadem.org/assembly-districts/ad-4/"

# pull the html from the website into R  
site_html <- read_html(site)

# the first slot is a table of candidate names
candidates <- html_table(site_html)[[1]] %>% 
  # column headers get interpreted as first row of data ... fix that
  set_names(.[1,]) %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  rename(gender = 4)

# all the statements have a name like this:
statements_raw <- html_nodes(site_html, ".et_pb_testimonial_no_image")

# pull text out of these statements
statements_extract <- html_text(statements_raw, trim = TRUE)

# get the name attached to each statement ... always the last line
statements <- tibble(statement_full = statements_extract,
                     # extract all the text between the last newline and the end
                     last_line      = map_chr(statement_full, str_extract, ".+$"),
                     # remove the leading tabs
                     statement_name    = str_trim(last_line)) %>% 
  select(statement_name, statement_full)

# mismatches appear to be from a mix of diacriticals, middle name punctuation
# easily resolved by using string approximate matching to find the closest ballot name to each candidate statement name
statements_ballot_name <- statements %>% 
  mutate(ballot_index = amatch(statement_name, candidates$ballot_name, method = "lv", maxDist = 4),
         ballot_name  = candidates$ballot_name[ballot_index])

statements_ballot_name %>% select(statement_name, ballot_name)

# attach statements to candidates and store dataset
candidates %>% 
  left_join(select(statements_ballot_name, ballot_name, statement_full), 
            by = "ballot_name") %>% 
  write_rds("candidates_and_statements.rds")
