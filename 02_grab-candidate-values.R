library(tidyverse)

candidates_and_statements <- read_rds("candidates_and_statements.rds")

# what do we want to identify? let's start with this
# * support for medicare for all or similar
# * any mention of housing
# * union membership
# * (rural) progressive justice alliance slate
statements_classified <- candidates_and_statements %>% 
  #drop_na(statement_full) %>%
  mutate(statement_lower_case = str_to_lower(statement_full),
         m4a     = str_detect(statement_lower_case, "medicare"),
         housing = str_detect(statement_lower_case, "housing"),
         climate = str_detect(statement_lower_case, "climate"),
         union   = str_detect(statement_lower_case, "union"),
         progres = str_detect(statement_lower_case, "progressive"),
         pja     = str_detect(statement_lower_case, "progressive justice"),
         rpa     = str_detect(statement_lower_case, "rural progressive"),
         slate   = str_detect(statement_lower_case, "slate"))

statements_classified %>%
  select(m4a:slate) %>%
  summarize(across(everything(), sum))

# didn't get a ton of info here, but...
