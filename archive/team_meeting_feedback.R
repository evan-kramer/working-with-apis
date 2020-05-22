# Testing Smartsheet API
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(httr)
library(rjson)
library(odbc)
library(rvest)
library(xml2)
library(XML)
library(jsonlite)
library(data.table)
library(googlesheets4)
library(janitor)
ss_api_key = readRegistry("Environment", hive = "HCU")$smartsheet_api_key
ss_api_pwd = readRegistry("Environment", hive = "HCU")$smartsheet_pwd
qb_api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
qb_api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
qb_api_app = readRegistry("Environment", hive = "HCU")$quickbase_api_token
qb_url = readRegistry("Environment", hive = "HCU")$quickbase_api_url
api_uid = readRegistry("Environment", hive = "HCU")$email_address

# Analysis of team meeting feedback
feedback = read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1vApMh95knR2RAGxdHJvsqKCLRK7RLK3HxcwhJE3d8CU/edit#gid=2030972886",
  sheet = 1
) %>% 
  filter(!is.na(`Right Priorities`)) 

# Are these the right priorities?
ggplot(feedback, aes(`Right Priorities`)) + 
  geom_bar(stat = "count") + 
  theme_bw() +
  scale_y_continuous(
    name = "Number of Responses", 
    limits = c(
      min(
        sum(feedback$`Right Priorities` == "Yes"),
        sum(feedback$`Right Priorities` == "No"),
        sum(feedback$`Right Priorities` == "Maybe")
      ), 
      max(
        sum(feedback$`Right Priorities` == "Yes"),
        sum(feedback$`Right Priorities` == "No"),
        sum(feedback$`Right Priorities` == "Maybe")
      )
    ),
    breaks = seq(0, 10, 2)
  ) + 
  xlab("Are these the right priorities?") 

# Which priorities do folks want to focus on? 
ggplot(feedback, aes(`Priority Focus Area`)) + 
  geom_bar(stat = "count") + 
  theme_bw() +
  scale_y_continuous(
    name = "Number of Responses", 
    limits = c(
      min(
        sum(feedback$`Right Priorities` == "Yes"),
        sum(feedback$`Right Priorities` == "No"),
        sum(feedback$`Right Priorities` == "Maybe")
      ), 
      max(
        sum(feedback$`Right Priorities` == "Yes"),
        sum(feedback$`Right Priorities` == "No"),
        sum(feedback$`Right Priorities` == "Maybe")
      )
    ),
    breaks = seq(0, 10, 2)
  ) + 
  xlab("Priority") +
  coord_flip()

# Follow up survey
feedback = read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1iE93TH_QKXXrsQsUxKQgv8oBRcSgHmpaNZLXiIkIQQU/edit#gid=1078225516",
  sheet = 1
) 

janitor::clean_names(feedback) %>%
  select(-(what_actions_are_missing_within_these_priorities:any_other_input_or_feedback)) %>% 
  rename_at(
    vars(outline_clear_roles_and_responsibilities:cio_data_management_creates_and_implements_data_validation_scripts_for_third_wave_functions_and_adds_to_ude_portal),
    funs(str_c("q_", .))
  ) %>%
  pivot_longer(
    cols = -(timestamp:email_address),
    names_to = "question",
    names_prefix = "q_",
    values_to = "value"
  ) %>% 
  group_by(question) %>% 
  summarize(
    median = median(value, na.rm = T),
    pct_agree = round(100 * sum(value >= 4, na.rm = T) / sum(!is.na(value)), 1)
  ) %>% 
  ungroup() %>% 
  left_join(
    tibble(
      question = names(feedback),
      question_clean = names(clean_names(feedback))
    ) %>% 
      mutate(order = row_number()),
    by = c("question" = "question_clean")
  ) %>% 
  arrange(-pct_agree, order) %>%
  View()
  ggplot(
    aes(
      # x = reorder(str_sub(question.y, 1, 100), -pct_agree),
      # x = reorder(question.y, pct_agree),
      x = ifelse(
        str_length(question.y) <= 60,
        str_c(order, ". ", question.y),
        str_c(order, ". ", str_sub(question.y, 1, 60), "...")
      ) %>%
        reorder(pct_agree),
      y = pct_agree
    )
  ) + 
  geom_bar(stat = "identity") +
  xlab("Question") + 
  scale_y_continuous(name = "% Rated 4 or 5", limits = c(0, 100)) + 
  theme_bw() + 
  coord_flip()

# Did people take it seriously?
janitor::clean_names(feedback) %>%
  select(-(what_actions_are_missing_within_these_priorities:any_other_input_or_feedback)) %>% 
  rename_at(
    vars(outline_clear_roles_and_responsibilities:cio_data_management_creates_and_implements_data_validation_scripts_for_third_wave_functions_and_adds_to_ude_portal),
    funs(str_c("q_", .))
  ) %>%
  pivot_longer(
    cols = -(timestamp:email_address),
    names_to = "question",
    names_prefix = "q_",
    values_to = "value"
  ) %>% 
  group_by(email_address) %>%
  filter(!is.na(value)) %>% 
  summarize_at(
    "value",
    c("mean", "median", "sd")
  )
