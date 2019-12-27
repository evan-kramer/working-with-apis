# Data Request Email Updates
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(rvest)
library(odbc)
library(DBI)
library(curl)
library(blastula)
api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
api_uid = readRegistry("Environment", hive = "HCU")$email_address
api_app = readRegistry("Environment", hive = "HCU")$quickbase_api_token
url = readRegistry("Environment", hive = "HCU")$quickbase_api_url

# Get DB table names and IDs from API
## Get a list of DBs I have access to 
granted_dbs = GET(
  str_c(
    url,
    "?a", "=", "API_GrantedDBs", # API_GrantedDBs function,
    "&", "usertoken", "=", api_key # use API user key to authenticate (could also use ticket)
  )
) %>% 
  content()

## Create a tibble and extract names and IDs from API
db = tibble(
  db_name = xml_nodes(granted_dbs, "dbname") %>% 
    str_flatten() %>% 
    str_split("</dbname><dbname>") %>% 
    unlist(),
  db_id = xml_nodes(granted_dbs, "dbid") %>% 
    str_flatten() %>% 
    str_split("</dbid><dbid>") %>% 
    unlist() 
) %>% 
  ## Concatenate to match QuNect naming conventions
  mutate(
    db_name = str_replace_all(db_name, "</dbname>", "") %>% 
      str_replace_all("<dbname>", "") %>% 
      str_replace_all("[ :]", "_"),
    db_id = str_replace_all(db_id, "</dbid>", "") %>% 
      str_replace_all("<dbid>", ""),
    db_name_id = str_c(db_name, db_id, sep  = "_")
  ) 

# Connect to database
con = dbConnect(
  odbc(), 
  "QuickBase via QuNect user",
  timeout = 10
)

# Query database 
## Requests table
requests = dbGetQuery(
  con,
  str_c(
    "select *
    from ",
    db$db_name_id[str_ends(db$db_name, "__Requests")]
  )
) %>% 
  as_tibble() %>% 
  janitor::clean_names()

## Status table
status = dbGetQuery(
  con,
  str_c(
    "select *
    from ",
    db$db_name_id[str_detect(db$db_name, "Status")]
  )
) %>% 
  as_tibble() %>% 
  janitor::clean_names()

## Filter to get a list of status updates 
email_data = arrange(status, request_id, desc(date_modified)) %>% 
  group_by(request_id) %>%
  summarize_at(
    vars(date_modified, status, status_notes, requester_full_name, 
         request_contact_email),
    "first"
  ) %>% 
  ungroup() %>% 
  left_join(
    select(requests, record_id, requesting_organization, first_name, 
           desired_delivery_date, osse_due_date, communications_log),
    by = c("request_id" = "record_id")
  ) %>% 
  filter(!status %in% c("Closed", "Complete") & 
           str_detect(requesting_organization, "OSSE"))

# Send email update
## Set up message body template
message_template = 
'From: <evan.kramer@dc.gov>
To: <evan.kramer@dc.gov>
Subject: Data Request Status Update

Dear <<first_name>>,

I am writing to update you on the status of request <<request_id>>. This request is listed as <<status>> and was last updated on <<date_modified>>.

Please reach out with any questions or feedback.

Thanks,
Evan'

## Loop through all rows
# for(r in 1:nrow(email_data)) {
for(r in c(1, 5, 11, 20)) {
  # Adjust body for all rows
  message = str_replace_all(message_template, "<<first_name>>", email_data$first_name[r]) %>% 
    str_replace_all("<<request_id>>", as.character(email_data$request_id[r])) %>% 
    str_replace_all("<<status>>", email_data$status[r]) %>% 
    str_replace_all("<<date_modified>>", as.character(email_data$date_modified[r]))
  
  # Send mail via SMTP
  send_mail(
    mail_from = api_uid,
    mail_rcpt = api_uid, # change to email_data$contact_email[r] when ready
    message = message,
    smtp_server = "smtp://smtp.office365.com:587",
    username = api_uid,
    password = api_pwd,
    use_ssl = "force"
  )
}