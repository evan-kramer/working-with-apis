# Testing Smartsheet API
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

message = str_c(
  'From: "OSSE Data Request Portal" <', api_uid, '>
  To: "Recipient" <', requester, '>
  Subject: Data Request #', email_data$request_id[1], '
  
  Hi ', email_data$first_name[1], '
  
  I am writing with an update on data request #', email_data$request_id[1], '. The status of this request is listed as ', email_data$status[1],', and it was last updated on, ', email_data$date_modified[1], '.
  
  Please reach out with any questions or feedback.
  
  Regards,
  Evan'
)

send_mail(
  mail_from = api_uid, # send from osse.datasharing@dc.gov?
  mail_rcpt = requester,
  message = message,
  smtp_server = "smtp://smtp.office365.com:587",
  username = api_uid,
  password = api_pwd,
  use_ssl = T,
  verbose = F # whether you want to display output
)

## Can I send a list of new data requests to Smartsheet Front Office review sheet? 
