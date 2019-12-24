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
api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
api_uid = readRegistry("Environment", hive = "HCU")$email_address
api_app = readRegistry("Environment", hive = "HCU")$quickbase_api_token
url = readRegistry("Environment", hive = "HCU")$quickbase_api_url

# Connect to database
con = dbConnect(
  odbc(), 
  "QuickBase via QuNect user",
  timeout = 10
)

# Query database 
requests = dbGetQuery(
  con,
  str_c(
    "select *
    from OSSE_Data_Request_Portal__Requests_bm6u3xrcx"
  )
) %>% 
  as_tibble()

status = dbGetQuery(
  con,
  str_c(
    "select *
    from OSSE_Data_Request_Portal__Status_bm6u3yxcu"
  )
) %>% 
  as_tibble()

break()



# Get DB table names and IDs? 
granted_dbs = GET(
  str_c(
    url,
    "?a", "=", "API_GrantedDBs", # API_GrantedDBs function,
    "&", "usertoken", "=", api_key # use API user key to authenticate (could also use ticket)
  )
) %>% 
  content()
db = tibble(
  db_name = xml_nodes(granted_dbs, "dbname") %>% 
    str_flatten() %>% 
    str_split("</dbname><dbname>") %>% 
    unlist(),
  db_id = xml_nodes(granted_dbs, "dbid") %>% 
    str_flatten() %>% 
    str_split("</dbid><dbid>") %>% 
    unlist() 
)

# Query the tables from the API
GET(
  str_c(
    str_replace(url, "main", "bm6u3xrcx"),
    "?a", "=", "API_DoQuery", # API_DoQuery function,
    "&", "query", "=", "{1.HAS.'a'}", # send query criteria
    # "&", "ticket", "=", auth_ticket, # use authentication ticket to prove who you are
    "&", "apptoken", "=", api_app,
    "&", "usertoken", "=", api_key # use API key instead?
  )
) %>% 
  content() %>% 
  View()

# Send email update
requester = api_uid
send_mail(
  mail_from = api_uid, # send from osse.datasharing@dc.gov?
  mail_rcpt = requester,
  message = str_c("This email was sent to ", requester, " as a test."), # format?
  smtp_server = NA, 
  username = NA,
  password = NA
)