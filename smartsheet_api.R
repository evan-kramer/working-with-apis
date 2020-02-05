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
ss_api_key = readRegistry("Environment", hive = "HCU")$smartsheet_api_key
ss_api_pwd = readRegistry("Environment", hive = "HCU")$smartsheet_pwd
qb_api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
qb_api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
qb_api_app = readRegistry("Environment", hive = "HCU")$quickbase_api_token
qb_url = readRegistry("Environment", hive = "HCU")$quickbase_api_url
api_uid = readRegistry("Environment", hive = "HCU")$email_address

# Send a list of new data requests to Smartsheet Front Office review sheet? 
# Get DBs to which I have access
granted_dbs = GET(
  str_c(
    qb_url,
    "?a", "=", "API_GrantedDBs", # API_GrantedDBs function,
    "&", "usertoken", "=", qb_api_key # use API user key to authenticate (could also use ticket)
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
) %>% 
  # Concatenate to match QuNect naming conventions
  mutate(
    db_name = str_replace_all(db_name, "</dbname>", "") %>% 
      str_replace_all("<dbname>", "") %>% 
      str_replace_all("[ :]", "_"),
    db_id = str_replace_all(db_id, "</dbid>", "") %>% 
      str_replace_all("<dbid>", ""),
    db_name_id = str_c(db_name, db_id, sep  = "_")
  ) 

# Query the API for all new data requests
new_dr = GET(
  str_c(
    str_replace(qb_url, "main", db$db_id[db$db_name == "OSSE_Data_Request_Portal__Requests"]), # database ID
    "?a", "=", "API_DoQuery", # call API_DoQuery function or API_GenResultsTable function
    "&query={'0'.OAF.", month(today() - 7), "/", day(today() - 7), "/", year(today() - 7), "}", # query | field ID, OAF = on or after, date format
    "&clist=a", # return all columns
    "&", "usertoken", "=", qb_api_key # use API user key to authenticate; users need to create in Quick Base and store as environment variable
  )
) %>% 
  content()

xml_find_all(new_dr, "//record") %>% 
  xml_text() %>% 
  trimws()

## use xml_nodes
tibble(
  db_name = xml_nodes(granted_dbs, "record") %>% 
    str_flatten() %>% 
    str_split("</dbname><dbname>") %>% 
    unlist(),
  db_id = xml_nodes(granted_dbs, "dbid") %>% 
    str_flatten() %>% 
    str_split("</dbid><dbid>") %>% 
    unlist() 
)






# Find last row in Smartsheet
dr_review = GET(
  url = "https://api.smartsheet.com/2.0/sheets/6420298896566148",
  authenticate(
    user = api_uid,
    password = ss_api_pwd
  ),
  add_headers(
    Authorization = str_c("Bearer ", ss_api_key)
  )
) %>% 
  content(as = "parsed", type = "application/json") 
dr_review$totalRowCount






