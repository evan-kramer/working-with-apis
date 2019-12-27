# Testing Smartsheet API
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(httr)
library(rjson)
ss_api_key = readRegistry("Environment", hive = "HCU")$smartsheet_api_key
ss_api_pwd = readRegistry("Environment", hive = "HCU")$smartsheet_pwd
qb_api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
qb_api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
qb_api_app = readRegistry("Environment", hive = "HCU")$quickbase_api_token
qb_url = readRegistry("Environment", hive = "HCU")$quickbase_api_url
api_uid = readRegistry("Environment", hive = "HCU")$email_address

# Send a list of new data requests to Smartsheet Front Office review sheet? 
# Connect to Quick Base API
con = dbConnect(
  odbc(), 
  "QuickBase via QuNect user",
  timeout = 10
)

# See which databases I have access to 
granted_dbs = GET(
  str_c(
    qb_url,
    "?a", "=", "API_GrantedDBs", # API_GrantedDBs function,
    "&", "usertoken", "=", qb_api_key # use API user key to authenticate (could also use ticket)
  )
) %>% 
  content()

# Create a tibble and extract names and IDs from API
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

# Get all new requests from QB
dbGetQuery(
  con,
  str_c(
    "select *
    from ", db$db_name_id[str_ends(db$db_name, "__Requests")]
  )
) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  filter(date_created >= today() - ddays(7))

# Get all sheets
# Why is it only showing 100 sheets? How can we fix that? 
GET(
  url = "https://api.smartsheet.com/2.0/sheets",
  authenticate(
    user = api_uid,
    password = ss_api_pwd
  ),
  add_headers(
    Authorization = str_c("Bearer ", ss_api_key)
  )
) %>% 
  content(as = "parsed", type = "application/json") %>% 
  as_tibble() %>% 
  unnest_wider(col = data) 

# Get information from front office sheet
GET(
  url = "https://api.smartsheet.com/2.0/sheets/6420298896566148",
  authenticate(
    user = api_uid,
    password = ss_api_pwd
  ),
  add_headers(
    Authorization = str_c("Bearer ", ss_api_key)
  )
) %>% 
  content(as = "parsed", type = "application/json") %>% 
  as_tibble() %>% 
  unnest_wider(col = data) 