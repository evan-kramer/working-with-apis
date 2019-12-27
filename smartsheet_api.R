# Testing Smartsheet API
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(httr)
api_key = readRegistry("Environment", hive = "HCU")$smartsheet_api_key
api_pwd = readRegistry("Environment", hive = "HCU")$smartsheet_pwd



# Can I send a list of new data requests to Smartsheet Front Office review sheet? 
con = dbConnect(
  odbc(), 
  "QuickBase via QuNect user",
  timeout = 10
)



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

# Get all sheets
GET(
  url = "https://api.smartsheet.com/2.0/sheets",
  authenticate(
    user = "evan.kramer@dc.gov",
    password = api_pwd
  ),
  add_headers(
    Authorization = str_c("Bearer ", api_key)
  )
) %>% 
  content() 

# Get data from a sheet

# DR 