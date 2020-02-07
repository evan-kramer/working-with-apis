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
new_dr = left_join(
  # Get all new requests
  GET(
    str_c(
      str_replace(qb_url, "main", db$db_id[db$db_name == "OSSE_Data_Request_Portal__Requests"]), # database ID
      "?a", "=", "API_DoQuery", # call API_DoQuery function or API_GenResultsTable function
      "&query={'2'.OAF.", month(today() - 7), "/", day(today() - 7), "/", year(today() - 7), "}", # query | field ID, OAF = on or after, date format
      "AND{'23'.XCT.'Complete'}", # continue query, this appears to be doing nothing
      "&clist=a", # return all columns
      "&", "usertoken", "=", qb_api_key # use API user key to authenticate; users need to create in Quick Base and store as environment variable
    )
  ) %>% 
    content() %>% # parse XML content
    xmlToDataFrame( # turn into a data frame
      doc = ., # parsed XML content from above
      homogeneous = F, # F because not all fields are uniform,filled in
      nodes = getNodeSet(xmlParse(.), "//record"), # specify the particular nodes in the XML doc to add to the data frame
      stringsAsFactors = F
    ),
  # Join to statuses
  GET(
    str_c(
      str_replace(qb_url, "main", db$db_id[db$db_name == "OSSE_Data_Request_Portal__Status"]), # database ID
      "?a", "=", "API_DoQuery", # call API_DoQuery function or API_GenResultsTable function
      "&query={'0'.OAF.", month(today() - 7), "/", day(today() - 7), "/", year(today() - 7), "}", # query | field ID, OAF = on or after, date format
      # "AND{'23'.XCT.'Complete'}", # continue query, this appears to be doing nothing
      "&clist=a", # return all columns
      "&", "usertoken", "=", qb_api_key # use API user key to authenticate; users need to create in Quick Base and store as environment variable
    )
  ) %>% 
    content() %>% # parse XML content
    xmlToDataFrame( # turn into a data frame
      doc = ., # parsed XML content from above
      homogeneous = F, # F because not all fields are uniform,filled in
      nodes = getNodeSet(xmlParse(.), "//record"), # specify the particular nodes in the XML doc to add to the data frame
      stringsAsFactors = F
    ) %>% 
    group_by(request_id_) %>% 
    arrange(desc(as.numeric(date_modified))) %>% 
    summarize_at(
      vars(status, data_request_id, ),
      "first"
    ) %>% 
    ungroup(),
  by = c("record_id" = "request_id_")
) %>% 
  as_tibble() %>% # convert to tidy tibble 
  arrange(desc(as.numeric(record_id))) %>% 
  filter(
    !str_detect(case_status, "Complete")  # Don't add requests that are complete to the tracker
      
  ) # %>% transmute()
  
  

# Add hyperlink


# All requests that are still "New Request Submitted" 

# Requests past due


# convert variables to dates


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

# Make sure none of the records are already in the sheet

# Insert values below the last row

# Add hyperlink to data request ID 

# Add email status updates to this script? Email update to RL/SM?