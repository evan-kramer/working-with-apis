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
ss_api_key = readRegistry("Environment", hive = "HCU")$smartsheet_api_key
ss_api_pwd = readRegistry("Environment", hive = "HCU")$smartsheet_pwd
qb_api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
qb_api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
qb_api_app = readRegistry("Environment", hive = "HCU")$quickbase_api_token
qb_url = readRegistry("Environment", hive = "HCU")$quickbase_api_url
api_uid = readRegistry("Environment", hive = "HCU")$email_address

# Get DBs to which I have access
dbs = GET(
  str_c(
    qb_url,
    "?a", "=", "API_GrantedDBs", # API_GrantedDBs function,
    "&", "usertoken", "=", qb_api_key # use API user key to authenticate (could also use ticket)
  )
) %>% 
  content() %>% 
  xmlToDataFrame( # turn into a data frame
    doc = ., # parsed XML content from above
    homogeneous = F, # F because not all fields are uniform,filled in
    nodes = getNodeSet(xmlParse(.), "//dbinfo"), # specify the particular nodes in the XML doc to add to the data frame
    stringsAsFactors = F
  ) %>% 
  as_tibble() %>% 
  mutate(numRecords = NA_integer_, lastRecModTime = NA_integer_)
  
# Get info for all databases
for(d in dbs$dbid) {
  # Call API (API_GetDBInfo)
  response = GET(
    str_c(
      str_replace(qb_url, "main", d), # database ID
      "?a", "=", "API_GetDBInfo", 
      "&", "usertoken", "=", qb_api_key
    )
  ) %>% 
    read_xml() 
  # Replace missing values in tibble above
  for(d2 in c("numRecords", "lastRecModTime")) {
    dbs[dbs$dbid == d, d2] = xml_find_all(response, str_c("//", d2)) %>% 
      xml_contents() %>% 
      as.character() %>% 
      as.numeric() 
  }
}

# Clean up dates
dbs = mutate(dbs, lastRecModTime = as.POSIXct(lastRecModTime / 1000, origin = "1970-01-01"))

# Query database for all requests in the last X days
GET(
  str_c(
    "?a", "=", "API_DoQuery", # call API_DoQuery function or API_GenResultsTable function
    "&query={'2'.OAF.", as.numeric(now() - days(1)), "}",
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
  as_tibble()

# Connect to Smartsheet and pull sheet metadata
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

# Find last row and column in Smartsheet
last_row = dr_review$totalRowCount
last_col = length(dr_review$columns)

# Get column information
col_data = row_names = tibble()
for(i in 1:last_col) {
  col_data = tibble(
    col_name = unlist(dr_review$columns[[i]]["title"]),
    col_id = unlist(dr_review$columns[[i]]["id"])
  ) %>% 
  bind_rows(col_data, .)
}

# Get a list of all DR IDs currently in the sheet
dr_review$rows[[last_row]]$id
dr_review$rows[[last_row]]$rowNumber
dr_review$rows[[last_row]]$parentId
dr_review$rows[[last_row]]$siblingId
dr_review$rows[[last_row]]$expanded
dr_review$rows[[last_row]]$createdAt
dr_review$rows[[last_row]]$cells

# dr_review$rows[[last_row]]$cells[[j]]

for(i in 1:last_row) {
  if(length(dr_review$rows[[i]]$cells) == last_col) {
    for(j in 1:last_col) {
      print(i)
      print(j)
      print(dr_review$rows[[i]]$cells[[j]]['columnId'])
      print(dr_review$rows[[i]]$cells[[j]]['value'])
      print(dr_review$rows[[i]]$cells[[j]]['displayValue'])
    }
  }
}



# Sandbox
data = tibble()
names(tibble) = col_data$col_name


row_data = tibble()
for(i in 1:last_row) {
  if(length(dr_review$rows[[i]]$cells) == last_col) {
    for(j in 1:last_col) {
      row_data = tibble(
        id = ifelse(is.null(dr_review$rows[[i]]$id), NA, dr_review$rows[[i]]$id),
        row_number = ifelse(is.null(dr_review$rows[[i]]$rowNumber), NA, dr_review$rows[[i]]$rowNumber),
        parent_id = ifelse(is.null(dr_review$rows[[i]]$parentId), NA, dr_review$rows[[i]]$parentId),
        sibling_id = ifelse(is.null(dr_review$rows[[i]]$siblingId), NA, dr_review$rows[[i]]$siblingId),
        expanded = ifelse(is.null(dr_review$rows[[i]]$expanded), NA, dr_review$rows[[i]]$expanded),
        created_at = ifelse(is.null(dr_review$rows[[i]]$createdAt), NA, dr_review$rows[[i]]$createdAt),
        col_id = ifelse(is.null(dr_review$rows[[i]]$cells[[j]]$columnId), NA, dr_review$rows[[i]]$cells[[j]]$columnId),
        value = ifelse(is.null(dr_review$rows[[i]]$cells[[j]]$value), NA, dr_review$rows[[i]]$cells[[j]]$value),
        display_value = ifelse(is.null(dr_review$rows[[i]]$cells[[j]]$displayValue), NA, dr_review$rows[[i]]$cells[[j]]$displayValue)
      ) %>%
        bind_rows(row_data, .)
    }
  }
}
row_data



for(i in 1:last_row) {
  if(length(dr_review$rows[[i]]$cells) == last_col) {
    for(j in 1:last_col) {
      row_data = tibble(
        col_id = unlist(dr_review$rows[[i]]$cells[[j]]["columnId"]),
        value = unlist(dr_review$rows[[i]]$cells[[j]]["value"]),
        display_value = unlist(dr_review$rows[[i]]$cells[[j]]["displayValue"]),
      ) %>% 
        bind_rows(row_data, .)
    }
  }
}

# Sandbox
for(i in 1:last_row) {
  if(length(dr_review$rows[[i]][["cells"]]) == last_col) {
    for(i in 1:last_col) {
      print(dr_review$rows[[i]][["cells"]][i])
    }
  }
}



# Test to see what is already there
GET(
  url = str_c("https://api.smartsheet.com/2.0/sheets/6420298896566148/rows", ),
  authenticate(
    user = api_uid,
    password = ss_api_pwd
  ),
  add_headers(
    Authorization = str_c("Bearer ", ss_api_key)
  )
) %>% 
  content(as = "parsed", type = "application/json") 

# Determine whether the DR IDs are not already in the sheet
# Add hyperlink
# Follow up on all requests that are still "New Request Submitted" 
# Follow up on requests that are past due?
# Convert variables to dates
# Make sure none of the records are already in the sheet
# Insert values below the last row
# Add hyperlink to data request ID 
# Add email status updates to this script? Email update to RL/SM? from data_request_email_updates