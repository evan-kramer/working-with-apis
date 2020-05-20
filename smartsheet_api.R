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
for(d in 10:1) {
  new_requests = GET(
    str_c(
      str_replace(qb_url, "main", "bm6u3xrcx"), # database ID
      "?a", "=", "API_DoQuery", # call API_DoQuery function
      "&query={'2'.IR.'last+", d, "+d'}", # All requests in the last X days
      # "&query={'2'.IR.yesterday}",
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
} 

# Connect to Smartsheet and pull sheet metadata
drs = GET(
  # url = "https://api.smartsheet.com/2.0/sheets/1774666982418308", # File > Properties > Sheet ID
  url = "https://api.smartsheet.com/2.0/sheets/1998748848023428", # Sandbox
  authenticate(
    user = api_uid,
    password = ss_api_pwd
  ),
  add_headers(
    Authorization = str_c("Bearer ", ss_api_key)
  )
) %>% 
  content(as = "parsed", type = "application/json") 

# Will need to unnest significantly
# Try adding a new example row
body = 
  '[{
    "toBottom": true, 
    "cells":
      [
        {"columnId": 434763312981892, "value": "Example DR #"},
        {"columnId": 8597537637590916, "value": "Example Work Stream"},
        {"columnId": 4282710412814212, "value": "https://google.com"}
      ]
  }]'

# Loop through all new requests
# Add parent row
for(r in 1:nrow(new_requests)) {
  # Add parent row
  parent_post = POST(
    url = "https://api.smartsheet.com/2.0/sheets/1998748848023428/rows", # Sandbox,
    authenticate(user = api_uid, password = ss_api_pwd),
    add_headers(Authorization = str_c("Bearer ", ss_api_key)),
    # add_headers(Content-Type = "application/json"),
    encode = "json", 
    body = str_replace_all(
      body, 
      "Example DR #", 
      str_c("DR #", new_requests$data_request_id[r])
    ) 
  )
  
  # Parent response
  parent_response = tibble(
    id = content(parent_post)$result[[1]]$id,
    sheetId = content(parent_post)$result[[1]]$sheetId,
    rowNumber = content(parent_post)$result[[1]]$rowNumber,
    siblingId = content(parent_post)$result[[1]]$siblingId
  )
  
  # Child rows
  for(t in c("Request received", "Draft specifications according to business rules",
             "QA", "Finalize analysis and complete memo and business rules", 
             "Manager review and submit for approval", "Send data to requester")) {
    # Add child rows
    child_post = POST(
      url = "https://api.smartsheet.com/2.0/sheets/1998748848023428/rows", # Sandbox,
      authenticate(user = api_uid, password = ss_api_pwd),
      add_headers(Authorization = str_c("Bearer ", ss_api_key)),
      # add_headers(Content-Type = "application/json"),
      encode = "json", 
      body = str_replace_all(
        body, 
        "Example DR #", 
        t
      )
    )
    
    # Child response
    child_response = tibble(
      id = content(child_post)$result[[1]]$id,
      sheetId = content(child_post)$result[[1]]$sheetId,
      rowNumber = content(child_post)$result[[1]]$rowNumber,
      siblingId = content(child_post)$result[[1]]$siblingId
    )
    
    # Indent child rows
    PUT(
      url = "https://api.smartsheet.com/2.0/sheets/1998748848023428/rows", # Sandbox,
      authenticate(user = api_uid, password = ss_api_pwd),
      add_headers(Authorization = str_c("Bearer ", ss_api_key)),
      # add_headers(Content-Type = "application/json"),
      encode = "json", 
      body = str_c(
        '[{
            "indent":1,
            "id": ', child_response$id, ' 
        }]'
      )
    )
  }
}

break

# Find last row and column in Smartsheet
drs$totalRowCount == length(drs$rows)
length(drs$columns)
drs$columns
drs$rows

# Get column information
col_data = row_names = tibble()
for(i in 1:length(drs$columns)) {
  col_data = tibble(
    col_name = unlist(drs$columns[[i]]["title"]),
    col_id = unlist(drs$columns[[i]]["id"])
  ) %>% 
  bind_rows(col_data, .)
}

# Get row information
row_data = tibble()
for(i in 1:length(drs$rows)) {
  row_data = tibble(
    row_id = unlist(drs$rows[[i]]["id"]),
    row_value = unlist(drs$rows[[i]]["value"])
  ) %>% 
    bind_row(row_data, .)
}

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