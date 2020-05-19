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

# Sandbox
GET(
  str_c(
    str_replace(qb_url, "main", db$db_id[db$db_name == "OSSE_Data_Request_Portal__Requests"]), # database ID
    "?a", "=", "API_DoQuery", # call API_DoQuery function or API_GenResultsTable function
    "&query={'2'.OAF.'Tue, 18 May 2020 19:45:24 GMT'}",
    # "&query={'10'.CT.'Urban'}", # query | field ID, OAF = on or after, date format
    # "&clist=a", # return all columns
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




# Data requests within the last X days
for(d in 1:10) {
  print(str_c("Number of data requests in the past ", d, " days..."))
  GET(
    str_c(
      str_replace(qb_url, "main", db$db_id[db$db_name == "OSSE_Data_Request_Portal__Requests"]), # database ID
      "?a", "=", "API_DoQueryCount", # call API_DoQuery function or API_GenResultsTable function
      "&query={'2'.OAF.", month(now()), "/", day(now()) - d, "/", year(now()), "}", # query | field ID, OAF = on or after, date format
      "AND{'23'.XCT.'Complete'}", # continue query, this appears to be doing nothing
      # "&clist=a", # return all columns
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
    as_tibble() %>% 
    print()
} 

# Query the API for all new data requests
new_dr = left_join(
  # Get all new requests
  GET(
    str_c(
      str_replace(qb_url, "main", db$db_id[db$db_name == "OSSE_Data_Request_Portal__Requests"]), # database ID
      "?a", "=", "API_DoQuery", # call API_DoQuery function or API_GenResultsTable function
      # "&query={'2'.OAF.", month(today() - 7), "/", day(today() - 7), "/", year(today() - 7), "}", # query | field ID, OAF = on or after, date format
      "&query={'2'.OAF.", "12/31/", year(today()) - 1, "}", # query | field ID, OAF = on or after, date format
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
      vars(status, data_request_id),
      "first"
    ) %>% 
    ungroup(),
  by = c("record_id" = "request_id_")
) %>% 
  as_tibble() %>% # convert to tidy tibble 
  arrange(desc(as.numeric(record_id))) %>% 
  filter(
    !case_status %in% c("Closed", "Complete")  # Don't add requests that are complete to the tracker
    # case_status %in% c("Closed", "Complete")
  ) 

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