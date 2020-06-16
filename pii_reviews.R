# PII Reviews
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
installed_packages = as.data.frame(installed.packages())
required_packages = c("tidyverse", "lubridate", "httr", "rjson", "odbc", "rvest", "xml2",
                      "XML", "jsonlite", "data.table")
for(p in required_packages) {
  if(!p %in% installed_packages$Package) {
    install.packages(p)
  }
  library(p, character.only = T)
}
rm(list = ls(pattern = "_packages"))

# Download R: https://mirrors.nics.utk.edu/cran/
# Download RStudio: https://rstudio.com/products/rstudio/download/#download
# Create a Smartsheet API key: https://www.loom.com/share/0cba648be36c401d9f55429d10433f5a
# Create a QB API key: Account > My Preferences > Manage user tokens for OCTO realm... > + New user token > Name > Assign to DRT and DRMOA > Copy key
# Store in local environment variables: Start menu > Edit system environment variables > Environment variables > New > Give name and value
# You'll have to edit here when you update your QB and SS passwords

# Credentials
ss_api_key = readRegistry("Environment", hive = "HCU")$smartsheet_api_key
ss_api_pwd = readRegistry("Environment", hive = "HCU")$smartsheet_pwd
qb_api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
qb_api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
qb_api_app = readRegistry("Environment", hive = "HCU")$quickbase_api_token
qb_url = readRegistry("Environment", hive = "HCU")$quickbase_api_url
api_uid = readRegistry("Environment", hive = "HCU")$email_address

# Get DBs to which the user has access
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

# Query database for all requests in Pending PII review status
pii_reviews = GET(
  str_c(
    str_replace(qb_url, "main", dbs$dbid[str_detect(dbs$dbname, "Status")]), # database ID
    "?a", "=", "API_DoQuery", # call API_DoQuery function
    "&query={'2'.IR.'this+y'}", # All requests in the last X days
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
  as_tibble() %>% 
  arrange(desc(data_request_id), desc(as.numeric(date_modified))) %>% 
  group_by(data_request_id) %>% 
  summarize(
    date_requested = min(as.POSIXct(as.numeric(date_created) / 1000, origin = "1970-01-01")),
    requestor = first(requesting_organization_final),
    topic_area = first(topic_area),
    type_of_data = first(level_of_data),
    dsa_in_place = first(request___moa_status),
    date_submitted_for_approval = min(as.POSIXct(as.numeric(date_date_submitted_for_pending_pii_approval) / 1000, origin = "1970-01-01")),
    analyst = first(data_request_analyst),
    qa = first(data_request_qa),
    data_location = first(file_location),
    data_sharing_mechanism = first(data_sharing_mechanism),
    status = first(status)
  ) %>% 
  filter(str_detect(status, "Pending PII")) %>% 
  mutate(
    dsa_in_place = case_when(
      dsa_in_place == "MoA not required" ~ "Not needed",
      str_detect(dsa_in_place, "on file") ~ "Y",
      str_detect(dsa_in_place, "not required") ~ "Not needed",
    ),
    type_of_data = case_when(
      str_detect(type_of_data, "-sup") & str_detect(type_of_data, "-unsup") ~ "Aggregate suppressed and unsuppressed",
      str_detect(type_of_data, "-sup") ~ "Aggregate suppressed",
      str_detect(type_of_data, "-unsup") ~ "Aggregate unsuppressed",
      str_detect(type_of_data, "Indiv") & str_detect(type_of_data, "Agg") ~ "Individual-level and aggregate",
      str_detect(type_of_data, "Indiv") ~ "Individual-level",
      str_detect(type_of_data, "record") ~ "Student education record"
    ),
    date_requested = as.character(date_requested + hours(6)) %>% 
      str_replace_all(" ", "T") %>% 
      str_c("Z"),
    date_submitted_for_approval = as.character(now() + hours(6)) %>% 
      str_replace_all(" ", "T") %>% 
      str_c("Z")
  )

# Connect to Smartsheet and pull sheet metadata
sheet_id = 385865148065668
drs = GET(
  url = str_c("https://api.smartsheet.com/2.0/sheets/", sheet_id),
  authenticate(
    user = api_uid,
    password = ss_api_pwd
  ),
  add_headers(
    Authorization = str_c("Bearer ", ss_api_key)
  )
) %>% 
  content(as = "parsed", type = "application/json") 

column_ids = tibble(
  col_name = rep(NA_character_, length(drs$columns)),
  col_id = rep(NA_character_, length(drs$columns))
)
for(r in 1:length(drs$columns)) {
  column_ids$col_name[r] = drs$columns[[r]]$title
  column_ids$col_id[r] = as.character(drs$columns[[r]]$id)
}

# Get a list of all the DR IDs in there
dr_list = tibble(data_request_id = rep(NA_character_, drs$totalRowCount))
for(r in 1:drs$totalRowCount) {
  if(is.null(drs$rows[[r]]$cells[[1]]$value)) {
    dr_list$data_request_id[r] = NA_character_
  } else {
    dr_list$data_request_id[r] = drs$rows[[r]]$cells[[1]]$value
  }
}

# Add rows
for(r in 1:nrow(pii_reviews)) {
  # Make sure the data request ID is not already in there
  if(!pii_reviews$data_request_id[r] %in% dr_list$data_request_id) {
    # Define dictionary 
    body = 
      '[{
        "toTop": true, 
        "cells":
          [
            {"columnId": 632689700169604, "value": "data_request_id"},
            {"columnId": 2884489513854852, "value": "date_requested"},
            {"columnId": 7388089141225348, "value": "requestor"},
            {"columnId": 1758589607012228, "value": "topic_area"},
            {"columnId": 6262189234382724, "value": "type_of_data"},
            {"columnId": 2919316917118852, "value": "dsa_in_place"},
            {"columnId": 4010389420697476, "value": "date_submitted_for_approval"},
            {"columnId": 8513989048067972, "value": "analyst"},
            {"columnId": 351214723458948 , "value": "qa"}
          ]
      }]'
    
    # Loop through all columns and replace in body
    for(n in names(pii_reviews)) {
      if(!n %in% c("data_location", "status")) {
        body = str_replace_all(body, n, as.character(pii_reviews[r, n]))
      }
    }
    
    # POST new rows
    POST(
      url = str_c("https://api.smartsheet.com/2.0/sheets/", sheet_id, "/rows"),
      authenticate(user = api_uid, password = ss_api_pwd),
      add_headers(Authorization = str_c("Bearer ", ss_api_key)),
      # add_headers(Content-Type = "application/json"),
      encode = "json", 
      body = body
    ) 
  }
}
