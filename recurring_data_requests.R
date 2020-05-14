# Recurring Data Requests
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

# List of recurring data requests
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


# Data 
(data = left_join(
  # Get all new requests
  GET(
    str_c(
      str_replace(qb_url, "main", db$db_id[db$db_name == "OSSE_Data_Request_Portal__Requests"]), # database ID
      "?a", "=", "API_DoQuery", # call API_DoQuery function or API_GenResultsTable function
      # "&query={'2'.OAF.", month(today() - 7), "/", day(today() - 7), "/", year(today() - 7), "}", # query | field ID, OAF = on or after, date format
      "&query={'2'.OAF.", "12/31/", year(today()) - 4, "}", # query | field ID, OAF = on or after, date format
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
      # "&query={'0'.OAF.", month(today() - 7), "/", day(today() - 7), "/", year(today() - 7), "}", # query | field ID, OAF = on or after, date format
      "&query={'0'.OAF.", "12/31/", year(today()) - 4, "}",
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
  by = c("record_id" = "request_id_", "data_request_id")
) %>% 
    as_tibble() %>% # convert to tidy tibble 
    arrange(desc(as.numeric(record_id))))

# What is on the forecast? 
# What is the proportion of requests that require MOAs? 
filter(data, moa_status != "") %>% 
count(moa_status, sort = T) %>% 
  mutate(pct = round(100 * n / sum(n, na.rm = T), 1))

# What proportion of requests are internal?
count(data, requesting_organization_final, sort = T) %>% 
  mutate(pct = round(100 * n / sum(n, na.rm = T), 1)) %>% 
  write_csv("C:/Users/evan.kramer/Downloads/requests_by_organization.csv")


(data = left_join(
  # Get all new requests
  GET(
    str_c(
      str_replace(qb_url, "main", db$db_id[db$db_name == "OSSE_Data_Request_Portal__Requests"]), # database ID
      "?a", "=", "API_DoQuery", # call API_DoQuery function or API_GenResultsTable function
      # "&query={'2'.OAF.", month(today() - 7), "/", day(today() - 7), "/", year(today() - 7), "}", # query | field ID, OAF = on or after, date format
      "&query={'2'.OAF.", "12/31/", year(today()) - 4, "}", # query | field ID, OAF = on or after, date format
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
      # "&query={'0'.OAF.", month(today() - 7), "/", day(today() - 7), "/", year(today() - 7), "}", # query | field ID, OAF = on or after, date format
      "&query={'0'.OAF.", "12/31/", year(today()) - 4, "}",
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
  by = c("record_id" = "request_id_", "data_request_id")
) %>% 
  as_tibble() %>% # convert to tidy tibble 
  arrange(desc(as.numeric(record_id))) %>% 
  filter(
    (frequency_of_data_request != "" | str_detect(is_this_a_recurring_request_, "Yes")) &
      !str_detect(str_to_lower(status), "closed")
  ) %>% 
    # Figure out which columns to keep? 
    transmute(
      school_year = data_request_school_year__date_created,
      record_id,
      data_request_id,
      # title = title_of_data_request,
      requester_full_name,
      contact_email,
      requesting_organization_final,
      recurring_request = is_this_a_recurring_request_,
      # frequency = if_yes__please_identify_the_frequency,
      frequency = frequency_of_data_request,
      purpose = purpose_of_data_request,
      info = data_request_information__please_summarize_what_you_are_looking_for_in_this_data_request_,
      data_elements,
      special_education,
      attendance,
      attendance_data_elements,
      other_attendance_data,
      enrollment,
      enrollment_data_elements,
      other_enrollment_data,
      other_data_elements,
      demographics,
      demographic_data_elements,
      other_demographics_data,
      other_data = specify_other_data_elements_required,
      status,
      url = str_c("https://octo.quickbase.com/db/bm6u3xrcx?a=dr&rid=", record_id, "&rl=eka")
    ))

# Output file
# write_csv(data, "C:/Users/evan.kramer/Downloads/recurring_data_requests.csv", na = "")
