# Data Request Memo
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
library(curl)
library(blastula)
api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
api_uid = readRegistry("Environment", hive = "HCU")$email_address
api_app = readRegistry("Environment", hive = "HCU")$quickbase_api_token
url = readRegistry("Environment", hive = "HCU")$quickbase_api_url

# Get DB table names and IDs from API
## Get a list of DBs I have access to 
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

con = dbConnect(
  odbc(), 
  "QuickBase via QuNect user",
  timeout = 10
)

# Query database 
memo_data = left_join(
  # Requests table
  dbGetQuery(
    con,
    str_c(
      "select *
    from ",
      db$db_name_id[str_ends(db$db_name, "__Requests")]
    )
  ) %>% 
    janitor::clean_names(),
  # Status table
  dbGetQuery(
    con,
    str_c(
      "select *
    from ",
      db$db_name_id[str_detect(db$db_name, "Status")]
    )
  ) %>% 
    janitor::clean_names() %>% 
    arrange(request_id, desc(date_modified)) %>% 
    group_by(request_id) %>% 
    summarize_at(vars(file_location:other_data_sharing_mechanism), "first") %>% 
    ungroup(),
  # Keys
  by = c("record_id" = "request_id")
) %>% 
  as_tibble() %>% 
  filter(case_status == "Pending PII Approval")

# Output memos
for(req in 1:nrow(memo_data)) {
# for(req in sort(memo_data$record_id)[1:3]) {  
  rmarkdown::render("C:/Users/evan.kramer/Documents/working-with-apis/data_request_memo.Rmd",
                    output_format = "word_document",
                    output_file = str_c("DR #", memo_data$record_id[req], " Approval Memo.docx"),
                    output_dir = "C:/Users/evan.kramer/Downloads/") # change to X:/ drive location
}

# Remove files we don't want pushed to GitHub
if(file.exists("C:/Users/evan.kramer/Documents/working-with-apis/data_request_memo.docx")) {
  file.remove("C:/Users/evan.kramer/Documents/working-with-apis/data_request_memo.docx")
}