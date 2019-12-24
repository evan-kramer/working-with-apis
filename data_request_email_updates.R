# Testing Smartsheet API
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(httr)
library(xml2)
library(rvest)
api_key = readRegistry("Environment", hive = "HCU")$quickbase_api_key
api_pwd = readRegistry("Environment", hive = "HCU")$quickbase_pwd
api_uid = readRegistry("Environment", hive = "HCU")$email_address
url = readRegistry("Environment", hive = "HCU")$quickbase_api_url

# Authenticate
auth = GET(
  str_c(
    url, # Main url to access the data
    "?a", "=", "API_Authenticate", # function to use, API_Authenticate here
    "&", "username", "=", api_uid, # user ID
    "&", "password", "=", api_pwd, # password
    "&", "hours", "=", 24 # string to reference hours of authentication
  )
) %>%
  content(as = "text")
auth_ticket = str_sub(
  auth, 
  str_locate(auth, "<ticket>")[2] + 1, 
  str_locate(auth, "</ticket>")[1] - 1
)

# Get DB names? 
GET(
  str_c(
    url,
    "?a", "=", "API_GrantedDBs", # API_GrantedDBs function,
    "&", "ticket", "=", auth_ticket # use authentication ticket to prove who you are
    # "&", "usertoken", "=", api_key, # use API key instead? 
  )
) %>% 
  content() %>% 
  # xml_attrs("database")
  # xml_nodes("dbname") %>% 
  xml_nodes("dbid") %>% 
  .[61:80]

# Get field attributes

# Query the tables
GET(
  str_c(
    str_replace(url, "main", "bm6u3xrcx"),
    "?a", "=", "API_DoQuery", # API_DoQuery function,
    "&", "query", "=", "bm6u3xrcx", # Get DB ID using calls above
    "&", "ticket", "=", auth_ticket # use authentication ticket to prove who you are
    # "&", "usertoken", "=", api_key, # use API key instead? 
  )
)


break()

# Get all sheets
GET(
  url = "https://octo.quickbase.com/db/bm6u3xrcx?a=q&qid=16",
  authenticate(
    user = "evan.kramer@dc.gov",
    password = api_pwd
  ),
  add_headers(
    Authorization = str_c("Bearer ", api_key)
  )
) %>% 
  class()
  content() 

# Get data from a table



# Send email
requester = api_uid
send_mail(
  mail_from = api_uid, # send from osse.datasharing@dc.gov?
  mail_rcpt = requester,
  message = str_c("This email was sent to ", requester, " as a test."), # format?
  smtp_server = NA, 
  username = NA,
  password = NA
)

# DR 