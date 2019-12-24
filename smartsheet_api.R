# Testing Smartsheet API
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(httr)
api_key = readRegistry("Environment", hive = "HCU")$smartsheet_api_key
api_pwd = readRegistry("Environment", hive = "HCU")$smartsheet_pwd
url = "https://app.smartsheet.com/sheets/Q6pPgXVCvM43R2X789x3JhXXGPrXCX83F47h9p71"

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