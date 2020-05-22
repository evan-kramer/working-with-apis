# Source All QB/Smartsheet API Scripts
# Evan Kramer

# Attach packages
options(java.parameters = "-Xmx16G")
# library(tidyverse)

# Create function
source_all = function(dir, files, ...) {
  # Check if directory exists
  if(dir.exists(dir)) {
    # Run for each script
    for(f in files) {
      # Check that files is not empty
      if(length(files) > 0) {
        # Check that all files exist in specified directory
        if(f %in% list.files(dir)) {    
          # Run the script and print to console
          source(f, ...)  
        } else {
          message(str_c('"', f, '" not found in "', dir, '"'))
        }
      } else {
        message("No files specified")
      }
    }
  } else {
    message(str_c('Directory "', dir, '" not found'))
  }
}

# Call function
dir = "X:/Analysis Team/Development"
files = c()

source_all(dir, files)
