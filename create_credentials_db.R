# Init DB using credentials data
# This needs to be run to recreate user credential db

source("cred_users.R")

credentials <- data.frame(
  user = c("cameratester", "cameraadmin"),
  password = c(cameratester_pw, cameraadmin_pw),
  # passwords will automatically be hashed
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

library(shiny)
library(shinymanager)
# Currently not setting this in ~/Documents/.Renviron 
# Sys.setenv(R_shinymanager_key="tgwdnpo400C!")

# Init the database
create_db(
  credentials_data = credentials,
  sqlite_path = "database.sqlite", # will be created
  # passphrase = Sys.getenv("R_shinymanager_key")
  passphrase = "tgwdnpo400C!"
)

rm(passphrase, cameratester_pw, cameraadmin_pw)
