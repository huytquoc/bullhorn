### ++++++ Loading credentials Information
if(!file.exists("DataPull/BHCredentials.R")){
  stop("Stop! The Credentials Info must be provided.")
} else source("DataPull/BHCredentials.R")

###############################################
### Global variables loaded:
### clientID
### clientSecret
###############################################
### ===== Loading credentials Information


### ++++++ Loading all neccessary Libraries 
if(!require("jsonlite")){
  install.packages("jsonlite")
}
library("jsonlite")

if(!require("httr")){
  install.packages("httr")
}
library("httr")

### ====== Loading all neccessary Libraries 


### +++++++++ Get Authentication

bTokenRenew <- function(bullhorn_token) 
{
  bullhorn_token$refresh()
  req <- GET("https://rest.bullhornstaffing.com/rest-services/login?version=2.0", query = list(access_token = bullhorn_token$credentials$access_token))        
  content(req)$BhRestToken
}

getAuth <- function(clientID, clientSecret)
{
  bullhorn_endpoint <- oauth_endpoint(
    authorize = "https://auth.bullhornstaffing.com/oauth/authorize",
    access = "https://auth.bullhornstaffing.com/oauth/token")
  
  mybullhorn <- oauth_app("bullhorn", 
                          key = clientID, # Client ID
                          secret = clientSecret) # Client Secret
  
  bullhorn_token <- oauth2.0_token(bullhorn_endpoint, mybullhorn, scope = NULL, type = NULL,
                                   use_oob = getOption("httr_oob_default"), as_header = TRUE,
                                   cache = getOption("httr_oauth_cache"))
  bullhorn_token
}

getToken <- function(bullhorn_token)
{
  req <- GET("https://rest.bullhornstaffing.com/rest-services/login?version=2.0", query = list(access_token = bullhorn_token$credentials$access_token))
  
  if (status_code(req) != 200) {
    bTokenRenew(bullhorn_token)
  } else {
    content(req)$BhRestToken
  }
}

### ============ Get Authentication

### ++++++ BEGIN ###
# Initiate
bHAuToken <- getAuth(clientID, clientSecret)
bHToken <- getToken(bHAuToken)


### ====== END ###
