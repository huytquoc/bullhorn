### ++++++ Loading credentials Information

if(!file.exists("DataPull/getBHAuth.R")){
  stop("Stop! Missing getBHAuth.R file.")
} else source("DataPull/getBHAuth.R")

###############################################
### Global variables loaded:
### clientID
### clientSecret
### bHToken: for retrieving data from REST API
###############################################
### ===== Loading credentials Information


###############################################
## Get Job Orders from Date range

getJOByDateRange <- function(from,to)
{
  # Compose Query string
  q <- character()
  q <- c("dateAdded:[",as.character(from),"TO",as.character(to),"]")
  q <- paste(q, collapse=" ")
  
  #### Read all Job Orders
  itemStart <- 0
  paging <- 100
  
  listJO <- list()
  
  repeat 
  {
    r3 <- GET(br, 
              path = "rest-services/4bk1/search/JobOrder",
              add_headers(BhRestToken = token),
              query = list(query = q, fields="id", start= itemStart, count=paging))
    
    
    if (status_code(r3)!= 200)
    {
      token <- bTokenRenew(bullhorn_token)
      r3 <- GET(br, 
                path = "rest-services/4bk1/search/JobOrder",
                add_headers(BhRestToken = token),
                query = list(query = q, fields="id", start= itemStart, count=paging))
    }
    
    d3 <- content(r3)
    
    
    # Read Job Order id to list
    
    numJOB <- d3$count
    for (i in 1:numJOB)
    {
      listJO[[length(listJO)+1]] <- d3$data[[i]]$id
    }
    
    # increase counting 
    itemStart <- itemStart + paging
    
    # exiting condition
    if(itemStart > d3$total) break
    
  }
  
  listJO
}

## =====================================
## Get Job Orders from Date range
