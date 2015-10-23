if(!require("jsonlite")){
  install.packages("jsonlite")
}
library("jsonlite")

if(!require("httr")){
  install.packages("httr")
}
library("httr")

if(!require("data.table")){
  install.packages("data.table")
}
library("data.table")

if(!require("xlsx")){
  install.packages("xlsx")
}
library("xlsx")


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
                          add_headers(BhRestToken = bHToken),
                          query = list(query = q, fields="id", start= itemStart, count=paging))
                
       
                if (status_code(r3)!= 200)
                {
                        bHToken <- bTokenRenew(bHAuToken)
                        r3 <- GET(br, 
                                  path = "rest-services/4bk1/search/JobOrder",
                                  add_headers(BhRestToken = bHToken),
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

getJOInfo <- function(jId){
        
        q <- paste(c("id:",as.character(jId)),collapse=" ")
        
        r1 <- GET(br, 
                  path = "rest-services/4bk1/search/JobOrder",
                  add_headers(BhRestToken = bHToken),
                  query = list(query = q, fields="id, dateAdded, dateClosed, skills, owner, clientCorporation, address, title, status, submissions, sendouts, interviews, placements, approvedPlacements"))
        
        if(status_code(r1)!=200) 
        {
                bHToken <- bTokenRenew(bHAuToken)
                r1 <- GET(br, 
                          path = "rest-services/4bk1/search/JobOrder",
                          add_headers(BhRestToken = bHToken),
                          query = list(query = q, fields="id, dateAdded, dateClosed, skills, owner, clientCorporation, address, title, status, submissions, sendouts, interviews, placements, approvedPlacements"))
        }
        
        d1 <- content(r1)
        # extracting data owner location
        #oId <- d1$data[[1]]$owner$id # Getting Owner ID to get Owner location
        
        jOrder <- as.integer(jId)
        
        # Added Date
        if(is.null(d1$data[[1]]$dateAdded)) {
                addedDate <- ""
        } else {
                ## addedDate <- as.character(d1$data[[1]]$dateAdded)
                addedDate <- as.POSIXct(as.numeric(d1$data[[1]]$dateAdded)/1000,origin="1970-01-01", tz="GMT")
        } 
        
        # Closed Date
        if(is.null(d1$data[[1]]$dateClosed)) {
                closedDate <- ""
        } else {
                # closedDate <- as.character(d1$data[[1]]$dateClosed)
                closedDate <- as.POSIXct(as.numeric(d1$data[[1]]$dateClosed)/1000,origin="1970-01-01", tz="GMT")
        } 
        
        # Job Title
        if(is.null(d1$data[[1]]$title)) {
                jTitle <- ""
        } else {
                jTitle <- as.character(d1$data[[1]]$title)
        }                     
        
        # Owner ID
        if(is.null(d1$data[[1]]$owner$id)){
                ownerId <- ""
        } else {
                ownerId <- as.character(d1$data[[1]]$owner$id)
        }
        
        # Job Owner First name
        if(is.null(d1$data[[1]]$owner$firstName)) {
                jOwnerF <- ""
        } else jOwnerF <- as.character(d1$data[[1]]$owner$firstName) 
        
        # Job Owner Last name
        if(is.null(d1$data[[1]]$owner$lastName)) {
                JOwnerL <- ""
        } else JOwnerL <- as.character(d1$data[[1]]$owner$lastName) 
        
        # jOwnerZip <- 
        
        # Client name
        if(is.null(d1$data[[1]]$clientCorporation$name)) {
                jClient <- ""
        } else jClient <- as.character(d1$data[[1]]$clientCorporation$name) 
        
        # Client State location
        if(is.null(d1$data[[1]]$address$state)) {
                jClientState <- ""
        } else jClientState <- as.character(d1$data[[1]]$address$state)
        
        # Client Zip location
        if(is.null(d1$data[[1]]$address$zip)) {
                jClientZip <- ""
        } else jClientZip <- as.character(d1$data[[1]]$address$zip) 
        
        if(is.null(d1$data[[1]]$status)) {
                jStatus <- ""
        } else jStatus <- as.character(d1$data[[1]]$status)
        
        # Matrix data
        jSourced <- as.integer(d1$data[[1]]$submissions$total) # Number of Sourced Candidates
        jClientSubs <- as.integer(d1$data[[1]]$sendouts$total) # Number of Client Submissions
        jInterviews <- as.integer(d1$data[[1]]$interviews$total) # Number of Interviews
        jPlacements <- as.integer(d1$data[[1]]$approvedPlacements$total) # Number of Placements
        
        jData <- data.frame(JID = jOrder,
                            AddedDate = addedDate,
                            ClosedDate = closedDate,
                            Title = jTitle,
                            OwnerID = ownerId,
                            OwnerFirstname = jOwnerF,
                            OwnerLastname = JOwnerL,
                            ClientName= jClient, 
                            ClientState = jClientState, 
                            ClientZip = jClientZip,
                            Status= jStatus,
                            SourcedCand = jSourced,
                            ClientSubmissions = jClientSubs,
                            Interviews = jInterviews,
                            Placements = jPlacements)
        #print(jData)
        jData
        
}

getSourcedCand <- function(listJO) 
{
        jSubmissions <- data.frame("JID" = integer(),
                                   "AddedDate" = character(),
                                   "ClosedDate" = character(),
                                   "Title" = character(),
                                   "OwnerID" = character(),
                                   "OwnerFirstname" = character(),
                                   "OwnerLastname" = character(),
                                   "ClientName" = character(),
                                   "ClientState" = character(),
                                   "ClientZip" = character(),
                                   "Status" = character(),
                                   "SourcedCand" = integer(),
                                   "ClientSubmissions" = integer(),
                                   "Interviews" = integer(),
                                   "Placements" = integer(),
                                   stringsAsFactors=FALSE)
                
        # jSubmissions <- data.frame()
        
        for(i in 1:length(listJO)) 
        {
                jId <- listJO[i]
                jData <- getJOInfo(jId)
                jSubmissions <- rbind(jSubmissions, jData)
                print(i)
                #print(jData)
                                     
        }
        #colnames(jSubmissions) <- c("Id", "Ttitle", "OwnerFirstname", "OwnerLastname", "ClientName", "ClientState","ClientZip", "Status", "SourcedCand", "ClientSubmissions", "Interviews", "Placements")
        # jId, jTitle, jOwnerF, JOwnerL, jClient, jClientState, jClientZip, jStatus, jSourced, jClientSubs, jInterviews, jPlacements
        jSubmissions
}






#CLIENT Effectiveness ANALYSIS
# Column Names:
# ClientName: name of the hiring company
# ClientZip: Zip Code of the hiring company
# Jobs: # of jobs being worked on for the client
# SourcedCands: # of candidates sourced for the jobs by the client
# Submissions: # of submissions made to the jobs by the client
# Interviews: # of interviews done to the jobs by the client
# Placements: # of placements made to the jobs by the client
#### Measuring Sourcing Activities
# SourcedToJobs: # of Sourced Candidates per Job
# SourcedToSubmissions: # of Sourced Candidates per Submission
# SourcedToInterviews: # of Sourced Candidates per Interview
# SourcedToPlacements: # of Sourced Candidates per Placement
#### Measuring Client effectiveness
# SubmissionToInterview: # of Submissions per Interview
# SubmissionToPlacement: # of Submissions per Placement

clientAData <- function(listJOData)
{
#         output <- data.frame("ClientName" = character(),
#                              "ClientZip" = character(),
#                              "ClientState" = character(),                                
#                              "Jobs" = integer(),
#                              "SourcedCands" = integer(),
#                              "Submissions" = integer(),
#                              "Interviews" = integer(),
#                              "Placements" = integer(),
#                              "SourcedToJobs" = integer(),
#                              "SourcedToSubmissions" = integer(),
#                              "SourcedToInterviews" = integer(),
#                              "SourcedToPlacements" = integer(),
#                              "SubmissionToInterview" = integer(),
#                              "SubmissionToPlacement" = integer(),
#                              stringsAsFactors=FALSE)
        
        #ClientName
        clientNameList <- unique(listJOData$ClientName) 
        ln <- length(clientNameList)
        
        #ClientZip
        zipList <- list() 
        
        #ClientState
        stateList <- list()
        
        #Jobs
        jobsNum <- list()
        
        #SourcedCands
        sourcedCandsNum <- list()
        
        #Submissions
        submissionsNum <- list()
        
        #Interviews
        interviewsNum <- list()
        
        #Placements
        placementsNum <- list()
        
        #SourcedToJobs
        sourcedToJobsNum <- list()
        
        #SourcedToSubmissions
        sourcedToSubmissionsNum <- list()
        
        #SourcedToInterviews
        sourcedToInterviewsNum <- list()
        
        #SourcedToPlacements
        sourcedToPlacementsNum <- list()
        
        #SubmissionToInterview
        submissionToInterviewNum <- list()
        
        #SubmissionToPlacement
        submissionToPlacementNum <- list()
        
        for(i in 1:ln){
                
                subDat <- subset(listJOData,subset=(ClientName==clientNameList[i]))
                
                zipList[i] <- as.character(subDat$ClientZip[1])
                
                stateList[i] <- as.character(subDat$ClientState[1])
                
                jobsNum[i] <- as.numeric(length(subDat$Title))
                
                sourcedCandsNum[i] <- sum(subDat$SourcedCand)
                submissionsNum[i] <- sum(subDat$ClientSubmissions)
                interviewsNum[i] <- sum(subDat$Interviews)
                placementsNum[i] <- sum(subDat$Placements)
                
                #SourcedToJobs
                if(as.numeric(jobsNum[i])!=0) {
                        sourcedToJobsNum[i] <- round(as.numeric(sourcedCandsNum[i])/as.numeric(jobsNum[i]),digits=2)
                } else sourcedToJobsNum[i] <- 0
                
                #SourcedToSubmissions
                if(as.numeric(submissionsNum[i])!=0) {
                        sourcedToSubmissionsNum[i] <- round(as.numeric(sourcedCandsNum[i])/as.numeric(submissionsNum[i]),digits=2)
                } else sourcedToSubmissionsNum[i] <- 0
                
                #SourcedToInterviews
                if(as.numeric(interviewsNum[i])!=0) {
                        sourcedToInterviewsNum[i] <- round(as.numeric(sourcedCandsNum[i])/as.numeric(interviewsNum[i]),digits=2)
                } else sourcedToInterviewsNum[i] <- 0
                
                #SourcedToPlacements
                if(as.numeric(placementsNum[i])!=0) {
                        sourcedToPlacementsNum[i] <- round(as.numeric(sourcedCandsNum[i])/as.numeric(placementsNum[i]),digits=2)
                } else sourcedToPlacementsNum[i] <- 0
                
                #SubmissionToInterview
                if(as.numeric(interviewsNum[i])!=0) {
                        submissionToInterviewNum[i] <- round(as.numeric(submissionsNum[i])/as.numeric(interviewsNum[i]),digits=2)
                } else submissionToInterviewNum[i] <- 0
                
                #SubmissionToPlacement
                if(as.numeric(placementsNum[i])!=0) {
                        submissionToPlacementNum[i] <- round(as.numeric(submissionsNum[i])/as.numeric(placementsNum[i]),digits=2)
                } else submissionToPlacementNum[i] <- 0
                
        } 
        
        output <- data.table(clientNameList,
                             zipList,
                             stateList,
                             jobsNum,
                             sourcedCandsNum,
                             submissionsNum,
                             interviewsNum,
                             placementsNum,
                             sourcedToJobsNum,
                             sourcedToSubmissionsNum,
                             sourcedToInterviewsNum,
                             sourcedToPlacementsNum,
                             submissionToInterviewNum,
                             submissionToPlacementNum)
                
        setnames(output,c("ClientName",
                          "ClientZip",
                          "ClientState",
                          "Jobs",
                          "SourcedCands",
                          "Submissions",
                          "Interviews",
                          "Placements",
                          "SourcedToJobs",
                          "SourcedToSubmissions",
                          "SourcedToInterviews",
                          "SourcedToPlacements",
                          "SubmissionToInterview",
                          "SubmissionToPlacement"))
        output

}

cleansedStates <- function(listJOData)
{
        states <- read.xlsx("usastates.xlsx",1)
        
        statesInPut <- listJOData$ClientState
        
        statesOutPut <- list()
        
        for(i in 1:length(statesInPut)) 
        {
                if(statesInPut[i] %in% states$Input){
                        n <- match(statesInPut[i],states$Input)
                        statesOutPut[i] <- as.character(states$Cleansed[n])
                } else {
                        statesOutPut[i] <- paste(statesInPut[i],"NEW")
                }
        }
        
        unlist(statesOutPut)
        
#         cleansedStateTable <- data.table(statesInPut,unlist(statesOutPut))
#         setnames(cleansedStateTable,c("InputState","OutputState"))
#         setkey(cleansedStateTable,"InputState")
#         
#         listJOData <- as.data.table(listJOData)
#         setkey(listJOData,"ClientState")
        
        #merge(listJOData,cleansedStateTable)
}

getOwnerMarketInfo <- function(listJOData)
{
        ownerList <- unique(listJOData$OwnerID)
        
        ownerDept <- list()
        
        deptName <- list()
        
        for(i in 1:length(ownerList)){
                p <- paste("rest-services/4bk1/entity/CorporateUser/",as.character(ownerList[i]),sep="")
                
                owner1 <- GET(br, 
                              path = p,
                              add_headers(BhRestToken = bHToken),
                              query = list(fields="departments"))
                
                if(status_code(owner1)!=200) 
                {
                        bHToken <- bTokenRenew(bHAuToken)
                        owner1 <- GET(br, 
                                      path = p,
                                      add_headers(BhRestToken = bHToken),
                                      query = list(fields="departments"))
                }
                
                o1 <- content(owner1)
                
                ownerDept[i] <- as.character(o1$data$departments$data[[1]]$id)
                
                # print(o1$data$departments$data[[1]]$id)
                
                pd <- paste("rest-services/4bk1/entity/CorporationDepartment/",as.character(ownerDept[i]),sep="")
                
                #pd <- paste("rest-services/4bk1/entity/CorporationDepartment/",16492,sep="")
                
                dept1 <- GET(br, 
                              path = pd,
                              add_headers(BhRestToken = bHToken),
                              query = list(fields="name"))
                
                if(status_code(owner1)!=200) 
                {
                        bHToken <- bTokenRenew(bHAuToken)
                        dept1 <- GET(br, 
                                     path = pd,
                                     add_headers(BhRestToken = bHToken),
                                     query = list(fields="name"))
                }
                
                deptName[i] <- as.character(content(dept1)$data$name)
        }
        
        ownerData <- data.table(ownerList,ownerDept, deptName)
        setnames(ownerData, c("OwnerID","OwnerDept","DepartmentName"))
        setkey(ownerData,"OwnerID")
        
        listJOData <- as.data.table(listJOData)
        setkey(listJOData,"OwnerID")
        
        merge(listJOData,ownerData)
        
}




oauth2.0_refresh <- function(endpoint, app, access_token, type = NULL) {
        req <- POST(
                url = endpoint$access,
                multipart = FALSE,
                body = list(
                        client_id = app$key,
                        client_secret = app$secret,
                        grant_type = "refresh_token",
                        refresh_token = access_token$refresh_token
                )
        )
        content_out <- content(req, type = type)
        content_out <- c(content_out, access_token['refresh_token'])
}

#t <- oauth2.0_refresh(bullhorn_endpoint, mybullhorn, bHAuToken$credentials)
#########################################


##############################################
bSearchName <- function(x,bHToken)
{
        # {corpToken}
        # /search/{entityType}
        # ?query={luceneQueryString}
        # &fields={fieldList}&sort={fieldList}
        # &count={count}
        # &start={start}
        
        # set_cookies("BhRestToken"=bHToken)
        
        r <- GET(br, 
                 path = "rest-services/4bk1/search/JobOrder", 
                 add_headers(BhRestToken = bHToken),
                 query = list(query="address.state:NJ", fields="id,address,status", count="1"))
        
        
        r1 <- GET(br, 
                  path = "rest-services/4bk1/search/JobOrder",
                  add_headers(BhRestToken = bHToken),
                  query = list(query="id:33338", fields="owner, dateAdded, clientCorporation, address, title, status, submissions, sendouts, interviews, placements, approvedPlacements"))
        
        owner2 <- GET(br, 
                      path = "rest-services/4bk1/entity/CorporateUser/320014",
                      add_headers(BhRestToken = bHToken),
                      query = list(fields="departments"))
        
        j1 <- GET(br, 
                  path = "rest-services/4bk1/search/JobOrder",
                  add_headers(BhRestToken = bHToken),
                  query = list(query="id:32958", fields="owner, clientCorporation, address, title, status, submissions, sendouts, interviews, placements, approvedPlacements"))
        j2 <- GET(br, 
                  path = "rest-services/4bk1/search/JobOrder",
                  add_headers(BhRestToken = bHToken),
                  query = list(query="id:33268", fields="owner, clientCorporation, address, title, status, submissions, sendouts, interviews, placements, approvedPlacements"))
        
        #33268: wrong data
        # submissions = sourced candidates
        # customInt1 = submitted # -> unneccesary?
        # sendouts = client submissions
        # interviews = interview
        # placements = placement
        
        
        
        # Get the meta
        meta <- GET(br, 
                    path = "rest-services/4bk1/meta/JobOrder",
                    add_headers(BhRestToken = bHToken),
                    query = list(query="id:33268", fields="owner, clientCorporation, address, title, status, submissions, sendouts, interviews, placements, approvedPlacements"))
        
        
        
        r2 <- GET(br, 
                  path = "rest-services/4bk1/search/Candidate",
                  add_headers(BhRestToken = bHToken),
                  query = list(query = "dateAdded>03012014", fields="*"))
        
        r3 <- GET(br, 
                  path = "rest-services/4bk1/search/JobOrder",
                  add_headers(BhRestToken = bHToken),
                  query = list(query = "dateAdded: [20150301 TO 20150315]", fields="id"))
        
        # start=01-01-2012&end=01-31-2012
        
        bHToken <- bTokenRenew(bHAuToken)
        
        # Get the list of Job Order in a Data range
        r3 <- GET(br, 
                  path = "rest-services/4bk1/search/JobOrder",
                  add_headers(BhRestToken = bHToken),
                  query = list(query = "dateAdded: [20150301 TO 20150315]", fields="id"), count="100")
}



getJOInfo_Full <- function(jId){
  
  q <- paste(c("id:",as.character(jId)),collapse=" ")
  
  r1 <- GET(br, 
            path = "rest-services/4bk1/search/JobOrder",
            add_headers(BhRestToken = bHToken),
            query = list(query = q, fields="*"))
  
  if(status_code(r1)!=200) 
  {
    bHToken <- bTokenRenew(bHAuToken)
    r1 <- GET(br, 
              path = "rest-services/4bk1/search/JobOrder",
              add_headers(BhRestToken = bHToken),
              query = list(query = q, fields="*"))
  }
  
  r1
  
}

getJobListData_Full <- function(listJO) 
{
  jDataList <- list()
  
  print(length(listJO))
  
  for(i in 1:length(listJO)) 
  {
    print(i)
    jId <- listJO[i]
    print(jId)
    jData <- as.data.table(fromJSON(toString(getJOInfo_Full(jId)))$data)
    jDataList <- c(jDataList,jData)
  }
  jDataList
}