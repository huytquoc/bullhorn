source("DataPull/getBHAuth.R")
source("DataPull/bullhornAPICalls.R")

if(!require("dplyr")){
  install.packages("dplyr")
}
library("dplyr")

listJO <- getJOByDateRange(20101201,20101231)
jobsData <- getJobListData_Full(listJO)

#write.list(test, filename="test.txt")

jobsData_dataframe <- data.frame()
data_names = NULL
jobi_last = NULL
for (i in 1:(length(jobsData_temp)/135)) {
  jobi <- as.data.frame(jobsData_temp[((i-1)*135+1):((i-1)*135+135)])
#   if (is.null(data_names)) {
#     data_names <- names(jobi)
#   } else {
#       names(jobi) <- data_names
#   }
#   print(i)
#   jobsData_dataframe <- rbind(jobsData_dataframe,jobi, row.names=NULL)
  jobsData_dataframe <- bind_rows(jobsData_dataframe,jobi)
  
}

jobsData_dataframe <- data.frame()
for (i in 1:(length(jobsData_temp)/133)) {
  jobi <- flatten(as.data.frame(jobsData_temp[((i-1)*133+1):((i-1)*133+133)]))
  jobsData_dataframe <- rbind(jobsData_dataframe,jobi)
}
