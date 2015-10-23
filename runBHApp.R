source("DataPull/getBHAuth.R")
source("DataPull/bullhornAPICalls.R")



listJO <- getJOByDateRange(20101220,20101231)
jobsData <- getJobListData_Full(listJO)

#write.list(test, filename="test.txt")

jobsData_dataframe <- data.frame()
for (i in 1:(length(jobsData_temp)/133)) {
  jobi <- flatten(as.data.frame(jobsData_temp[((i-1)*133+1):((i-1)*133+133)]))
  jobsData_dataframe <- rbind(jobsData_dataframe,jobi)
}

jobsData_dataframe <- data.frame()
for (i in 1:(length(jobsData_temp)/133)) {
  jobi <- flatten(as.data.frame(jobsData_temp[((i-1)*133+1):((i-1)*133+133)]))
  jobsData_dataframe <- rbind(jobsData_dataframe,jobi)
}
