library(xlsx)

setwd("C:\\Jig12051\\Telecom Case Study")

#telecom <- read.csv("C:/Data Science with R/Assignments/Graded Assignments/Topic 13 -  Final Case Study Course Wrap up/telecomfinal.csv")
telecom <- read.csv("Telecom_Updated.csv")
#getting the data type,summaries and missing values
lists<-1
records <- 1
availableData<- 1
availablePercent<- 1
missingPercent<- 1
maxs<-1
mins<-1
average<-1
sd<-1
u<-1
tile5<- 1
tile10<- 1
tile25<- 1
tile50<- 1
tile75<- 1
tile90<- 1
tile95<- 1

#missing value
missing_count<-colSums(is.na(telecom))
variable<-names(telecom)

for(i in 1:length(variable))
{
  x<-variable[i]
  y<-telecom[,x] #all the values present in a given column
  
  lists[i]<-class(y)
  records[i]<-length(y)
  availableData[i] <- length(which(!is.na(y)))
  availablePercent[i]<- availableData[i]/records[i]
  missingPercent[i] <- missing_count[i]/records[i]
  maxs[i]<-ifelse((class(y)=="integer" | class(y)=="numeric"),max(y,na.rm=TRUE),0)
  mins[i]<-ifelse((class(y)=="integer" | class(y)=="numeric"),min(y,na.rm=TRUE),0)
  average[i]<-as.integer(ifelse((class(y)=="integer" | class(y)=="numeric"),mean(y,na.rm=TRUE),0))
  sd[i]<-ifelse((class(y)=="integer" | class(y)=="numeric"),sd(y,na.rm=TRUE),0)
  u[i]<-ifelse(class(y)=="factor" ,unique(y),0)
  tile5[i]<-as.integer(ifelse((class(y)=="integer" | class(y)=="numeric"),quantile(y, probs = c(0.05), na.rm = TRUE ),0))
  tile10[i]<-as.integer(ifelse((class(y)=="integer" | class(y)=="numeric"),quantile(y, probs = c(0.10), na.rm = TRUE ),0))
  tile25[i]<-as.integer(ifelse((class(y)=="integer" | class(y)=="numeric"),quantile(y, probs = c(0.25), na.rm = TRUE ),0))
  tile50[i]<-as.integer(ifelse((class(y)=="integer" | class(y)=="numeric"),quantile(y, probs = c(0.50), na.rm = TRUE ),0))
  tile75[i]<-as.integer(ifelse((class(y)=="integer" | class(y)=="numeric"),quantile(y, probs = c(0.75), na.rm = TRUE ),0))
  tile90[i]<-as.integer(ifelse((class(y)=="integer" | class(y)=="numeric"),quantile(y, probs = c(0.90), na.rm = TRUE ),0))
  tile95[i]<-as.integer(ifelse((class(y)=="integer" | class(y)=="numeric"),quantile(y, probs = c(0.95), na.rm = TRUE ),0))
  
  print(i)
}



final_summary<-data.frame(Variable=names(telecom),DataType=lists,NoOfRecords=records,
                          AvailableData = availableData,AvailablePercent=availablePercent,
                          MissingData=missing_count,MissingPercent = missingPercent,
                          Unique=u,Minimum=mins,Maximum=maxs,Average=average,
                          StandardDeviation=sd, Percentile_5th= tile5, Percentile_10th= tile10,
                          Percentile_25th= tile25, Percentile_50th= tile50,
                          Percentile_75th= tile75, Percentile_95th= tile90,
                          Percentile_95th= tile95)

#Generate xlsx file of report
write.xlsx(final_summary, "Data Report_Updated.xlsx", row.names = FALSE)



