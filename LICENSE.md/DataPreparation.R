getwd()
setwd("C:\\Jig12051\\Data Preparation")
library(dplyr)
library(mosaic)
library(lubridate)
library(ggplot2)

#Reading data from the text files
Campaign <- read.table("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 8.2 -  Data Preparation\\Campaign_File.txt",header = T, sep = "\t")
Customers<- read.table("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 8.2 -  Data Preparation\\Customers_File.txt",header = T, sep = "\t")
Products<- read.table("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 8.2 -  Data Preparation\\Products_File.txt",header = T, sep = "\t")
Transactions<- read.table("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 8.2 -  Data Preparation\\Transactions_File.txt",header = T, sep = "\t")

table(Campaign$Campaign_Responce)
#Changing True=1 and false=0
Campaign$Campaign_Responce <- ifelse(Campaign$Campaign_Responce== TRUE,1,0)

#Calculating the age
Customers$Age <- as.integer(round(difftime(Sys.Date(), as.Date(Customers$Birth_Date), units = "days")/365,0))

#Calculating tenure
Customers$Tenure <- as.numeric(round(difftime(as.Date("2002-12-31"),as.Date(Customers$Registration_Date),units = "days")/365,1))
summary(Customers$Tenure)

#Grouping the age data
Customers <- mutate(Customers, Age_bucket = derivedFactor(
  "20-50" = (Age>20 & Age<=50),
  "50-75" = (Age>50 & Age<=75),
  "75-100" = (Age>75 & Age<=100),
  "100-130" = (Age>100 & Age<=130),
  .method = "first",
  .default = NA
))

#Grouping the tenure data
Customers <- mutate(Customers, Tenure_Quartiles = derivedFactor(
  "0.-1" = (Tenure>0 & Tenure<=1),
  "1-2" = (Tenure>1 & Tenure<=2),
  "2-3" = (Tenure>2 & Tenure<=3),
  "3-4" = (Tenure>3 & Tenure<=4),
  "4-5" = (Tenure>4 & Tenure<=5),
  .method = "first",
  .default = NA
))

#Merging the tables Transactions and Products
summary(Transactions)
summary(Products)
Trans_Pro <- merge(x= Transactions, y =Products, by = "Product_Code")
Trans_Pro %>% group_by(Product_Category) %>% summarise(No_of_Transaction =n(),Amount_spent = sum(Items_Amount)) %>% arrange(-Amount_spent)
#Answer- Entertainment category dominates in $ amount

Trans_Cust <- merge(x= Transactions, y =Customers, by = "Card_ID") 
#Calculating the age at the time of transaction
Trans_Cust %>% group_by(Age_bucket) %>% summarise(Contribution_of_Group = sum(Items_Amount))

summary(Campaign)
#Calculating Response rate
Campaign %>% summarise(Response_Rate = mean(Campaign_Responce))

#Merge the tables
Camp_Cust <- merge(x= Campaign, y =Customers, by = "Card_ID") 
summary(Camp_Cust$Age)
table(Camp_Cust$Age_bucket,Camp_Cust$Campaign_Responce)

Camp_Cust %>% group_by(Age_bucket) %>% summarise(TotalResponse= sum(Campaign_Responce),ResponseRate = mean(Campaign_Responce))
#Higher response rate in Quartile 1
# Response rate is decreasing with increasing age group


#Calculating Response rate by tenure quartiles
Camp_Cust %>% group_by(Tenure_Quartiles) %>% summarise(Respons_eRate = mean(Campaign_Responce))
#Response rate higher in Q5 i.e people higher tenure
#Response rate increasing with increase in tenure


#Calculating Resp rate by tenure and age
Camp_Cust%>% group_by(Age_bucket,Tenure_Quartiles) %>% summarise(ResponseRate = mean(Campaign_Responce)) -> Resp_Rate
xtabs(ResponseRate~Age_bucket+Tenure_Quartiles,data= Resp_Rate)



Transactions %>% group_by(Payment_Method) %>% summarise(No_Of_Trans = n()) %>% arrange(-No_Of_Trans)
#Credit card
Transactions$Timestamp <- ymd_hms(Transactions$Timestamp)
Transactions <- mutate(Transactions, Hour_Of_Day = hour(Timestamp))
summary(Transactions$Hour_Of_Day)

Transactions <- mutate(Transactions, Time_Of_Day = derivedFactor(
  "9-12" = (Hour_Of_Day>9 & Hour_Of_Day<=12),
  "12-15" = (Hour_Of_Day>12 & Hour_Of_Day<=15),
  "15-18" = (Hour_Of_Day>15 & Hour_Of_Day<=18),
  .method = "first",
  .default = NA
))
table(Transactions$Payment_Method,Transactions$Time_Of_Day)
#Not effected by time

Age_Gender_Amt_Tb <- xtabs(Items_Amount~Age_bucket+Gender, data = Trans_Cust)

                                          
p <- ggplot(Customers,aes(x=Tenure, fill=Gender))
p+ geom_histogram(stat = "bin", position = "dodge", alpha= 0.8, binwidth = 0.5)+
  theme_light()+
  scale_x_continuous(breaks=seq(0.0,5.0,0.5))+
  ggtitle("Tenure of a Customer") +ylab("Frequency")
