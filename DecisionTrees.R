library(dplyr)
library(irr)
library(rpart)
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(reshape2)

setwd("C:\\Jig12051\\Decision Tree")

#reading the BH.csv
bh <- read.csv("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 11.2 -  Decison Trees\\BH.csv")
summary(bh)
str(bh$cus_employername)
length(unique(bh$cus_employername))

#Replacing missing values as 'Missing'
bh$cus_employername <- ifelse(bh$cus_employername =="", "Missing", as.character(bh$cus_employername))
bh$cus_employername <- as.factor(bh$cus_employername)

#ReChecking for missing values
colSums(is.na(bh))
#No missing values in cus_employername

#Removing rows with missing values in TARGET
bh <- bh[!is.na(bh$TARGET),]

IDV_DV <- select(bh, cus_employername, TARGET, GOOD_BAD)
colSums(is.na(IDV_DV))

table(bh$TARGET)

#Decsion Tree
mod <- rpart(TARGET~cus_employername, data=bh, method = "class",
             control=rpart.control(cp=0.001, maxdepth =2 ))

mod
summary(mod)
print(mod)
fancyRpartPlot(mod)
#Extracting Rules
asRules(mod)

#Code to extract cus_employernames for the different nodes

#Contains levels of cus_employername
xlevels <- data.frame(attr(mod,"xlevels"))

#Contains information on number of splits
split <- data.frame(mod$split)

#Contains node information
csplit <- data.frame(mod$csplit)

#Changing Row to column
csplit <- data.frame(melt(csplit))
csplit <- data.frame(csplit[,-1])
table(csplit)

employer <- cbind(xlevels,csplit)

#1= left node(node 2) 3= right node(node 3)
#Node 2 has high bad rate, Node 3 has low bad rate
employer$grp_employer <- ifelse(employer$csplit....1. == 1, "Group 1",as.character(employer$csplit....1.))
employer$grp_employer <- ifelse(employer$csplit....1. == 3, "Group 2",as.character(employer$grp_employer))
employer$grp_employer <- ifelse(employer$csplit....1. == 2, NA ,as.character(employer$grp_employer))

#Deleting csplit columns
employer <- employer[,-2] 
table(employer$grp_employer)


#Merging the tables on cus_employername
bh_updated <- merge(x = bh, y = employer, by = "cus_employername", all.x = TRUE)

IDV_DV2 <- select(bh_updated, cus_employername, TARGET, GOOD_BAD,grp_employer)
colSums(is.na(IDV_DV2))

#Re-arranging the columns
bh_updated %>% select(id,cus_employername,grp_employer, everything()) ->bh_updated

write.csv(bh_updated,"C:\\Jig12051\\Decision Tree\\BH_Updated.csv")
          