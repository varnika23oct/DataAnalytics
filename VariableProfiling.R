library(dplyr)
library(gtools)
library(xlsx)


#set working directory
setwd("C:\\Jig12051\\Telecom Case Study")

telecom <- read.csv("C:/Data Science with R/Assignments/Graded Assignments/Topic 13 -  Final Case Study Course Wrap up/telecomfinal.csv")

#telecom <- read.csv("Telecom_Updated.csv")



#set working directory
setwd("C:\\Jig12051\\Telecom Case Study\\Continous Variable Profiles")

#Variable profiling : Continous Variable
table(telecom$churn)

#eqpdays
telecom %>% mutate( dec = ntile(eqpdays,n=10))->eqpdays
eqpdays %>% group_by(dec) %>% summarise(n=sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(eqpdays),LessThan=max(eqpdays)) -> eqpdays
eqpdays$varname<-rep("eqpdays",nrow(eqpdays))

#mou_Mean
telecom %>% mutate( dec = ntile(mou_Mean,n=10))->mou_Mean
mou_Mean %>% group_by(dec) %>% summarise(n=sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(mou_Mean),LessThan=max(mou_Mean)) -> mou_Mean
mou_Mean$varname<-rep("mou_Mean",nrow(mou_Mean))
write.xlsx(mou_Mean, "mou_Mean.xlsx")

#totmrc_Mean
telecom %>% mutate( dec = ntile(totmrc_Mean,n=10))->totmrc_Mean
totmrc_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(totmrc_Mean),LessThan=max(totmrc_Mean)) -> totmrc_Mean
totmrc_Mean$varname<-rep("totmrc_Mean",nrow(totmrc_Mean))
write.xlsx(totmrc_Mean, "totmrc_Mean.xlsx")

#rev_Range
telecom %>% mutate( dec = ntile(rev_Range,n=10))->rev_Range
rev_Range %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(rev_Range),LessThan=max(rev_Range)) -> rev_Range
rev_Range$varname<-rep("rev_Range",nrow(rev_Range))
write.xlsx(rev_Range, "rev_Range.xlsx")


#mou_Range
telecom %>% mutate( dec = ntile(mou_Range,n=10))->mou_Range
mou_Range %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(mou_Range),LessThan=max(mou_Range)) -> mou_Range
mou_Range$varname<-rep("mou_Range",nrow(mou_Range))
write.xlsx(mou_Range, "mou_Range.xlsx")

#change_mou
telecom %>% mutate( dec = ntile(change_mou,n=10))->change_mou
change_mou %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(change_mou),LessThan=max(change_mou)) -> change_mou
change_mou$varname<-rep("change_mou",nrow(change_mou))
write.xlsx(change_mou, "change_mou.xlsx")
#No trend

#drop_blk_Mean
telecom %>% mutate( dec = ntile(drop_blk_Mean,n=8))->drop_blk_Mean
drop_blk_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(drop_blk_Mean),LessThan=max(drop_blk_Mean)) -> drop_blk_Mean
drop_blk_Mean$varname<-rep("drop_blk_Mean",nrow(drop_blk_Mean))
write.xlsx(drop_blk_Mean, "drop_blk_Mean.xlsx")
#Almost constant

#owylis_vce_Range

#totrev
telecom %>% mutate( dec = ntile(totrev,n=10))->totrev
totrev %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(totrev),LessThan=max(totrev)) -> totrev
totrev$varname<-rep("totrev",nrow(totrev))
write.xlsx(totrev, "totrev.xlsx")
#Almost constant

#months
telecom %>% mutate( dec = ntile(months,n=10))->months
months %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(months),LessThan=max(months)) -> months
months$varname<-rep("months",nrow(months))
write.xlsx(months, "months.xlsx")

#totcalls
telecom %>% mutate( dec = ntile(totcalls,n=10))->totcalls
totcalls %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(totcalls),LessThan=max(totcalls)) -> totcalls
totcalls$varname<-rep("totcalls",nrow(totcalls))
write.xlsx(totcalls, "totcalls.xlsx")

#ovrrev_Mean
telecom %>% mutate( dec = ntile(ovrrev_Mean,n=10))->ovrrev_Mean
ovrrev_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(ovrrev_Mean),LessThan=max(ovrrev_Mean)) -> ovrrev_Mean
ovrrev_Mean$varname<-rep("ovrrev_Mean",nrow(ovrrev_Mean))
write.xlsx(ovrrev_Mean, "ovrrev_Mean.xlsx")

#ovrmou_Mean
telecom %>% mutate( dec = ntile(ovrmou_Mean,n=10))->ovrmou_Mean
ovrmou_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(ovrmou_Mean),LessThan=max(ovrmou_Mean)) -> ovrmou_Mean
ovrmou_Mean$varname<-rep("ovrmou_Mean",nrow(ovrmou_Mean))
write.xlsx(ovrmou_Mean, "ovrmou_Mean.xlsx")

#rev_Mean
telecom %>% mutate( dec = ntile(rev_Mean,n=10))->rev_Mean
rev_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(rev_Mean),LessThan=max(rev_Mean)) -> rev_Mean
rev_Mean$varname<-rep("rev_Mean",nrow(rev_Mean))
write.xlsx(rev_Mean, "rev_Mean.xlsx")

#avg3mou
telecom %>% mutate( dec = ntile(avg3mou,n=10))->avg3mou
avg3mou %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(avg3mou),LessThan=max(avg3mou)) -> avg3mou
avg3mou$varname<-rep("avg3mou",nrow(avg3mou))
write.xlsx(avg3mou, "avg3mou.xlsx")

#avgmou
telecom %>% mutate( dec = ntile(avgmou,n=10))->avgmou
avgmou %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(avgmou),LessThan=max(avgmou)) -> avgmou
avgmou$varname<-rep("avgmou",nrow(avgmou))
write.xlsx(avgmou, "avgmou.xlsx")

#avg3qty
telecom %>% mutate( dec = ntile(avg3qty,n=10))->avg3qty
avg3qty %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(avg3qty),LessThan=max(avg3qty)) -> avg3qty
avg3qty$varname<-rep("avg3qty",nrow(avg3qty))
write.xlsx(avg3qty, "avg3qty.xlsx")

#avgqty
telecom %>% mutate( dec = ntile(avgqty,n=10))->avgqty
avgqty %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(avgqty),LessThan=max(avgqty)) -> avgqty
avgqty$varname<-rep("avgqty",nrow(avgqty))
write.xlsx(avgqty, "avgqty.xlsx")

#avg6mou
telecom %>% mutate( dec = ntile(avg6mou,n=10))->avg6mou
avg6mou %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(avg6mou),LessThan=max(avg6mou)) -> avg6mou
avg6mou$varname<-rep("avg6mou",nrow(avg6mou))
write.xlsx(avg6mou, "avg6mou.xlsx")

#avg6qty
telecom %>% mutate( dec = ntile(avg6qty,n=10))->avg6qty
avg6qty %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(avg6qty),LessThan=max(avg6qty)) -> avg6qty
avg6qty$varname<-rep("avg6qty",nrow(avg6qty))
write.xlsx(avg6qty, "avg6qty.xlsx")

#opk_dat_Mean
telecom %>% mutate( dec = ntile(opk_dat_Mean,n=2))->opk_dat_Mean
opk_dat_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(opk_dat_Mean),LessThan=max(opk_dat_Mean)) -> opk_dat_Mean
opk_dat_Mean$varname<-rep("opk_dat_Mean",nrow(opk_dat_Mean))
write.xlsx(opk_dat_Mean, "opk_dat_Mean.xlsx")

#blck_dat_Mean
telecom %>% mutate( dec = ntile(blck_dat_Mean,n=2))->blck_dat_Mean
blck_dat_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(blck_dat_Mean),LessThan=max(blck_dat_Mean)) -> blck_dat_Mean
blck_dat_Mean$varname<-rep("blck_dat_Mean",nrow(blck_dat_Mean))
write.xlsx(blck_dat_Mean, "blck_dat_Mean.xlsx")

#datovr_Mean
telecom %>% mutate( dec = ntile(datovr_Mean,n=2))->datovr_Mean
datovr_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(datovr_Mean),LessThan=max(datovr_Mean)) -> datovr_Mean
datovr_Mean$varname<-rep("datovr_Mean",nrow(datovr_Mean))
write.xlsx(datovr_Mean, "datovr_Mean.xlsx")

#drop_dat_Mean
telecom %>% mutate( dec = ntile(drop_dat_Mean,n=2))->drop_dat_Mean
drop_dat_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(drop_dat_Mean),LessThan=max(drop_dat_Mean)) -> drop_dat_Mean
drop_dat_Mean$varname<-rep("drop_dat_Mean",nrow(drop_dat_Mean))
write.xlsx(drop_dat_Mean, "drop_dat_Mean.xlsx")

#drop_vce_Mean
telecom %>% mutate( dec = ntile(drop_vce_Mean,n=6))->drop_vce_Mean
drop_vce_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(drop_vce_Mean),LessThan=max(drop_vce_Mean)) -> drop_vce_Mean
drop_vce_Mean$varname<-rep("drop_vce_Mean",nrow(drop_vce_Mean))
write.xlsx(drop_vce_Mean, "drop_vce_Mean.xlsx")

#adjmou
telecom %>% mutate( dec = ntile(adjmou,n=10))->adjmou
adjmou %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(adjmou),LessThan=max(adjmou)) -> adjmou
adjmou$varname<-rep("adjmou",nrow(adjmou))
write.xlsx(adjmou, "adjmou.xlsx")

#comp_dat_Mean
telecom %>% mutate( dec = ntile(comp_dat_Mean,n=20))->comp_dat_Mean
comp_dat_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(comp_dat_Mean),LessThan=max(comp_dat_Mean)) -> comp_dat_Mean
comp_dat_Mean$varname<-rep("comp_dat_Mean",nrow(comp_dat_Mean))
write.xlsx(comp_dat_Mean, "comp_dat_Mean.xlsx")

#plcd_dat_Mean
telecom %>% mutate( dec = ntile(plcd_dat_Mean,n=20))->plcd_dat_Mean
plcd_dat_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(plcd_dat_Mean),LessThan=max(plcd_dat_Mean)) -> plcd_dat_Mean
plcd_dat_Mean$varname<-rep("plcd_dat_Mean",nrow(plcd_dat_Mean))
write.xlsx(plcd_dat_Mean, "plcd_dat_Mean.xlsx")

#avgrev
telecom %>% mutate( dec = ntile(avgrev,n=10))->avgrev
avgrev %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(avgrev),LessThan=max(avgrev)) -> avgrev
avgrev$varname<-rep("avgrev",nrow(avgrev))
write.xlsx(avgrev, "avgrev.xlsx")

#adjrev
telecom %>% mutate( dec = ntile(adjrev,n=6))->adjrev
adjrev %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(adjrev),LessThan=max(adjrev)) -> adjrev
adjrev$varname<-rep("adjrev",nrow(adjrev))
write.xlsx(adjrev, "adjrev.xlsx")

#datovr_Range
telecom %>% mutate( dec = ntile(datovr_Range,n=2))->datovr_Range
datovr_Range %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(datovr_Range),LessThan=max(datovr_Range)) -> datovr_Range
datovr_Range$varname<-rep("datovr_Range",nrow(datovr_Range))
write.xlsx(datovr_Range, "datovr_Range.xlsx")

#da_Mean
telecom %>% mutate( dec = ntile(da_Mean,n=4))->da_Mean
da_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(da_Mean),LessThan=max(da_Mean)) -> da_Mean
da_Mean$varname<-rep("da_Mean",nrow(da_Mean))
write.xlsx(da_Mean, "da_Mean.xlsx")

#da_Range
telecom %>% mutate( dec = ntile(da_Range,n=4))->da_Range
da_Range %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(da_Range),LessThan=max(da_Range)) -> da_Range
da_Range$varname<-rep("da_Range",nrow(da_Range))
write.xlsx(da_Range, "da_Range.xlsx")

#mou_pead_Mean
telecom %>% mutate( dec = ntile(mou_pead_Mean,n=2))->mou_pead_Mean
mou_pead_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(mou_pead_Mean),LessThan=max(mou_pead_Mean)) -> mou_pead_Mean
mou_pead_Mean$varname<-rep("mou_pead_Mean",nrow(mou_pead_Mean))
write.xlsx(mou_pead_Mean, "mou_pead_Mean.xlsx")

#drop_blk_Mean
telecom %>% mutate( dec = ntile(drop_blk_Mean,n=8))->drop_blk_Mean
drop_blk_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(drop_blk_Mean),LessThan=max(drop_blk_Mean)) -> drop_blk_Mean
drop_blk_Mean$varname<-rep("drop_blk_Mean",nrow(drop_blk_Mean))
write.xlsx(drop_blk_Mean, "drop_blk_Mean.xlsx")

#drop_vce_Range
telecom %>% mutate( dec = ntile(drop_vce_Range,n=4))->drop_vce_Range
drop_vce_Range %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(drop_vce_Range),LessThan=max(drop_vce_Range)) -> drop_vce_Range
drop_vce_Range$varname<-rep("drop_vce_Range",nrow(drop_vce_Range))
write.xlsx(drop_vce_Range, "drop_vce_Range.xlsx")

#custcare_Mean
telecom %>% mutate( dec = ntile(custcare_Mean,n=2))->custcare_Mean
custcare_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(custcare_Mean),LessThan=max(custcare_Mean)) -> custcare_Mean
custcare_Mean$varname<-rep("custcare_Mean",nrow(custcare_Mean))
write.xlsx(custcare_Mean, "custcare_Mean.xlsx")

#ccrndmou_Range
telecom %>% mutate( dec = ntile(ccrndmou_Range,n=2))->ccrndmou_Range
ccrndmou_Range %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(ccrndmou_Range),LessThan=max(ccrndmou_Range)) -> ccrndmou_Range
ccrndmou_Range$varname<-rep("ccrndmou_Range",nrow(ccrndmou_Range))
write.xlsx(ccrndmou_Range, "ccrndmou_Range.xlsx")

#comp_vce_Mean
telecom %>% mutate( dec = ntile(comp_vce_Mean,n=10))->comp_vce_Mean
comp_vce_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(comp_vce_Mean),LessThan=max(comp_vce_Mean)) -> comp_vce_Mean
comp_vce_Mean$varname<-rep("comp_vce_Mean",nrow(comp_vce_Mean))
write.xlsx(comp_vce_Mean, "comp_vce_Mean.xlsx")

#plcd_vce_Mean
telecom %>% mutate( dec = ntile(plcd_vce_Mean,n=10))->plcd_vce_Mean
plcd_vce_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(plcd_vce_Mean),LessThan=max(plcd_vce_Mean)) -> plcd_vce_Mean
plcd_vce_Mean$varname<-rep("plcd_vce_Mean",nrow(plcd_vce_Mean))
write.xlsx(plcd_vce_Mean, "plcd_vce_Mean.xlsx")

#roam_Mean
telecom %>% mutate( dec = ntile(roam_Mean,n=2))->roam_Mean
roam_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(roam_Mean),LessThan=max(roam_Mean)) -> roam_Mean
roam_Mean$varname<-rep("roam_Mean",nrow(roam_Mean))
write.xlsx(roam_Mean, "roam_Mean.xlsx")

#recv_sms_Mean
telecom %>% mutate( dec = ntile(recv_sms_Mean,n=2))->recv_sms_Mean
recv_sms_Mean %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(recv_sms_Mean),LessThan=max(recv_sms_Mean)) -> recv_sms_Mean
recv_sms_Mean$varname<-rep("recv_sms_Mean",nrow(recv_sms_Mean))
write.xlsx(recv_sms_Mean, "recv_sms_Mean.xlsx")

#Variable Profiling: Categorical
#set working directory
setwd("C:\\Jig12051\\Telecom Case Study\\Categorical Variable Profiles")

#crclscod
telecom %>% mutate( dec = ntile(crclscod,n=3))->crclscod
crclscod %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn)) -> crclscod
crclscod$varname<-rep("crclscod",nrow(crclscod))
write.xlsx(crclscod, "crclscod.xlsx")

#asl_flag
telecom %>% group_by(asl_flag) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn)) -> asl_flag
asl_flag$varname<-rep("asl_flag",nrow(asl_flag))
write.xlsx(asl_flag, "asl_flag.xlsx")

#prizm_social_one
telecom %>% group_by(prizm_social_one) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn)) -> prizm_social_one
prizm_social_one$varname<-rep("prizm_social_one",nrow(prizm_social_one))
write.xlsx(prizm_social_one, "C:\\Jig12051\\Telecom Case Study\\Categorical Variable Profiles\\prizm_social_one.xlsx")

#area
telecom %>% group_by(area) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn)) %>% arrange(ChurnRate) -> area
area$varname<-rep("area",nrow(area))
write.xlsx(area, "C:\\Jig12051\\Telecom Case Study\\Categorical Variable Profiles\\area.xlsx")

#marital
telecom %>% group_by(marital) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn)) -> marital
marital$varname<-rep("marital",nrow(marital))
write.xlsx(marital, "C:\\Jig12051\\Telecom Case Study\\Categorical Variable Profiles\\marital.xlsx")

#ethnic
#telecom %>% mutate( dec = ntile(ethnic,n=2))->ethnic
telecom %>% group_by(ethnic) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn)) %>% arrange(ChurnRate) -> ethnic
ethnic$varname<-rep("ethnic",nrow(ethnic))
write.xlsx(ethnic, "C:\\Jig12051\\Telecom Case Study\\Categorical Variable Profiles\\ethnic.xlsx")

#age1
telecom %>% mutate( dec = ntile(telecom$age1,n=3))->age1
age1 %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(age1),LessThan=max(age1)) -> age1
age1$varname<-rep("age1",nrow(age1))
write.xlsx(age1, "age1.xlsx")

#age2
telecom %>% mutate( dec = ntile(age2,n=2))->age2
age2 %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(age2),LessThan=max(age2)) -> age2
age2$varname<-rep("age2",nrow(age2))
write.xlsx(age2, "age2.xlsx")

#actvsubs
telecom$actvsubs <- as.factor(telecom$actvsubs)
telecom %>% group_by(actvsubs) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn))-> actvsubs
actvsubs$varname<-rep("actvsubs",nrow(actvsubs))
write.xlsx(actvsubs, "actvsubs.xlsx")

#uniqsubs
telecom$uniqsubs <- as.factor(telecom$uniqsubs)
telecom %>% group_by(uniqsubs) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn)) -> uniqsubs
uniqsubs$varname<-rep("uniqsubs",nrow(uniqsubs))
write.xlsx(uniqsubs, "uniqsubs.xlsx")

#hnd_webcap
telecom %>% group_by(hnd_webcap) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn)) -> hnd_webcap
hnd_webcap$varname<-rep("hnd_webcap",nrow(hnd_webcap))
write.xlsx(hnd_webcap, "hnd_webcap.xlsx")

#Variable Profiling: Derived
#set working directory
setwd("C:\\Jig12051\\Telecom Case Study\\Derived Variable Profiles")

#ovrmou_Percent
telecom %>% mutate( dec = ntile(ovrmou_Percent,n=2))->ovrmou_Percent
ovrmou_Percent %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(ovrmou_Percent),LessThan=max(ovrmou_Percent)) -> ovrmou_Percent
ovrmou_Percent$varname<-rep("ovrmou_Percent",nrow(ovrmou_Percent))
write.xlsx(ovrmou_Percent, "C:\\Jig12051\\Telecom Case Study\\Derived Variable Profiles\\ovrmou_Percent.xlsx")

#ovrrev_Percent
telecom %>% mutate( dec = ntile(ovrrev_Percent,n=2))->ovrrev_Percent
ovrrev_Percent %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(ovrrev_Percent),LessThan=max(ovrrev_Percent)) -> ovrrev_Percent
ovrrev_Percent$varname<-rep("ovrrev_Percent",nrow(ovrrev_Percent))
write.xlsx(ovrrev_Percent, "C:\\Jig12051\\Telecom Case Study\\Derived Variable Profiles\\ovrrev_Percent.xlsx")

#comp_vce_Percent
telecom %>% mutate( dec = ntile(comp_vce_Percent,n=10))->comp_vce_Percent
comp_vce_Percent %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(comp_vce_Percent),LessThan=max(comp_vce_Percent)) -> comp_vce_Percent
comp_vce_Percent$varname<-rep("comp_vce_Percent",nrow(comp_vce_Percent))
write.xlsx(comp_vce_Percent, "comp_vce_Percent.xlsx")

#comp_dat_Percent
telecom %>% mutate( dec = ntile(comp_dat_Percent,n=2))->comp_dat_Percent
comp_dat_Percent %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(comp_dat_Percent),LessThan=max(comp_dat_Percent)) -> comp_dat_Percent
comp_dat_Percent$varname<-rep("comp_dat_Percent",nrow(comp_dat_Percent))
write.xlsx(comp_dat_Percent, "comp_dat_Percent.xlsx")

#drop_vce_Percent
telecom %>% mutate( dec = ntile(drop_vce_Percent,n=10))->drop_vce_Percent
drop_vce_Percent %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(drop_vce_Percent),LessThan=max(drop_vce_Percent)) -> drop_vce_Percent
drop_vce_Percent$varname<-rep("drop_vce_Percent",nrow(drop_vce_Percent))
write.xlsx(drop_vce_Percent, "drop_vce_Percent.xlsx")

#drop_dat_Percent
telecom %>% mutate( dec = ntile(drop_dat_Percent,n=2))->drop_dat_Percent
drop_dat_Percent %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(drop_dat_Percent),LessThan=max(drop_dat_Percent)) -> drop_dat_Percent
drop_dat_Percent$varname<-rep("drop_dat_Percent",nrow(drop_dat_Percent))
write.xlsx(drop_dat_Percent, "drop_dat_Percent.xlsx")

#totmrc_Percent
telecom %>% mutate( dec = ntile(totmrc_Percent,n=10))->totmrc_Percent
totmrc_Percent %>% group_by(dec) %>% summarise(n= sum(churn),N=n(),ChurnRate= mean(churn),GreaterThan=min(totmrc_Percent),LessThan=max(totmrc_Percent)) -> totmrc_Percent
totmrc_Percent$varname<-rep("totmrc_Percent",nrow(totmrc_Percent))
write.xlsx(totmrc_Percent, "totmrc_Percent.xlsx")



