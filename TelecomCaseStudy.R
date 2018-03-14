library(dplyr)
library(gtools)
library(xlsx)
library(caret)
library(ROCR)


#set working directory
setwd("C:\\Jig12051\\Telecom Case Study")

telecom <- read.csv("C:/Data Science with R/Assignments/Graded Assignments/Topic 13 -  Final Case Study Course Wrap up/telecomfinal.csv")

telecom_bk <- telecom

summary(telecom)

#Deleting variables with large number of missing values
telecom %>% select(everything(),-income,-dwlltype,-dwllsize,-mailordr,-occu1,-numbcars,
                   -retdays,-wrkwoman,-solflag,-proptype,-mailresp,-cartype,
                   -children,-div_type) -> telecom

#Deleting 181 rows with NAs in mou_mean
telecom <- telecom[!is.na(telecom$mou_Mean),]

#Replacing NA in change_mou with mean 
telecom$change_mou <- ifelse(is.na(telecom$change_mou),mean(telecom$change_mou,na.rm = TRUE), as.numeric(telecom$change_mou))

#Replacing NA in avg6mou with mean 
telecom$avg6mou <- ifelse(is.na(telecom$avg6mou),mean(telecom$avg6mou,na.rm = TRUE), as.numeric(telecom$avg6mou))

#Replacing NA in avg6qty with mean 
telecom$avg6qty <- ifelse(is.na(telecom$avg6qty),mean(telecom$avg6qty,na.rm = TRUE), as.numeric(telecom$avg6qty))

#Replacing NA in eqpdays with mean
telecom$eqpdays <- ifelse(is.na(telecom$eqpdays),mean(telecom$eqpdays,na.rm = TRUE), as.numeric(telecom$eqpdays))

#Replacing NA in marital with S
telecom$marital <- ifelse(is.na(telecom$marital),'S',as.character(telecom$marital))
telecom$marital <- as.factor(telecom$marital)
summary(telecom$marital)

#Replacing NA in prizm_social_one with T
telecom$prizm_social_one <- ifelse(is.na(telecom$prizm_social_one),'T',as.character(telecom$prizm_social_one))
telecom$prizm_social_one <- as.factor(telecom$prizm_social_one)
summary(telecom$prizm_social_one)

#Replacing NA in ethnic with M
telecom$ethnic <- ifelse(is.na(telecom$ethnic),'M',as.character(telecom$ethnic))
telecom$ethnic <- as.factor(telecom$ethnic)
summary(telecom$ethnic)

#Replacing NA in refurb_new as N (randomly)
telecom$refurb_new <- ifelse(is.na(telecom$refurb_new),'N',as.character(telecom$refurb_new))
telecom$refurb_new <- as.factor(telecom$refurb_new)
summary(telecom$refurb_new)

#Replacing NA in area with OHIO AREA
telecom$area <- ifelse(is.na(telecom$area),'OHIO AREA',as.character(telecom$area))
telecom$area <- as.factor(telecom$area)
summary(telecom$area)


#Derived Variable
#overrage mou percent
telecom %>% mutate(ovrmou_Percent = ovrmou_Mean/mou_Mean) -> telecom

#overrage rev percent
telecom %>% mutate(ovrrev_Percent = ovrrev_Mean/rev_Mean) -> telecom

#Completed calls percent
#Voice
telecom %>% mutate(comp_vce_Percent = comp_vce_Mean/plcd_vce_Mean) -> telecom
#Data
telecom %>% mutate(comp_dat_Percent = comp_dat_Mean/plcd_dat_Mean) -> telecom

#Dropped calls percent
#Voice
telecom %>% mutate(drop_vce_Percent = drop_vce_Mean/plcd_vce_Mean) -> telecom
#Data
telecom %>% mutate(drop_dat_Percent = drop_dat_Mean/plcd_dat_Mean) -> telecom

#mMonthly recurring charges as percent of monthly revenue
telecom %>% mutate(totmrc_Percent = totmrc_Mean/rev_Mean) -> telecom

#Creating levels/dummy variables 

#crclscod
#telecom %>% mutate( crclscod_dummy = ntile(crclscod,n=3))->telecom
#telecom$crclscod_dummy <- as.factor(telecom$crclscod_dummy)
#Removing crclscod from data
#telecom <- select(telecom, -crclscod)

#asl_flag, N=0, Y=1
telecom$asl_flag <- ifelse(telecom$asl_flag=='N',0,1)
telecom$asl_flag <- as.factor(telecom$asl_flag)
#hnd_webcap
telecom <- mutate(telecom, hnd_webcap_dummy = ifelse(telecom$hnd_webcap=='WC' | telecom$hnd_webcap =='WCMB',1,0))
telecom$hnd_webcap_dummy <- ifelse(is.na(telecom$hnd_webcap_dummy),0,telecom$hnd_webcap)
#Removing hnd_web from data
telecom <- select(telecom, -hnd_webcap)

#Age1 into 3 levels
telecom <- mutate(telecom, age1_dummy = ntile(telecom$age1, n=3))
#NA has churn rate equivalent to group 3
telecom$age1_dummy <- ifelse(is.na(telecom$age1_dummy), 3, telecom$age1_dummy)
telecom$age1_dummy <- as.factor(telecom$age1_dummy)
summary(telecom$age1_dummy)
#Removing age1 from data
telecom <- select(telecom, -age1)

#actvsubs into 3 groups
telecom <- mutate(telecom, actvsubs_dummy = 
                    ifelse(telecom$actvsubs==7 | telecom$actvsubs==11 ,1,
                           ifelse(telecom$actvsubs==8,3,2) ))
telecom$actvsubs_dummy <- as.factor(telecom$actvsubs_dummy)
#Removing actvsubs from data
telecom <- select(telecom, -actvsubs)

#uniqsubs into 3 groups
telecom <- mutate(telecom, uniqsubs_dummy = 
                    ifelse(telecom$uniqsubs==13,3,
                           ifelse(telecom$uniqsubs==7 | telecom$uniqsubs==8 | telecom$uniqsubs==9 | telecom$uniqsubs==10,2,1) ))
telecom$uniqsubs_dummy <- as.factor(telecom$uniqsubs_dummy)
#Removing uniqsubs from data
telecom <- select(telecom, -uniqsubs)
summary(telecom$uniqsubs_dummy)

#Marital - merging A, B and U
telecom <- telecom %>% mutate(marital_dummy = 
                                ifelse(telecom$marital=='A' | telecom$marital=='B','U',as.character(telecom$marital)))
telecom$marital_dummy <- as.factor(telecom$marital_dummy)
telecom <- select(telecom, - marital)

#prizm_social_one - C(C,U,S ) n R(R,T)
telecom <- telecom %>% mutate(prizm_social_one_dummy = 
                                ifelse(telecom$prizm_social_one=='R' | telecom$prizm_social_one=='T','R','C'))
telecom$prizm_social_one_dummy <- as.factor(telecom$prizm_social_one_dummy)
telecom <- select(telecom, - prizm_social_one)

#ethnic 1(C,Z,P,X) 3(I,J,D,B,O) 2(M,N,S,U,R,H,G,F)
telecom <- telecom %>% mutate(ethnic_dummy = 
                                ifelse(ethnic=='C'|ethnic=='Z'|ethnic=='P'|ethnic=='X',1,
                              ifelse(ethnic=='I'|ethnic=='J'|ethnic=='D'|ethnic=='B'|ethnic=='O',3,2)))
telecom$ethnic_dummy <- as.factor(telecom$ethnic_dummy)
telecom <- select(telecom, -ethnic)

#area into 3 groups
telecom <- telecom %>% mutate(area_dummy = 
                                ifelse(area=='CALIFORNIA NORTH AREA'|area=='NEW ENGLAND AREA'|area=='SOUTH FLORIDA AREA'|area=='NORTHWEST/ROCKY MOUNTAIN AREA',3,
                              ifelse(area=='TENNESSEE AREA'|area=='MIDWEST AREA'|area=='CENTRAL/SOUTH TEXAS AREA'|area=='HOUSTON AREA'|area=='OHIO AREA'|area=='DC/MARYLAND/VIRGINIA AREA'|area=='GREAT LAKES AREA',1,2)))
telecom$area_dummy <- as.factor(telecom$area_dummy)
telecom <- select(telecom, -area)

#Creating dummy for derived variables

#comp_dat_Percent into 3 groups
telecom<- telecom %>% mutate( comp_dat_Percent_dummy = ntile(comp_dat_Percent,n=2))
telecom$comp_dat_Percent_dummy <- ifelse(is.na(telecom$comp_dat_Percent_dummy),3, (telecom$comp_dat_Percent_dummy))
telecom$comp_dat_Percent_dummy <- as.factor(telecom$comp_dat_Percent_dummy)
#Removing comp_dat_Percent from data
telecom <- select(telecom, -comp_dat_Percent)

#comp_vce_Percent into 3 groups
telecom <- telecom %>% mutate( comp_vce_Percent_dummy = ntile(comp_vce_Percent,n=10))
telecom$comp_vce_Percent_dummy <- ifelse(telecom$comp_vce_Percent_dummy !=1,2,telecom$comp_vce_Percent_dummy)
telecom$comp_vce_Percent_dummy <- ifelse(is.na(telecom$comp_vce_Percent_dummy),3,telecom$comp_vce_Percent_dummy)                   
telecom$comp_vce_Percent_dummy <- as.factor(telecom$comp_vce_Percent_dummy)
#Removing comp_vce_Percent from data
telecom <- select(telecom, -comp_vce_Percent)

#drop_dat_Percent into 3 groups
telecom<- telecom %>% mutate( drop_dat_Percent_dummy = ntile(drop_dat_Percent,n=2))
telecom$drop_dat_Percent_dummy <- ifelse(is.na(telecom$drop_dat_Percent_dummy),3, (telecom$drop_dat_Percent_dummy))
telecom$drop_dat_Percent_dummy <- as.factor(telecom$drop_dat_Percent_dummy)
summary(telecom$drop_dat_Percent_dummy)
#Removing drop_dat_Percent from data
telecom <- select(telecom, -drop_dat_Percent)

#drop_vce_Percent into 3 groups
telecom <- telecom %>% mutate( drop_vce_Percent_dummy = ntile(drop_vce_Percent,n=10))
telecom$drop_vce_Percent_dummy <- ifelse(telecom$drop_vce_Percent_dummy ==10,2,1)
telecom$drop_vce_Percent_dummy <- ifelse(is.na(telecom$drop_vce_Percent_dummy),3,telecom$drop_vce_Percent_dummy)                   
telecom$drop_vce_Percent_dummy <- as.factor(telecom$drop_vce_Percent_dummy)
#Removing drop_vce_Percent from data
telecom <- select(telecom, -drop_vce_Percent)

#ovrmou_Percent into 3 groups
telecom <- telecom %>% mutate( ovrmou_Percent_dummy = ntile(ovrmou_Percent,n=2))
telecom$ovrmou_Percent_dummy <- ifelse(is.na(telecom$ovrmou_Percent_dummy),3, (telecom$ovrmou_Percent_dummy))
telecom$ovrmou_Percent_dummy <- as.factor(telecom$ovrmou_Percent_dummy)
#Removing ovrmou_Percent from data
telecom <- select(telecom, -ovrmou_Percent)

#ovrrev_Percent into 3 groups
telecom <- telecom %>% mutate( ovrrev_Percent_dummy = ntile(ovrrev_Percent,n=2))
telecom$ovrrev_Percent_dummy <- ifelse(is.na(telecom$ovrrev_Percent_dummy),3, (telecom$ovrrev_Percent_dummy))
telecom$ovrrev_Percent_dummy <- as.factor(telecom$ovrrev_Percent_dummy)
#Removing ovrrev_Percent from data
telecom <- select(telecom, -ovrrev_Percent)

#totmrc_Percent into 3 groups
telecom <- telecom %>% mutate( totmrc_Percent_dummy = ntile(totmrc_Percent,n=10))
telecom$totmrc_Percent_dummy <- ifelse(telecom$totmrc_Percent_dummy==1 | telecom$totmrc_Percent_dummy==2,
                                       1, (telecom$totmrc_Percent_dummy))
telecom$totmrc_Percent_dummy <- ifelse(telecom$totmrc_Percent_dummy>=3 & telecom$totmrc_Percent_dummy<=7,
                                       2, (telecom$totmrc_Percent_dummy))
telecom$totmrc_Percent_dummy <- ifelse(telecom$totmrc_Percent_dummy>=8 & telecom$totmrc_Percent_dummy<=10,
                                       3, (telecom$totmrc_Percent_dummy))
telecom$totmrc_Percent_dummy <- ifelse(is.na(telecom$totmrc_Percent_dummy), 3, (telecom$totmrc_Percent_dummy))
telecom$totmrc_Percent_dummy <- as.factor(telecom$totmrc_Percent_dummy)

#Removing ovrrev_Percent from data
telecom <- select(telecom, -totmrc_Percent)

#creating dummy for cont. variables

#change_mou
telecom <- telecom %>% mutate( change_mou_dummy = ntile(change_mou,n=10))
telecom$change_mou_dummy <- ifelse(telecom$change_mou_dummy>6,2,1)
telecom$change_mou_dummy <- as.factor(telecom$change_mou_dummy)

#avg3mou
telecom <- telecom %>% mutate( avg3mou_dummy = ntile(avg3mou,n=10))
telecom$avg3mou_dummy <- ifelse(telecom$avg3mou_dummy>1 & telecom$avg3mou_dummy<=5,2,telecom$avg3mou_dummy)
telecom$avg3mou_dummy <- ifelse(telecom$avg3mou_dummy>5,3,telecom$avg3mou_dummy)
telecom$avg3mou_dummy <- as.factor(telecom$avg3mou_dummy)

#months
telecom <- telecom %>% mutate( months_dummy = ntile(months,n=10))
telecom$months_dummy <- ifelse(telecom$months_dummy ==1 | telecom$months_dummy ==2,1,telecom$months_dummy)
telecom$months_dummy <- ifelse(telecom$months_dummy ==3 | telecom$months_dummy ==4,2,telecom$months_dummy)
telecom$months_dummy <- ifelse(telecom$months_dummy >=5,3,telecom$months_dummy)
telecom$months_dummy <- as.factor(telecom$months_dummy)


#csa car_buy truck mtrcycle forgntvl hnd_price age2 models crclscod

telecom <- select(telecom, -csa, -car_buy, -truck, -mtrcycle, -forgntvl,-hnd_price,-age2, -models, -crclscod)
summary(telecom)
colSums(is.na(telecom))

#Copying clean Telecom data in csv file
write.csv(telecom, "Telecom_Updated.csv",row.names = FALSE)

#telecom %>% select(blck_dat_Mean, drop_dat_Mean,comp_dat_Mean,mou_pead_Mean,datovr_Mean,plcd_dat_Mean) ->tel_dat

#telecom %>% select(months,datovr_Mean,ovrrev_Mean,rev_Mean,totmrc_Mean,adjrev,avgrev,totrev) ->tel_rev

#telecom %>% select(months,mou_Mean,ovrmou_Mean,datovr_Mean,ovrrev_Mean) ->tel_ovr

#Splitting into test and train samples(40%-60%)
set.seed(150)
index <- sample(nrow(telecom),0.60*nrow(telecom),replace = F)
train <- telecom[index,]
test <- telecom[-index,]

nrow(train)
nrow(test)

table(telecom$churn)
table(train$churn)
table(test$churn)
table(telecom$churn)/nrow(telecom)
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

mod <- glm(churn~mou_Mean+change_mou+months+avg3mou+avg3qty+avg6mou+avg6qty+
              comp_vce_Mean+plcd_vce_Mean+comp_dat_Mean+plcd_dat_Mean+
              totmrc_Mean+custcare_Mean+
              drop_vce_Mean+drop_dat_Mean+
              ovrmou_Mean+ovrrev_Mean+
              rev_Mean+totrev+avgrev 
            , family = "binomial", data=train)
summary(mod)
#step(mod,direction = "both")

mod_step <- glm(churn ~ change_mou + months + avg3mou + avg6mou + 
                  comp_vce_Mean + plcd_vce_Mean + totmrc_Mean + custcare_Mean + 
                  drop_vce_Mean + ovrmou_Mean + rev_Mean + avgrev, family = "binomial", data = train)
summary(mod_step)

mod_step_b <- glm(churn ~ mou_Mean + change_mou + months + avg6mou + 
                  comp_vce_Mean + plcd_vce_Mean + totmrc_Mean + custcare_Mean + 
                  drop_vce_Mean + ovrmou_Mean + ovrrev_Mean + rev_Mean + totrev + 
                  avgrev, family = "binomial", data = train)
summary(mod_step)

#adding comp_vce_Percent_dummy +comp_dat_Percent_dummy + 
#drop_vce_Percent_dummy + drop_dat_Percent_dummy+
#ovrmou_Percent_dummy + ovrrev_Percent_dummy
mod1 <- glm(churn~ change_mou + months + avg3mou + avg6mou + 
              comp_vce_Mean + plcd_vce_Mean + totmrc_Mean + custcare_Mean + 
              drop_vce_Mean + ovrmou_Mean + rev_Mean + avgrev +
              comp_vce_Percent_dummy +comp_dat_Percent_dummy + 
              drop_vce_Percent_dummy + drop_dat_Percent_dummy+
              ovrmou_Percent_dummy + ovrrev_Percent_dummy
            , family = "binomial", data=train)
summary(mod1)

mod1_b <- glm(churn~ mou_Mean + change_mou + months + avg6mou + 
                  comp_vce_Mean + plcd_vce_Mean + totmrc_Mean + custcare_Mean + 
                  drop_vce_Mean + ovrmou_Mean + ovrrev_Mean + rev_Mean + totrev + 
                  avgrev +
              comp_vce_Percent_dummy +comp_dat_Percent_dummy + 
              drop_vce_Percent_dummy + drop_dat_Percent_dummy+
              ovrmou_Percent_dummy + ovrrev_Percent_dummy
            , family = "binomial", data=train)
summary(mod1)
#step(mod1,direction = "both")

mod1_step <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
                   custcare_Mean + drop_vce_Mean + rev_Mean + avgrev + comp_vce_Percent_dummy + 
                   comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
                   ovrmou_Percent_dummy, 
                 family = "binomial", data = train)
summary(mod1_step)

mod1_step_b <- glm(churn ~ mou_Mean + change_mou + months + avg6mou + 
                   totmrc_Mean + custcare_Mean + drop_vce_Mean + ovrmou_Mean + 
                   ovrrev_Mean + rev_Mean + totrev + avgrev + 
                   comp_vce_Percent_dummy + 
                   comp_dat_Percent_dummy + drop_vce_Percent_dummy + 
                   drop_dat_Percent_dummy + 
                   ovrmou_Percent_dummy, 
                 family = "binomial", data = train)
summary(mod1_step)

#adding totmrc_Percent_dummy
mod2 <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
                custcare_Mean + drop_vce_Mean + rev_Mean + avgrev + comp_vce_Percent_dummy + 
                comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
                ovrmou_Percent_dummy +
                totmrc_Percent_dummy,
              family = "binomial", data=train)
summary(mod2)

mod2_b <- glm(churn~mou_Mean + change_mou + months + avg6mou + 
                   totmrc_Mean + custcare_Mean + drop_vce_Mean + ovrmou_Mean + 
                   ovrrev_Mean + rev_Mean + totrev + avgrev + 
                   comp_vce_Percent_dummy + 
                   comp_dat_Percent_dummy + drop_vce_Percent_dummy + 
                   drop_dat_Percent_dummy + 
                   ovrmou_Percent_dummy +
              totmrc_Percent_dummy,
            family = "binomial", data=train)
summary(mod2)
#step(mod2,direction = "both")

#adding change_mou_dummy + avg3mou_dummy + months_dummy
mod3 <- glm(churn~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
              custcare_Mean + drop_vce_Mean + rev_Mean + avgrev + comp_vce_Percent_dummy + 
              comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
              ovrmou_Percent_dummy +
              totmrc_Percent_dummy +
                change_mou_dummy + avg3mou_dummy + months_dummy
              , family = "binomial", data=train)
summary(mod3)

mod3_b <- glm(churn~ mou_Mean + change_mou + months + avg6mou + 
              totmrc_Mean + custcare_Mean + drop_vce_Mean + ovrmou_Mean + 
              ovrrev_Mean + rev_Mean + totrev + avgrev + 
              comp_vce_Percent_dummy + comp_dat_Percent_dummy + 
              drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
              ovrmou_Percent_dummy + totmrc_Percent_dummy +
              change_mou_dummy + avg3mou_dummy + months_dummy
            , family = "binomial", data=train)
summary(mod3)
#step(mod3,direction = "both")

mod3_step_b <- glm(formula = churn ~ mou_Mean + months + avg6mou + totmrc_Mean + 
                   custcare_Mean + drop_vce_Mean + ovrmou_Mean + ovrrev_Mean + 
                   rev_Mean + totrev + avgrev + comp_vce_Percent_dummy + comp_dat_Percent_dummy + 
                   drop_vce_Percent_dummy + drop_dat_Percent_dummy + ovrmou_Percent_dummy + 
                   totmrc_Percent_dummy + change_mou_dummy + avg3mou_dummy + 
                   months_dummy, family = "binomial", data = train)
summary(mod3_step)

#removing change_mou + months 
mod4_b <- glm(churn~ mou_Mean + avg6mou + 
              totmrc_Mean + custcare_Mean + drop_vce_Mean + ovrmou_Mean + 
              ovrrev_Mean + rev_Mean + totrev + avgrev + 
              comp_vce_Percent_dummy + comp_dat_Percent_dummy + 
              drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
              ovrmou_Percent_dummy + totmrc_Percent_dummy +
              change_mou_dummy + avg3mou_dummy + months_dummy
            , family = "binomial", data=train)
summary(mod4)
#step(mod4,direction = "both")

mod4_step_b <- glm(formula = churn ~ mou_Mean + avg6mou + totmrc_Mean + custcare_Mean + 
    drop_vce_Mean + rev_Mean + avgrev + comp_vce_Percent_dummy + 
    comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
    ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
    avg3mou_dummy + months_dummy, family = "binomial", data = train)
summary(mod4_step)

#adding asl_flag + hnd_webcap_dummy +age1_dummy +actvsubs_dummy +uniqsubs_dummy
mod5 <- glm(churn~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
              custcare_Mean + drop_vce_Mean + rev_Mean + avgrev + comp_vce_Percent_dummy + 
              comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
              ovrmou_Percent_dummy +
              totmrc_Percent_dummy +
              change_mou_dummy + avg3mou_dummy + months_dummy + asl_flag + hnd_webcap_dummy + age1_dummy +
              actvsubs_dummy + uniqsubs_dummy
            , family = "binomial", data=train)
summary(mod5)

mod5_b <- glm(churn~ mou_Mean + months + avg6mou + totmrc_Mean + 
                   custcare_Mean + drop_vce_Mean + ovrmou_Mean + ovrrev_Mean + 
                   rev_Mean + totrev + avgrev + comp_vce_Percent_dummy + comp_dat_Percent_dummy + 
                   drop_vce_Percent_dummy + drop_dat_Percent_dummy + ovrmou_Percent_dummy + 
                   totmrc_Percent_dummy + change_mou_dummy + avg3mou_dummy + 
                   months_dummy + asl_flag + hnd_webcap_dummy + age1_dummy +
              actvsubs_dummy + uniqsubs_dummy
            , family = "binomial", data=train)
summary(mod5)
#step(mod5,direction = "both")
#step(mod5,direction = "backward")

mod5_step <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
                   drop_vce_Mean + rev_Mean + avgrev + comp_vce_Percent_dummy + 
                   comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
                   ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
                   avg3mou_dummy + months_dummy + asl_flag + hnd_webcap_dummy + 
                   age1_dummy + actvsubs_dummy + uniqsubs_dummy
            , family = "binomial", data=train)
summary(mod5_step)

mod5_step_b <- glm(formula = churn ~ mou_Mean + months + avg6mou + totmrc_Mean + 
    custcare_Mean + drop_vce_Mean + ovrmou_Mean + ovrrev_Mean + 
    rev_Mean + totrev + avgrev + comp_vce_Percent_dummy + comp_dat_Percent_dummy + 
    drop_vce_Percent_dummy + drop_dat_Percent_dummy + ovrmou_Percent_dummy + 
    totmrc_Percent_dummy + change_mou_dummy + avg3mou_dummy + 
    months_dummy + asl_flag + hnd_webcap_dummy + age1_dummy + 
    actvsubs_dummy + uniqsubs_dummy, family = "binomial", data = train)
summary(mod5_step)

# Removing asl_flag + hnd_webcap_dummy from mod 5
mod6 <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
              drop_vce_Mean + rev_Mean + avgrev + comp_vce_Percent_dummy + 
              comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
              ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
              avg3mou_dummy + months_dummy  + 
              age1_dummy
                 , family = "binomial", data=train)
summary(mod6)
step(mod6,direction = "both")

#removing avgrev from mod6
mod7 <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
              drop_vce_Mean + rev_Mean + comp_vce_Percent_dummy + 
              comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
              ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
              avg3mou_dummy + months_dummy  + 
              age1_dummy
            , family = "binomial", data=train)
summary(mod7)


#adding totcalls+ mou_pead_Mean + opk_dat_Mean to mod7
mod8 <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
              drop_vce_Mean + rev_Mean + comp_vce_Percent_dummy + 
              comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
              ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
              avg3mou_dummy + months_dummy  + 
              age1_dummy+ totcalls + mou_pead_Mean + opk_dat_Mean
              , family = "binomial", data = train)
summary(mod8)

#adding drop_blk_Mean + iwylis_vce_Mean + owylis_vce_Range to mod7
mod9 <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
               drop_vce_Mean + rev_Mean + comp_vce_Percent_dummy + 
               comp_dat_Percent_dummy + drop_vce_Percent_dummy + drop_dat_Percent_dummy + 
               ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
               avg3mou_dummy + months_dummy  + 
               age1_dummy + drop_blk_Mean + iwylis_vce_Mean + owylis_vce_Range
              , family = "binomial", data = train)
summary(mod9)


#Creating dummies for significant levels(from mod 7)
telecom <- telecom %>% mutate(drop_dat_Percent_dummy2 = ifelse(drop_dat_Percent_dummy == 2,1,0))
telecom <- select(telecom, -drop_dat_Percent_dummy)

telecom <-  telecom %>% mutate(comp_vce_Percent_dummy2 = ifelse(comp_vce_Percent_dummy == 2,1,0))
telecom <- select(telecom,-comp_vce_Percent_dummy)

telecom <-  telecom %>% mutate(drop_vce_Percent_dummy2 = ifelse(drop_vce_Percent_dummy == 2,1,0))
telecom <- select(telecom,-drop_vce_Percent_dummy)

#Splitting into test and train samples(40%-60%)
set.seed(150)
index <- sample(nrow(telecom),0.60*nrow(telecom),replace = F)
train <- telecom[index,]
test <- telecom[-index,]

nrow(train)
nrow(test)
#replacing dummy variables in mod7
mod10 <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
              drop_vce_Mean + rev_Mean + comp_vce_Percent_dummy2 + 
              comp_dat_Percent_dummy + drop_vce_Percent_dummy2 + drop_dat_Percent_dummy2 + 
              ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
              avg3mou_dummy + months_dummy  + 
              age1_dummy
            , family = "binomial", data=train)
summary(mod10)
                  

# adding eqpdays marital_dummy prizm_social_one_dummy ethnic_dummy refurb_new area_dummy
mod11 <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
               drop_vce_Mean + rev_Mean + comp_vce_Percent_dummy2 + 
               comp_dat_Percent_dummy + drop_vce_Percent_dummy2 + drop_dat_Percent_dummy2 + 
               ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
               avg3mou_dummy + months_dummy  + 
               age1_dummy +
              eqpdays + marital_dummy + prizm_social_one_dummy + ethnic_dummy + refurb_new + area_dummy
              , family = "binomial", data = train)
summary(mod11)

#Removing insignificant variables from mod11 (marital_dummy)
mod12 <- glm(churn ~ change_mou + avg3mou + avg6mou + totmrc_Mean + 
               drop_vce_Mean + rev_Mean + comp_vce_Percent_dummy2 + 
               comp_dat_Percent_dummy + drop_vce_Percent_dummy2 + drop_dat_Percent_dummy2 + 
               ovrmou_Percent_dummy + totmrc_Percent_dummy + change_mou_dummy + 
               avg3mou_dummy + months_dummy  + 
               age1_dummy +
               eqpdays + prizm_social_one_dummy + ethnic_dummy + refurb_new + area_dummy
             , family = "binomial", data = train)
summary(mod12)
#step(mod13,direction = "both")

#Final model
mod13 <- glm(churn ~ avg3mou + avg6mou + totmrc_Mean + 
               drop_vce_Mean + rev_Mean + comp_vce_Percent_dummy2 + 
               comp_dat_Percent_dummy + drop_vce_Percent_dummy2 + drop_dat_Percent_dummy2 + 
               ovrmou_Percent_dummy + totmrc_Percent_dummy  + 
              months_dummy  + change_mou_dummy+
               age1_dummy +
               eqpdays + prizm_social_one_dummy + ethnic_dummy + refurb_new + area_dummy
             , family = "binomial", data = train)
summary(mod13)

#Predicting the values from model on train 
predicted1 <- as.data.frame(predict(mod13, type = "response", data = train))
predicted1 <- ifelse(predicted1>=0.2388,1,0)

#Predicting the values from model on test
predicted2 <- as.data.frame(predict(mod13, type = "response", newdata = test))
predicted2 <- ifelse(predicted2>=0.2388,1,0)

#Validations

#Confusion Matrix
confusionMatrix(predicted1,train$churn, positive = "1")
confusionMatrix(predicted2,test$churn, positive = "1")

#Gains Chart
#GainsChart <- gains(as.numeric(test$X23_Target),predict(mod3,type="response",newdata=test),groups = 10)


pred<-prediction(predicted2,test$churn)

#ROCR Curve
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

#Proactive target customer base:
final_model <- glm(churn ~ avg3mou + avg6mou + totmrc_Mean + 
               drop_vce_Mean + rev_Mean + comp_vce_Percent_dummy2 + 
               comp_dat_Percent_dummy + drop_vce_Percent_dummy2 + drop_dat_Percent_dummy2 + 
               ovrmou_Percent_dummy + totmrc_Percent_dummy  + 
              months_dummy  + change_mou_dummy+
               age1_dummy +
               eqpdays + prizm_social_one_dummy + ethnic_dummy + refurb_new + area_dummy
             , family = "binomial", data = telecom)
summary(final_model)

# Merging the fitted values and telecom dataset
mod_fit <- data.frame(fitted_values= final_model$fitted.values)
telecom_fit <- cbind(telecom, mod_fit)
telecom_fit<- telecom_fit %>% mutate(probability_of_churn_grp=ntile(fitted_values,n=3))
telecom_fit<- telecom_fit %>% mutate(revenue_grp = ntile(totrev, n= 3))
#Filtering the customers for proactive retention campaigns
telecom_fit %>% filter(probability_of_churn_grp==3 & revenue_grp == 2) -> Cust1
telecom_fit %>% filter(probability_of_churn_grp==2 & revenue_grp == 3) -> Cust2
telecom_fit %>% filter(probability_of_churn_grp==3 & revenue_grp == 3) -> Cust3
telecom_target_segment <- rbind(Cust1,Cust2,Cust3)

#Writing telecom target population in csv file
write.csv(telecom_target_segment, "Telecom_Target_Segment.csv",row.names = FALSE)
