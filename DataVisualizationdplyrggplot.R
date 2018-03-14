#Inallling packages
library(dplyr)
library(ggplot2)
library(XML)
library(RCurl)

#Getting the content from Url

htmlContent <- getURL("file://The World's Most Valuable Brands List - Forbes.html", ssl.verifyPeer = FALSE)
htmlParse <- htmlTreeParse(htmlContent, useInternal = TRUE)

#Extraction table data from html
output <- unlist(xpathApply(htmlParse,"//*/table[@id='the_list']/tbody/tr/td",xmlValue))

#Converting data to Data Frame
content <- as.data.frame(matrix(output, ncol =8, byrow = TRUE))

#Dropping 1st column
content$V1 <- NULL

colnames(content) = c("Rank","Brand","BrandValue","1-YrValueChange","BrandRevenue","CompanyAdvertising","Industry")

dim(content) 
str(content)
summary(content)

#Cleaning the data

content$Rank <- as.numeric(gsub("#","",content$Rank))


content$BrandValue <- gsub("\\$","",content$BrandValue)
#Checking for values in M
grep("M",content$BrandValue)
content$BrandValue <- as.numeric(gsub("B","",content$BrandValue))


content$`1-YrValueChange` <-as.numeric(gsub("%","",content$`1-YrValueChange`))


#Checking for values in M
grep("M",content$BrandRevenue)
content$BrandRevenue <- gsub("\\$","", content$BrandRevenue)
content$BrandRevenue <- as.numeric(gsub("B","", content$BrandRevenue))


content$CompanyAdvertising <- gsub("\\$","", content$CompanyAdvertising)
valueInMillions <- grep("M",content$CompanyAdvertising)
content$CompanyAdvertising <- gsub("M","", content$CompanyAdvertising)
content$CompanyAdvertising <- as.numeric(gsub("B","", content$CompanyAdvertising))
#Converting values in millions to billions
content$CompanyAdvertising[valueInMillions] = content$CompanyAdvertising[valueInMillions]/1000

#Plotting the grapghs using ggplot2

#Plot for Technology sector
datTech<- content%>% filter(Industry=="Technology")

ggplot(datTech, aes(x = CompanyAdvertising,y=BrandRevenue)) + 
  theme_light()+ 
  theme(text = element_text(family = "Times",size = 12), 
        legend.key = element_rect(fill="light blue", color = "black"),
        legend.key.height = unit("0.5","cm"),
        legend.key.width = unit("0.5","cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  geom_point(aes(color= Brand, size=BrandValue)) +
  scale_size_continuous(range = c(2,6), breaks = c(30,60,100))+
  geom_text(aes(label=Brand, color = Brand, size = BrandValue), nudge_y = -3, family="Times")+
  guides(color= F,size = guide_legend(title = "Brand Values $ (Billions)"))+xlab("Company Advertising in Billions of $")+
  ylab("Brand Revenue in Billions of $")+ ggtitle("Technology")

#Plot for Luxury Sector
datLux<- content%>% filter(Industry=="Luxury")

ggplot(datLux, aes(x = CompanyAdvertising,y=BrandRevenue)) + 
  theme_light()+ 
  theme(text = element_text(family = "Times",size = 12), 
        legend.key = element_rect(fill="light blue", color = "black"),
        legend.key.height = unit("0.5","cm"),
        legend.key.width = unit("0.5","cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  geom_point(aes(color= Brand, size=BrandValue)) +
  scale_size_continuous(range = c(3,5), breaks = c(10,28.1))+
  geom_text(aes(label=Brand, color = Brand, size = BrandValue), nudge_y = -0.10, family="Times", fontface ="bold")+
  scale_x_continuous(breaks=seq(0.0,5.0,0.1))+
  scale_y_continuous(breaks=seq(4,10,2))+
  guides(color= F, size = guide_legend(title = "Brand Values $ (Billions)"))+xlab("Company Advertising in Billions of $")+
  ylab("Brand Revenue in Billions of $") +ggtitle("Luxury")

#Plot for financial services sector
datFin<- content%>% filter(Industry=="Financial Services")

ggplot(datFin, aes(x = CompanyAdvertising,y=BrandRevenue)) + 
  theme_light()+ 
  theme(text = element_text(family = "Times",size = 12), 
        legend.key = element_rect(fill="light blue", color = "black"),
        legend.key.height = unit("0.5","cm"),
        legend.key.width = unit("0.5","cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  geom_point(aes(color= Brand, size=BrandValue)) +
  scale_size_continuous(range = c(3,5), breaks = c(7.0,12.0,23.4))+
  geom_text(aes(label=Brand, color = Brand, size = BrandValue), nudge_y = -1, family="Times")+
  scale_x_continuous(breaks=seq(0.6,3.5,0.1))+
  scale_y_continuous(breaks=seq(10,90,10))+
  guides(color= F, size = guide_legend(title = "Brand Values $ (Billions)"))+xlab("Company Advertising in Billions of $")+
  ylab("Brand Revenue in Billions of $") +ggtitle("Financial Services")

#Plot for Aotomotive sector
datAuto<- content%>% filter(Industry=="Automotive")

ggplot(datAuto, aes(x = CompanyAdvertising,y=BrandRevenue)) + 
  theme_light()+ 
  theme(text = element_text(family = "Times",size = 12), 
        legend.key = element_rect(fill="light blue", color = "black"),
        legend.key.height = unit("0.5","cm"),
        legend.key.width = unit("0.5","cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  geom_point(aes(color= Brand, size=BrandValue)) +
  scale_size_continuous(range = c(3,5), breaks = c(6.2,20.0,37.8))+
  geom_text(aes(label=Brand, color = Brand, size = BrandValue), nudge_y = -1, family="Times")+
  scale_x_continuous(breaks=seq(0.8,5.4,0.1))+
  scale_y_continuous(breaks=seq(40,170,10))+
  guides(color= F, size = guide_legend(title = "Brand Values $ (Billions)"))+xlab("Company Advertising in Billions of $")+
  ylab("Brand Revenue in Billions of $") +ggtitle("Automotive")
