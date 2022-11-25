

##Set working directory
setwd()

library(ggplot2)
library(stringr)

##Load the data

Softdata <- read.csv("C:/Users/aborthakur/Desktop/Office docs/DS-CoE/R session/Data-R/softdata.csv",na.strings="")

##Get summary statistics

summary(Softdata)

##String cleaning and subsetting######

names<-data.frame(unlist(str_split_fixed(Softdata$Current.C.Suite.Role,"/",4)))
check<-data.frame(str_count(Softdata$Current.Company,"LP$"))
check<-data.frame(str_count(Softdata$Current.Company,"^HSBC"))
#check<-data.frame(str_extract(Softdata$Current.Company,"LP$"))
check<-data.frame(str_subset(Softdata$Current.Company,"LP$"))
check<-data.frame(str_subset(Softdata$Current.Company,"(?<!L)LP$"))



##Count the number of countries###
table(Softdata$Current.Company.Country)

###Arrange it in a data frame###
table_country<-as.data.frame(table(Softdata$Current.Company.Country))

## Sort the data in decreasing order###
table_country<-table_country[order(-table_country$Freq),]

##Change the column names##
colnames(table_country)<-c("Countries","Freq")

##Remove the extra row column##
row.names(table_country)<-NULL


##Count the designation##
table(Softdata$Current.C.Suite.Role)
table_designation<-as.data.frame(table(Softdata$Current.C.Suite.Role))
table_designation<-table_designation[order(-table_designation$Freq),]
colnames(table_designation)<-c("Clevel","Freq")
row.names(table_designation)<-NULL

###Count organisation###
table(Softdata$Organisation)
table_organisation<-as.data.frame(table(Softdata$Organisation))
table_organisation<-table_organisation[order(-table_organisation$Freq),]
colnames(table_organisation)<-c("organisation","Freq")
row.names(table_organisation)<-NULL

##Count Company Type###
table(Softdata$Company.Type)
table_companytype<-as.data.frame(table(Softdata$Company.Type))
table_companytype<-table_companytype[order(-table_companytype$Freq),]
colnames(table_companytype)<-c("companytype","Freq")
row.names(table_companytype)<-NULL

##Count the Countries###
table(Softdata$Country)
table_countries<-as.data.frame(table(Softdata$Country))
table_countries<-table_countries[order(-table_countries$Freq),]
colnames(table_countries)<-c("country","Freq")
row.names(table_countries)<-NULL

##Count role/qualification##
table(Softdata$Role.Qualification)
table_rolequal<-as.data.frame(table(Softdata$Role.Qualification))
table_rolequal<-table_rolequal[order(-table_rolequal$Freq),]
colnames(table_rolequal)<-c("roleandqual","Freq")
row.names(table_rolequal)<-NULL

#Plot bar graph for current country ##
subset_country<-table_country[1:10,]
barplot(subset_country$Freq,names.arg=as.character(subset_country$Countries),col="darkblue",cex.names=0.5,xlab="Current country",ylab="Number of people in each country",main="Number of people currently based by countries(Top10)")

#Plot bar graph for designation (C-level role) #
subset_designation<-table_designation[1:10,]
barplot(subset_designation$Freq,names.arg=as.character(subset_designation$Clevel),col="darkblue",cex.names=0.5,xlab="C Level role",ylab="Number of C Level mumbers",main="Number of C-Level members in each categories(Top10)")

###Plotting bar graphs for company type ###
barplot(table_companytype$Freq,names.arg=as.character(table_companytype$companytype),col="darkblue",cex.names=0.5,xlab="Company Type",ylab="Frequency",main="Company Type")


##Plot bar graph by country of education ##
subset_countries<-table_countries[1:10,]
barplot(subset_countries$Freq,names.arg=as.character(subset_countries$country),col="darkblue",cex.names=0.4,xlab="Country of education",ylab="Number of people in each country",main="Number of people by country of education(Top10)")

##Plot bar graph by organisation##
subset_organisation<-table_organisation[1:10,]
barplot(subset_organisation$Freq,names.arg=as.character(subset_organisation$organisation),col="darkblue",cex.names=0.4,xlab="Organisation name",ylab="Number of people by organisation",main="Number of people by organisation(Top10)")

##Plot bar graph by role and qualification##
subset_rolequal<-table_rolequal[1:10,]
barplot(subset_rolequal$Freq,names.arg=as.character(subset_rolequal$roleandqual),col="darkblue",cex.names=0.4,xlab="Role and Qualification",ylab="Number of people by role and qualification",main="Number of people by role of qualification(Top10)")

##Plotting in one single screen ##

##Dashboard 1##
par(mfrow=c(1,2))
barplot(subset_country$Freq,names.arg=as.character(subset_country$Countries),col="darkblue",cex.names=0.5,las=2,xlab="Current country",ylab="Number of people in each country",main="Number of people currently based by countries(Top10)")
barplot(subset_designation$Freq,names.arg=as.character(subset_designation$Clevel),col="darkgreen",cex.names=0.5,las=2,xlab="C Level role",ylab="Number of C Level mumbers",main="Number of C-Level members in each categories(Top10)")

##Dashboard 2##
par(mfrow=c(2,2))
barplot(table_companytype$Freq,names.arg=as.character(table_companytype$companytype),col="darkblue",cex.names=0.6,las=2,xlab="Company Type",ylab="Frequency",main="Company Type")
barplot(subset_countries$Freq,names.arg=as.character(subset_countries$country),col="darkgreen",cex.names=0.5,las=2,xlab="Country of education",ylab="Number of people in each country",main="Number of people by country of education(Top10)")
barplot(subset_organisation$Freq,names.arg=as.character(subset_organisation$organisation),col="lightblue",cex.names=0.5,las=2,xlab="Organisation name",ylab="Number of people by organisation",main="Number of people by organisation(Top10)")
barplot(subset_rolequal$Freq,names.arg=as.character(subset_rolequal$roleandqual),col="grey",cex.names=0.5,las=2,xlab="Role and Qualification",ylab="Number of people by role and qualification",main="Number of people by role of qualification(Top10)")



###Extracting the unique values##
uniquedata<-unique(Softdata$Current.individual.Name,)
uniquedata<-as.data.frame(uniquedata)

##Removing the duplicated entries##
newdata<-Softdata[!duplicated(Softdata$Current.individual.Name),]


###Plotting with ggplot2 (reordering issue of variables-Not used)###
ggplot(data=table_companytype,aes(x=companytype,y=Freq,fill=companytype))+geom_bar(colour="darkblue",stat="identity")




