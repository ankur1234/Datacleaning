
##sET WORKING DIRECTORY###
setwd()

#Load the data
sorted <- read.csv("C:/Users/aborthakur/Desktop/Office docs/DS-CoE/R session/Data-R/survey.csv", header=TRUE,stringsAsFactors=FALSE)

#Rename the data file
data<-sorted

#Summary statistics
summary(data)

#Structure of the data
str(data)

##Subsetting and combining data frames ###

##Subset by columns
##Column subset
data1<-data[,1:2]
data2<-data[,3:5]

datanew<-cbind(data1,data2)

##Row subset
data1<-data[1:3,]
data2<-data[6:7,]

datanew<-rbind(data1,data2)

##Delete a variable
datanew<-datanew[,-1]


##Loop through to find summary data###

sum<-data.frame(lapply(data,sum))
sum<-data.frame(apply(data,2,sum))

##Correlation
cor<-data.frame(cor(data))

library(d3heatmap)

d3heatmap(as.data.frame(cor), scale = "column",color="Greens",cexRow = 0.7,cexCol = 0.7)




#Label the dummy variables
#Load the labels file
labels <- read.csv("C:/Users/aborthakur/Desktop/Office docs/DS-CoE/R session/Data-R/labels.csv")

#Create seperate data files for each question variables for matching
all_Destination <- labels[which(labels$Field.Name=="DESTGEO"),]
all_travelreason <- labels[which(labels$Field.Name=="Q2PURP1"),]
all_safetylevel <- labels[which(labels$Field.Name=="Q10SAFE"),]

#Match and label in a new column
data$destination <- all_Destination$Label[match(data$DESTGEO,all_Destination$Code)]
data$travelreason <- all_travelreason$Label[match(data$Q2PURP1,all_travelreason$Code)]
data$safetylevel <- all_safetylevel$Label[match(data$Q10SAFE,all_safetylevel$Code)]

#Explore
table(data$DESTGEO)
table_destination<-as.data.frame(table(data$DESTGEO))
table_destination<-table_destination[order(-table_destination$Freq),]
table_destination$label<-all_Destination$Label[match(table_destination$Var1,all_Destination$Code)]

#Barchart
barplot(table_destination$Freq,names.arg=as.character(table_destination$label),col="darkblue",cex.names=0.5,xlab="Destinations",ylab="Number of passengers",main="Number of passengers to various destinations")

#Create Deloitte colour palette
deloitte_colours <- c("#002776","#3C8A2E","#C9DD03","#72C7E7","#7F7F7F","#92D400","#00A1DE")

#Piechart
pie(table_destination$Freq,labels=table_destination$label,col=deloitte_colours)

#Piechart with percentages
pct<-round(table_destination$Freq/sum(table_destination$Freq)*100)
lbls<-paste(table_destination$label,pct)
lbls <- paste(lbls,"%",sep="")
pie(table_destination$Freq,labels=lbls,cex=0.7,col=deloitte_colours, main="Proportion of passengers to various destinations")

#Histogram
hist(data$Q7WIFI,col="darkgreen",xlab="Quality of wi-fi(5-Outstanding,1-Unacceptable,6-Not used)",main="Responses of quality of wi-fi")


#Exploratory analysis
hist(data$destination,col="darkblue",xlab="Destinations",main="Histogram of the frequency of destinations")

#Correlation
cor(data$Q18AGE,data$Q20INCOME)

#Rename column questions as names
colnames(data)[3]<-"destination_geography" #Change particular column name

#Rename entire variables in columns
colnames(data)<-c("IDs","destination","destination_geography","destination_marketsize","ticket_collection","trip_purpose","mode_of_commuting","bagage_check","wifi_use","Frequency of flying","firsttime_travel","signs_direction","infon_screen","wifi_quality","safety","precheck_security","incoming_country","airport_experience","age","gender","income")



#Replace the first column 
data1<-data[,-1]

#Scatter plot
plot_scatter<-plot(jitter(data$Q20INCOME),jitter(data$Q19GENDER),col="darkblue",pch=20,cex=1.0)


#Explore the data
#table(data$destination)

###Arrange it in a data frame###
#table_destination<-as.data.frame(table(data$destination))
#table_destination<-table_destination[order(-table_destination$Freq),]




