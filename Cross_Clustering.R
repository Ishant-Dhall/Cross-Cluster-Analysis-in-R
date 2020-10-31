# importing libraries
if(!require(ggplot2)){install.packages('ggplot2')}; library(ggplot2)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(summarytools)){install.packages('summarytools')}; library(summarytools)
if(!require(ggthemes)){install.packages('ggthemes')}; library(ggthemes)
if(!require(ggrepel)){install.packages('ggrepel')}; library(ggrepel)
if(!require(plotly)){install.packages('plotly')}; library(plotly)
if(!require(ggpubr)){install.packages('ggpubr')}; library(ggpubr)
if(!require(readr)){install.packages('readr')}; library(readr)
if(!require(plyr)){install.packages('plyr')}; library(plyr)
############################################
demo<-read.csv(file.choose(),header=T, stringsAsFactors = F) # Loading Data sets
###
beh<-read.csv(file.choose(),header=T, stringsAsFactors = F)
###
View(demo)
View(beh)
########################################
# Generating ID column
demo["ID"]<-c(1:nrow(demo))
beh["ID"]<-c(1:nrow(beh))
########################################
colnames(demo)[7]<-"Demo_Segment" # Changing the column names
colnames(beh)[6]<-"Beh_Segment"
demo<-demo[c(8,1,2,3,5,6,7,4)]  # Rearranging the columns
beh<-beh[c(7,1,2,3,5,6,4)]
#########################################
#### Giving segment numbers unique names ###
demo$Demo_Segment <- ifelse(demo$Demo_Segment==1, "Young Couples",
                                ifelse(demo$Demo_Segment==2, "Educated Adults",
                                  ifelse(demo$Demo_Segment==3, "Surviving Xennials",
                                    ifelse(demo$Demo_Segment==4, "Middle-Age Partners",
                                      ifelse(demo$Demo_Segment==5, "Elderly Retired",
                                                       "Growing Youth")))))
##
## For behavioural segments
beh$Beh_Segment<-replace(beh$Beh_Segment, beh$Beh_Segment== "1", " Personal Loan ")
beh$Beh_Segment<-replace(beh$Beh_Segment, beh$Beh_Segment== "2", " Cellular Contact ")
beh$Beh_Segment<-replace(beh$Beh_Segment, beh$Beh_Segment== "3", " Mortgage Loan ")
beh$Beh_Segment<-replace(beh$Beh_Segment,beh$Beh_Segment== "4", " Both loans ")
beh$Beh_Segment<-replace(beh$Beh_Segment,beh$Beh_Segment== "5", " Less Contacted ")
####
#### Merging the data
com_data<-merge(demo,beh, by=c("ID","Subscribed"))
View(com_data)
#####################################################
### Bulleted Point 1
table(com_data$Demo_Segment, com_data$Beh_Segment) # Frequency Table
round(prop.table(table(com_data$Demo_Segment, com_data$Beh_Segment))*100, 2) # Prop table

####
### Bulleted Point 2 -- Calculating Lift values
table(com_data$Demo_Segment, com_data$Beh_Segment, com_data$Subscribed) # Frequency Table
lift<-table(com_data$Demo_Segment, com_data$Beh_Segment, com_data$Subscribed)
round(lift[,,"yes"]/apply(lift, 1:2, sum)*100,2) # Calculating Lift values
round(prop.table(table(com_data$Demo_Segment, com_data$Beh_Segment, com_data$Subscribed))*100, 2)# Prop table

##############
