library(readr)
library(dplyr)
library(ggplot2)
DailySpec<-read.csv("data/daily_SPEC_2014.csv.bz2")
object.size(DailySpec)

#Q1, What is average Arithmetic.Mean for “Bromine PM2.5 LC” in the state of Wisconsin in this dataset?
DailySpec %>% filter(State.Name=="Wisconsin",
                                Parameter.Name=="Bromine PM2.5 LC") %>%
  summarise(mean=mean(Arithmetic.Mean,na.rm=TRUE))

#Q2, Which constituent Parameter.Name has the highest average level?
q2<-DailySpec %>% select(Parameter.Name,Arithmetic.Mean) %>%
  group_by(Parameter.Name)%>%
  summarise(mean=mean(Arithmetic.Mean,na.rm=TRUE))%>%
  arrange(desc(mean))
q2

#Q3,Which monitoring site has the highest average level of “Sulfate PM2.5 LC” across all time?
q3<-DailySpec %>% filter(Parameter.Name=="Sulfate PM2.5 LC")%>%
  group_by(State.Code,County.Code,Site.Num)%>%
  summarize(mean=mean(Arithmetic.Mean))%>%
  arrange(desc(mean))
  
#Q4,What is the absolute difference in the average levels of “EC PM2.5 LC TOR” between the states 
#California and Arizona, across all time and all monitoring sites?  
q4<-data.frame()
q4<-DailySpec %>% filter(Parameter.Name=="Sulfate PM2.5 LC" &
                         State.Name%in%c("California","Arizona"))%>%
  group_by(State.Name)%>%
  summarize(mean=mean(Arithmetic.Mean,na.rm=TRUE))
Ca<-q4%>%filter(State.Name=="California")
Ar<-q4%>%filter(State.Name=="Arizona")
Ca-Ar

#What is the median level of “OC PM2.5 LC TOR” in the western United States, across all time? 
#Define western as any monitoring location that has a Longitude LESS THAN -100.

q5<-DailySpec %>% filter(Parameter.Name=="OC PM2.5 LC TOR",
                         Longitude < -100)%>%
  summarize(median=median(Arithmetic.Mean,na.rm=TRUE))

#Q6
library(readxl)
AqsSite<-read_excel("data/aqs_sites.xlsx")
AqsSite%>%filter(`Land Use`=="RESIDENTIAL",`Location Setting`=="SUBURBAN")%>%
  summarise(count=n())

#Q7 conbine two
Sub_DailySpec<-DailySpec%>%select(State.Code,County.Code,Site.Num,Longitude,Parameter.Name,CBSA.Name,Arithmetic.Mean)%>%
  filter(Parameter.Name=="EC PM2.5 LC TOR")%>%
  filter(Longitude >= -100)
Sub_AqsSite<- AqsSite %>% select(`State Code`,`County Code` ,`Site Number`,`Land Use`, `Location Setting`)
colnames(Sub_AqsSite)<-c("State.Code","County.Code","Site.Num","LandUse", "LocationSetting")
                 
Sub_DailySpec<-Sub_DailySpec %>%
  left_join(Sub_AqsSite,by=c("State.Code" ,"County.Code","Site.Num")) 
  
Sub_DailySpec %>%  filter(LandUse=="RESIDENTIAL",LocationSetting=="SUBURBAN")%>%
  summarise(median=median(Arithmetic.Mean))
 
#Q8 Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", 
#which month of the year has the highest average levels of "Sulfate PM2.5 LC"?
library(lubridate)

Sub_DailySpec<-DailySpec%>%
  select(State.Code,County.Code,Site.Num,Date.Local,Parameter.Name,Arithmetic.Mean)%>%
  filter(Parameter.Name=="Sulfate PM2.5 LC")

Sub_DailySpec$Date.Local=as.Date(Sub_DailySpec$Date.Local)
Sub_DailySpec<-Sub_DailySpec %>%
  mutate(month=months(Date.Local))

Sub_DailySpec<-Sub_DailySpec %>%
  left_join(Sub_AqsSite,by=c("State.Code" ,"County.Code","Site.Num")) 

Sub_DailySpec %>%  filter(LandUse=="COMMERCIAL")%>%
  group_by(month) %>%
  summarise(mean=mean(Arithmetic.Mean))%>%
  arrange(desc(mean))

#Q9 Take a look at the data for the monitoring site identified by State Code 6,
#County Code 65, and Site Number 8001 (this monitor is in California). At this monitor,
#for how many days is the sum of "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" greater than 10?

Sub_DailySpec<-DailySpec %>% filter(State.Code==6,County.Code==65,Site.Num==8001)%>%
  filter(Parameter.Name %in% c("Sulfate PM2.5 LC","Total Nitrate PM2.5 LC"))%>%
  select(Date.Local,Parameter.Name,Arithmetic.Mean)
Sub_DailySpec$Date.Local=as.Date(Sub_DailySpec$Date.Local)
Sub_DailySpec$Parameter.Name=as.character(Sub_DailySpec$Parameter.Name)

total <- Sub_DailySpec%>%
  group_by(Date.Local)%>%
  summarise(sum=sum(Arithmetic.Mean))%>%
  filter(sum>10)%>%
  summarise(count=n())

#Q10 Which monitoring site in the dataset has the highest correlation 
#between "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" across all dates?
Sub_DailySpec<-DailySpec %>% filter(State.Code==6,County.Code==65,Site.Num==8001)%>%
  filter(Parameter.Name==c("Sulfate PM2.5 LC","Total Nitrate PM2.5 LC"))%>%
  select(Date.Local,Parameter.Name,Arithmetic.Mean)