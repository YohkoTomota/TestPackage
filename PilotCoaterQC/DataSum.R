#load packages
library(GGally)
library(ggplot2)
library(statsr)
library(dplyr)
library(readxl)

setwd("X:/AMAGASAKI-HBI/Battery Lab/BATTERY LAB WORK SPACE/Electrochemical test/PilotCoaterWorksheet")
df1 <- read_excel("PilotCoatingQC 20171121 CP017 not oress.xlsx",sheet=1,skip=5)
df1 <- df1[-1,1:14]
df1$SheetNo <- as.factor(df1$SheetNo)
#df1$'Lposition' <-  as.numeric(df1$'Lposition')
df1$'Weight' <-  as.numeric(df1$'Weight')
df1$'Thickness' <-  as.numeric(df1$'Thickness')
df1$'SCE-L' <-  as.numeric(df1$'SCE-L')
df1$'SCE-a' <-  as.numeric(df1$'SCE-a')
df1$'SCE-b' <-  as.numeric(df1$'SCE-b')
df1$'SCI-L' <-  as.numeric(df1$'SCI-L')
df1$'SCI-a' <-  as.numeric(df1$'SCI-a')
df1$'SCI-b' <-  as.numeric(df1$'SCI-b')
df1$'Loading' <-  as.numeric(df1$'Loading')
df1$'Density' <-  as.numeric(df1$'Density')
df1$'position' <-  as.numeric(df1$'position')
colnames(df1) <- c("SheetNo", "Wposition","Lposition", "Weight", "Thckns" ,"SCE-L","SCE-a","SCE-b","SCI-L", "SCI-a","SCI-b","Loading","Density","position")
df1 <- df1%>%mutate(TotalPosition = paste("sheet#",SheetNo,Lposition,sep="-"))

#Std
ggplot(df1, aes(x = position, y = Loading))+
  geom_point(aes(colour=Wposition),size=3)+
  theme(axis.text = element_text(angle = 45))+
  geom_smooth(aes(group = Wposition,colour=Wposition),se=FALSE,fullrange =FALSE,span = 1)

#initial
ggplot(df1[1:18,], aes(x = Lposition, y = Loading))+
  geom_point(aes(colour=Wposition),size=3)+
  theme(axis.text = element_text(angle = 45))

#Correlation plot for powder prop
ggpairs(df1)




#control chart
library(qcc)
Loading<-qcc.groups(df1$Loading,df1$TotalPosition)
q<-qcc(Loading,type = "xbar",ylab="Loading",xlab="Position", title ="Average Loading")
plot(q)

Lstar<-qcc.groups(df1$'SCE-L',df1$TotalPosition)
g2<-plot(qcc(Lstar,type = "xbar"))

