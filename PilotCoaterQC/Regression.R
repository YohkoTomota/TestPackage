#load packages
library(GGally)
library(ggplot2)
library(statsr)
library(dplyr)
library(readxl)

setwd("X:/AMAGASAKI-HBI/Battery Lab/BATTERY LAB WORK SPACE/Electrochemical test")
df1 <- read_excel("2017 AMA pilot electrodes.xlsm",sheet=1,skip=9)

#1stDischarge multiple regression
df3 <- df2 %>% select(BET,ResLi2CO3,ResLiOH ,TotalLi,a,c,CS,Strain,`Me in Li layer`,`25deg1stDIS`)
colnames(df3) <- c("BET", "ResLi2CO3","ResLiOH","TotalLi" ,"a","c","CS", "Strain","Disorder","25deg1stDIS")
lm.1stDis <- lm(`25deg1stDIS`~.,data =df3)
summary(lm.1stDis)
lm.1stDis.step<-step(lm.1stDis)
summary(lm.1stDis.step)
#diagnosis
qplot(residuals(lm.1stDis.step),binwidth=0.5)
qplot(predict(lm.1stDis.step),df3$`25deg1stDIS`)+geom_smooth(method="lm")
coefplot(lm.1stDis.step)
#Prediction
TestCAM1 <- data.frame(BET=0.32,ResLiOH=0.54 ,a=2.86889,c=14.1886,CS=298.775,Strain=0.30548,Disorder=0.004862905)
predict(lm.1stDis.step,TestCAM,interval = "prediction",level = 0.95)
TestCAM2 <- data.frame(BET=0.32,ResLiOH=0.61 ,a=2.86886,c=14.1883,CS=313.247,Strain=0.31394,Disorder=0.009318531)
