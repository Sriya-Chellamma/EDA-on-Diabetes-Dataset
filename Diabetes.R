setwd("C:/Users/Ramachandran/Desktop/Tableau Docs/diabetes")
diabetes=read.csv("diabetes_tibble.csv")
data("diabetes")
str(diabetes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(forcats)
hist(diabetes$age)
ggplot(diabetes,aes(diabetes$age,
                    fill=diabetes$outcome))+geom_histogram(position = "dodge")+
  xlab("Age")+ylab("Count")+
  title("Histogram of Age distinguished by Outcome")
diabetes2 <- diabetes %>% filter(diabetes$outcome=='yes')
ggplot(diabetes,aes(diabetes$age,diabetes$pregnancies,fill=outcome))+
  geom_boxplot()+xlab("Age")+ylab("No. of pregnancies")+
  ggtitle("Average of Age and Pregnancies correlating to the Outcome")
ggplot(diabetes,aes(diabetes$glucose,diabetes$pregnancies,color=outcome))+
  geom_point()
ggplot(diabetes,aes(diabetes$glucose,diabetes$pregnancies,colour=outcome))+
  geom_point()+xlab("No. of Pregnancies")+ylab("Glucose Levels")+
  ggtitle("Correlation between Pregnancies and Glucose")
ggplot(diabetes,aes(diabetes$glucose,diabetes$age,colour=outcome))+
  geom_point()+xlab("Glucose")+ylab("Age")+
  ggtitle("Correlation between Glucose and Age")
ggplot(diabetes,aes(diabetes$blood_pressure,diabetes$age,colour=outcome))+
  geom_point()+xlab("Blood Pressure")+
  ylab("Age")+ggtitle("Corr btw Age and BP")
ggplot(diabetes,aes(diabetes$blood_pressure,diabetes$age,fill=outcome))+
  geom_bin_2d()+xlab("Blood Pressure")+
  ylab("Age")+ggtitle("Corr btw Age and BP")
ggplot(diabetes,aes(diabetes$skin_thickness,fill=outcome))+
  geom_histogram(position = 'dodge')+xlab("Skin Thickness")+ylab("Count")+
  ggtitle("Histogram of Skin Thickness with outcome classification")
ggplot(diabetes,aes(diabetes$insulin,diabetes$age,colour=outcome))+
  geom_point()+xlab("Insulin")+ylab("Age")+ggtitle("Correlation btw Insulin and Age")
ggplot(diabetes,aes(diabetes$glucose,diabetes$insulin,colour=outcome))+
  geom_point
ggplot(diabetes,aes(diabetes$insulin,diabetes$blood_pressure,colour=outcome))+geom_point()
ggplot(diabetes,aes(diabetes$insulin,diabetes$diabetes_pedigree_function,colour=outcome))+geom_point()
ggplot(diabetes,aes(diabetes$bmi,fill=outcome))+geom_histogram(position = "dodge")
ggplot(diabetes,aes(bmi,pregnancies,colour=outcome))+geom_point()
ggplot(diabetes,aes(bmi,glucose,colour=outcome))+geom_point()
ggplot(diabetes,aes(diabetes_pedigree_function,diabetes$pregnancies,colour=outcome))+geom_jitter()
ggplot(diabetes,aes(diabetes_pedigree_function,diabetes$glucose,colour=outcome))+geom_jitter()
ggplot(diabetes,aes(diabetes_pedigree_function,diabetes$age,colour=outcome))+geom_point()
ggplot(diabetes,aes(diabetes_pedigree_function,fill=outcome))+geom_histogram()
model1=lm(diabetes$outcome~.,diabetes)
model2= glm(diabetes$outcome~age+glucose+bmi,family=binomial(),diabetes)
