library(car)
library(arm)
library(stargazer)
library(texreg)
library(xlsx)
library(plyr) 
library(sandwich)
library(compactr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)
library(vcd)
library(psych)
library(Hmisc)
setwd("~/Box Sync/Research/Facebook Research 2015/2014 Voter Data/2015DatasetsMerged")
d<- read.csv(file='/Users/rebeccabryan/Box Sync/Research/Facebook Research 2015/2014 Voter Data/2015DatasetsMerged/FBData1.csv')

#Models
#Preliminary analysis 
d$PartyID2012<- as.factor(d$PartyID2012)
d$State<- as.factor(d$State)
Model.3 <- glm(General2012 ~ Facebook2012 +Friendslog2012*YearsOnFb2012 + Friendslog2012+ YearsOnFb2012+ Age2012 +Female+ PartyID2012+ State, family = binomial,
               data = d) 
display(Model.3, detail = TRUE) 

