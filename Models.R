#Statistical models, coefficient plots/tables for primary and secondary analyses of FBData2
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
d<- read.csv(file='https://raw.githubusercontent.com/rmbryan/FB-Data/master/FBData2.csv')
#Models
#Preliminary analysis 
d$PartyID2012<- as.factor(d$PartyID2012)
d$State<- as.factor(d$State)
#FB use only
Model.1 <- glm(General2012 ~ Facebook2012 + Age2012 +Female+ PartyID2012+ State, family = binomial,
               data = d) 
display(Model.1 , detail = TRUE)  
#non interactive model 
Model.2 <- glm(General2012 ~ Facebook2012 + Friendslog2012+ YearsOnFb2012+ Age2012 +Female+ PartyID2012+ State, family = binomial,
               data = d) 
display(Model.2, detail = TRUE) 
#interactive model (main)
Model.3 <- glm(General2012 ~ Facebook2012 +Friendslog2012*YearsOnFb2012 + Friendslog2012+ YearsOnFb2012+ Age2012 +Female+ PartyID2012+ State, family = binomial,
               data = d) 
display(Model.3, detail = TRUE) 
#create table
stargazer( Model.1, Model.2, Model.3, title="The Effect of Facebook Use on Individual  Voter Turnout During the 2012 General Election", align=TRUE, dep.var.labels=c("General Election (2012)"), model.numbers = FALSE, object.names = TRUE, covariate.labels=c("Facebook Use", "log(Number of Friends)", "Years", "Age","Female", "Republican", "State: OH", "log(Number of Friends)*Years on Facebook", "Intercept" ),   column.sep.width = "3pt",notes.align = "l", omit = c("20122","20123", "StateNC", "StateOK") )
#coefficient plot (https://github.com/dsparks)
m1Frame <- data.frame(Variable = rownames(summary(Model.1)$coef),
                      Coefficient = summary(Model.1)$coef[, 1],
                      SE = summary(Model.1)$coef[, 2],
                      Model = "1. Facebook Use Only ")
m2Frame <- data.frame(Variable = rownames(summary(Model.2)$coef),
                      Coefficient = summary(Model.2)$coef[, 1],
                      SE = summary(Model.2)$coef[, 2],
                      Model = "2. Friends & Years (No Interaction)")
m3Frame <- data.frame(Variable = rownames(summary(Model.3)$coef),
                      Coefficient = summary(Model.3)$coef[, 1],
                      SE = summary(Model.3)$coef[, 2],
                      Model = "3. Friends*Years (Interaction Included)")
#remove unnecessary Vars 
m1Frame <- m1Frame[c(1, 2, 3,4,5,9), ]
m2Frame <- m2Frame[c(1, 2, 3,4,5, 6, 10), ]
m3Frame <- m3Frame[c(1, 2, 3,4,5, 6, 10, 12), ]
#rename observations 
m1Frame$Variable<- revalue(m1Frame$Variable, c("Facebook2012" = "Facebook \n Use", "PartyID20121" = "Democrat", "Age2012" = "Age", "StateNC" = "State(NC)"))
m2Frame$Variable<- revalue(m2Frame$Variable, c( "Friendslog2012" = "log(Friends)", "YearsOnFb2012"="Years", "PartyID20121" = "Democrat", "Age2012" = "Age", "StateOH" = "State(OH)"))
m3Frame$Variable<- revalue(m3Frame$Variable, c( "Friendslog2012" = "log(Friends)", "YearsOnFb2012"="Years", "Friendslog2012:YearsOnFb2012"="Social Pressure \n (Friends*Years)", "PartyID20121" = "Democrat", "Age2012" = "Age", "StateOH" = "State(OH)" ))
# Combine  data.frames
allModelFrame <- data.frame(rbind(m1Frame, m2Frame, m3Frame)) 
# Specify confidence intervals
interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier
# Plot Logistic Regression Estimates
coefplot1 <- ggplot(allModelFrame, aes(x = Variable, y = Coefficient, shape= Model, colour = Model, group = Model)) +
  geom_point(aes(),size = 4, position=position_dodge(width=1/2))+
  geom_errorbar(aes(x = Variable, ymin = Coefficient - SE*interval2,
                    ymax = Coefficient + SE*interval2), width=0,
                lwd = 1, position = position_dodge(width = 1/2)) +
  scale_shape_manual(values = c( 18,18,18)) +
  geom_hline(aes(yintercept=-0.00), colour="firebrick", linetype="dashed", size=1)+
  scale_color_manual(values=c( "gray60", "gray30", "blue3"))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.y = element_text(face="bold", color="black", size=12, angle=0))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0))+
  theme(legend.text=element_text(size=12))+
  theme(legend.position = c(0.2, 0.8))+
  scale_x_discrete(position = "top")+
  labs(y = "")+
  theme(axis.title.y=element_blank())+
  scale_y_continuous(limits=c(-3.7,2)) + 
  coord_flip() 
#view plot
coefplot1


#Secondary analysis
#Key: 18:25=1;>26=0(base)
Model.1 <- glm(General2012 ~ Friendslog2012*YearsOnFb2012*AgeBi+  AgeBi +Friendslog2012+ YearsOnFb2012+ Female+ PartyID2012+ State, family = binomial,data = d) 
display(Model.1, detail = TRUE)
d$AgeCat<- as.factor(d$AgeCat)
#Key: 18:25=1;26:31=2;32:60=0(base);>60=3
Model.2 <- glm(General2012 ~ Facebook2012 + Friendslog2012*YearsOnFb2012*AgeCat+  AgeCat +Friendslog2012+ YearsOnFb2012+ Female+ PartyID2012+ State, family = binomial,data = d) 
display(Model.2, detail = TRUE)
Model.3 <- glm(General2012  ~ Friendslog2012*YearsOnFb2012*Age2012+ Age2012 +Friendslog2012+ YearsOnFb2012+ Female+ PartyID2012+ State, family = binomial,data = d) 
display(Model.3, detail = TRUE)
#create table
stargazer( Model.1, Model.2,  Model.3, title="Results", single.row=TRUE, omit = c("PartyID20122","PartyID20123","NC", "OK"), covariate.labels = c("Facebook Use","log(Friends)","Years","Age=18:25(Binary)","Age=18:25(Categorical)","Age=26:31(Categorical)", "Age$>$60(Categorical)","Age(Numerical)", "Female","Republican","State:OH","log(Friends)*Years","log(Friends)*Age=18:25(Binary)", "Years*Age=18:25(Binary)","log(Friends)*Years*Age=18:25(Binary)", "log(Friends)*Age=18:25","log(Friends)*Age=26:31","log(Friends)*Age$>$60","Years*Age=18:25","Years*Age=26:31","Years*Age$>$60","log(Friends)*Years*Age=18:25","log(Friends)*Years*Age=26:31","log(Friends)*Years*Age$>$60","log(Friends)*Age(Numerical)","Years*Age(Numerical)","log(Friends)*Years*Age(Numerical)","Intercept"))

# Put model estimates into temporary data.frames:
m1Frame <- data.frame(Variable = rownames(summary(Model.1)$coef),
                      Coefficient = summary(Model.1)$coef[, 1],
                      SE = summary(Model.1)$coef[, 2],
                      Model = "1. Age as binary variable")
m2Frame <- data.frame(Variable = rownames(summary(Model.2)$coef),
                      Coefficient = summary(Model.2)$coef[, 1],
                      SE = summary(Model.2)$coef[, 2],
                      Model = "2. Age as categorical")
m3Frame <- data.frame(Variable = rownames(summary(Model.3)$coef),
                      Coefficient = summary(Model.3)$coef[, 1],
                      SE = summary(Model.3)$coef[, 2],
                      Model = "3. Age as numerical")

#remove unnecessary Vars 
m1Frame <- m1Frame[c(1, 2, 3,4,5,10), ]
m2Frame <- m2Frame[c(1, 2, 3,4,5, 6,7,8, 12, 14,21:23), ]
m3Frame <- m3Frame[c(1:6, 10,12,15), ]
#rename observations 
m1Frame$Variable<- revalue(m1Frame$Variable, c( "Friendslog2012" = "log(Friends)", "YearsOnFb2012"="Years","PartyID20121" = "Democrat", "AgeBi" = "Age(Binary)", "StateOH" = "State(OH)"))
m2Frame$Variable<- revalue(m2Frame$Variable, c( "Friendslog2012" = "log(Friends)", "YearsOnFb2012"="Years", "PartyID20121" = "Democrat", "AgeCat1"="Age=18:25(Categorical)","AgeCat2"="Age=26:31(Categorical)", "AgeCat3"="Age>60(Categorical)","StateOH" = "State(OH)", "Friendslog2012:YearsOnFb2012"="Social Pressure (Friends*Years)", "Friendslog2012:YearsOnFb2012:AgeCat1"="Social Pressure Score*Age=18:25(Categorical)","Friendslog2012:YearsOnFb2012:AgeCat2"="Social Pressure Score*Age=26:31(Categorical)","Friendslog2012:YearsOnFb2012:AgeCat3"="Social Pressure Score*Age$>$60(Categorical)"))
m3Frame$Variable<- revalue(m3Frame$Variable, c( "Friendslog2012" = "log(Friends)", "YearsOnFb2012"="Years", "Friendslog2012:YearsOnFb2012"="Social Pressure (Friends*Years)", "PartyID20121" = "Democrat",  "StateOH" = "State(OH)","Age2012" = "Age(Numerical)", "Friendslog2012:YearsOnFb2012:Age2012"="Social Pressure Score*Age(Numerical)"))
# Combine these data.frames
allModelFrame <- data.frame(rbind(m1Frame, m2Frame, m3Frame))  # etc.
# Specify confidence intervals
interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier
# Plot Logistic Regression Estimates

coefplot2 <- ggplot(allModelFrame, aes(x = Variable, y = Coefficient, shape= Model, colour = Model, group = Model)) +
  geom_point(aes(),size = 4, position=position_dodge(width=1/2))+
  geom_errorbar(aes(x = Variable, ymin = Coefficient - SE*interval2,
                    ymax = Coefficient + SE*interval2), width=0,
                lwd = 1, position = position_dodge(width = 1/2)) +
  scale_shape_manual(values = c( 18,18,18)) +
  geom_hline(aes(yintercept=-0.00), colour="firebrick", linetype="dashed", size=1)+
  scale_color_manual(values=c( "gray60", "gray30", "blue3"))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.y = element_text(face="bold", color="black", size=12, angle=0))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0))+
  theme(legend.text=element_text(size=12))+
  theme(legend.position = c(0.3, 0.8))+
  scale_x_discrete(position = "top")+
  labs(y = "")+
  theme(axis.title.y=element_blank())+
  scale_y_continuous(limits=c(-6,2)) + 
  coord_flip() 
#view coef plot
coefplot2                                                                                                                                                                 
