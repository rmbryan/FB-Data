#Quantities of interest: Predicted probabilies, first differences, second differences for primary and secondary analyses from Modesl.R file
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
d$PartyID2012<- as.factor(d$PartyID2012)
d$State<- as.factor(d$State)

########################
#Model.3 from preliminary analysis
Model.3 <- glm(General2012 ~ Friendslog2012*YearsOnFb2012 + Friendslog2012+ YearsOnFb2012+ Age2012 +Female+ PartyID2012+ State, family = binomial, data = d); display(Model.3, detail = TRUE) ; 
m<- Model.3
########################
#Predicted Probabilities
beta.hat<- coef(m)
Sigma.hat <- vcov(m)
#x1= Friendslog2012, x2= YearsOnFb2012, all other vars set to means/mode  
coefs.c<- function(x1, x2){ X.c <- c(1,x1,x2,49,1,1,0,0,1,0,0,(x1*x2));return(X.c)}
#create functions
#probabilities
predicted.prob <- function(X.c.value, beta.hat.value ){p.hat <- plogis(X.c.value%*%beta.hat.value); return(p.hat)}
#simulate confidence intervals 
p.ci.sim <- function(beta.hat.value, Sigma.hat.value, X.c.value){n.sims <- 10000;  p.tilde <- numeric(n.sims);  for (i in 1:n.sims) {beta.tilde <- mvrnorm(1, beta.hat.value, Sigma.hat.value); p.tilde[i] <- plogis(X.c.value%*%beta.tilde)};return(p.tilde)}
#create points for graph (low): small number of friends, over 0-7 years 
setX.c<- coefs.c(4,0);prob1<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.1<-quantile(predicted.prob.ci,(0.05));ci.hi.1<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(4,1);prob2<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.2<-quantile(predicted.prob.ci,(0.05));ci.hi.2<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(4,2);prob3<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.3<-quantile(predicted.prob.ci,(0.05));ci.hi.3<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(4,3);prob4<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.4<-quantile(predicted.prob.ci,(0.05));ci.hi.4<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(4,4);prob5<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.5<-quantile(predicted.prob.ci,(0.05));ci.hi.5<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(4,5);prob6<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.6<-quantile(predicted.prob.ci,(0.05));ci.hi.6<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(4,6);prob7<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.7<-quantile(predicted.prob.ci,(0.05));ci.hi.7<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(4,7);prob8<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.8<-quantile(predicted.prob.ci,(0.05));ci.hi.8<-quantile(predicted.prob.ci,(0.95))
#create data.frame
div.values<- rbind( 0, 1,	2,	3,	4,	5,	6,	7)
bind.prob<- rbind(prob1,  prob2,	prob3,	prob4,	prob5,	prob6,	prob7, prob8)
bind.lo.ci.lo<-rbind(ci.lo.1,  ci.lo.2,	ci.lo.3,	ci.lo.4,	ci.lo.5,	ci.lo.6,	ci.lo.7, ci.lo.8)
bind.lo.ci.hi<- rbind(ci.hi.1,  ci.hi.2,  ci.hi.3,	ci.hi.4,	ci.hi.5,	ci.hi.6,	ci.hi.7, ci.hi.8)
Model<- c("Low Social Pressure (50 Friends)", "Low Social Pressure (50 Friends)", "Low Social Pressure (50 Friends)", "Low Social Pressure (50 Friends)", "Low Social Pressure (50 Friends)", "Low Social Pressure (50 Friends)", "Low Social Pressure (50 Friends)", "Low Social Pressure (50 Friends)")
bind.all.prob.lo<- data.frame(div.values, bind.prob, bind.lo.ci.lo, bind.lo.ci.hi, Model)
##create points for graph (high): large number of friends, over 0-7 years 
setX.c<- coefs.c(6,0);prob1<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.1<-quantile(predicted.prob.ci,(0.05));ci.hi.1<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(6,1);prob2<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.2<-quantile(predicted.prob.ci,(0.05));ci.hi.2<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(6,2);prob3<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.3<-quantile(predicted.prob.ci,(0.05));ci.hi.3<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(6,3);prob4<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.4<-quantile(predicted.prob.ci,(0.05));ci.hi.4<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(6,4);prob5<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.5<-quantile(predicted.prob.ci,(0.05));ci.hi.5<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(6,5);prob6<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.6<-quantile(predicted.prob.ci,(0.05));ci.hi.6<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(6,6);prob7<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.7<-quantile(predicted.prob.ci,(0.05));ci.hi.7<-quantile(predicted.prob.ci,(0.95))
setX.c<- coefs.c(6,7);prob8<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.8<-quantile(predicted.prob.ci,(0.05));ci.hi.8<-quantile(predicted.prob.ci,(0.95))
#create data.frame
div.values<- rbind( 0, 1, 2,	3,	4,	5,	6,	7)
bind.prob<- rbind( prob1, prob2,	prob3,	prob4,	prob5,	prob6,	prob7, prob8)
bind.hi.ci.lo<-rbind( ci.lo.1, ci.lo.2,	ci.lo.3,	ci.lo.4,	ci.lo.5,	ci.lo.6,	ci.lo.7, ci.lo.8)
bind.hi.ci.hi<- rbind( ci.hi.1, ci.hi.2,  ci.hi.3,	ci.hi.4,	ci.hi.5,	ci.hi.6,	ci.hi.7, ci.hi.8)
Model<- c("High Social Pressure (400 Friends)", "High Social Pressure (400 Friends)", "High Social Pressure (400 Friends)", "High Social Pressure (400 Friends)", "High Social Pressure (400 Friends)", "High Social Pressure (400 Friends)", "High Social Pressure (400 Friends)", "High Social Pressure (400 Friends)")
bind.all.prob.hi<- data.frame(div.values, bind.prob, bind.hi.ci.lo, bind.hi.ci.hi, Model)
#combine low/high data.frames
total <- rbind(bind.all.prob.lo, bind.all.prob.hi)
#graph predicted probabilities
prob.plot <- ggplot(total, aes( x = div.values, y = bind.prob, shape= Model, colour = Model, group = Model)) + geom_point(aes(),size = 6, position=position_dodge(width=1/2))+
  geom_linerange(aes(x = div.values, ymin = X5., ymax = X95.),lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = div.values, y = bind.prob, ymin = X5., ymax = X95.),lwd = 1/2, position = position_dodge(width = 1/2))+ 
  scale_shape_manual(values = c(20, 19)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c( "lightskyblue2", "darkblue"))+labs(x = "Years on Facebook")+ labs(y = "Pr(Vote)")+ theme(text = element_text(size=24)) + theme(legend.position = c(0.75, 0.3))
#view
prob.plot

##################
#first differences
#create functions
first.difference <- function(p.hi.value, p.lo.value){
  fd.hat<- p.hi.value - p.lo.value
  return(fd.hat)
}
fd.ci.sim <- function(beta.hat.value, Sigma.hat.value, X.hi.value, X.lo.value){
  n.sims <- 10000  
  beta.tilde <- mvrnorm(n.sims, beta.hat.value, Sigma.hat.value) 
  fd.tilde <- numeric(n.sims)  
  for (i in 1:n.sims) {  
    fd.tilde[i] <- (plogis(X.hi.value%*%beta.tilde[i, ])) - (plogis(X.lo.value%*%beta.tilde[i, ]))
  }
  return(fd.tilde)
}
#create points for graph (high values, diff in years )
X.hi.lo<- coefs.c(6,0);X.hi.hi<- coefs.c(6,2);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd0<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.0<-quantile(hi.fd.ci,(0.05));ci.hi.0<-quantile(hi.fd.ci,(0.95))
X.hi.lo<- coefs.c(6,1);X.hi.hi<- coefs.c(6,3);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd1<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.1<-quantile(hi.fd.ci,(0.05));ci.hi.1<-quantile(hi.fd.ci,(0.95))
X.hi.lo<- coefs.c(6,2);X.hi.hi<- coefs.c(6,4);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd2<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.2<-quantile(hi.fd.ci,(0.05));ci.hi.2<-quantile(hi.fd.ci,(0.95))
X.hi.lo<- coefs.c(6,3);X.hi.hi<- coefs.c(6,5);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd3<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.3<-quantile(hi.fd.ci,(0.05));ci.hi.3<-quantile(hi.fd.ci,(0.95))
X.hi.lo<- coefs.c(6,4);X.hi.hi<- coefs.c(6,6);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd4<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.4<-quantile(hi.fd.ci,(0.05));ci.hi.4<-quantile(hi.fd.ci,(0.95))
X.hi.lo<- coefs.c(6,5);X.hi.hi<- coefs.c(6,7);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd5<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.5<-quantile(hi.fd.ci,(0.05));ci.hi.5<-quantile(hi.fd.ci,(0.95))
#create data.frame
v<- rbind( "0 to 2", "1 to 3",  "2 to 4",  "3 to 5",	"4 to 6",	"5 to 7")
div.values<- rbind( 1,  2,  3,	4,	5,	6)
bind.fd<- rbind(hi.fd0, hi.fd1,  hi.fd2,	hi.fd3,	hi.fd4,	hi.fd5)
bind.hi.ci.lo<-rbind(ci.lo.0, ci.lo.1,  ci.lo.2,	ci.lo.3,	ci.lo.4,	ci.lo.5)
bind.hi.ci.hi<- rbind(ci.hi.0, ci.hi.1,  ci.hi.2,  ci.hi.3,	ci.hi.4,	ci.hi.5)
Model<- c("Large Friend Network", "Large Friend Network", "Large Friend Network", "Large Friend Network", "Large Friend Network", "Large Friend Network")
bind.all.fd.hi<- data.frame(div.values, bind.fd, bind.hi.ci.lo, bind.hi.ci.hi, Model, v)
# create points for graph (low values, diff in years )
X.lo.lo<- coefs.c(4,0);X.lo.hi<- coefs.c(4,2);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd0<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.0<-quantile(lo.fd.ci,(0.05));ci.hi.0<-quantile(lo.fd.ci,(0.95))
X.lo.lo<- coefs.c(4,1);X.lo.hi<- coefs.c(4,3);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd1<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.1<-quantile(lo.fd.ci,(0.05));ci.hi.1<-quantile(lo.fd.ci,(0.95))
X.lo.lo<- coefs.c(4,2);X.lo.hi<- coefs.c(4,4);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd2<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.2<-quantile(lo.fd.ci,(0.05));ci.hi.2<-quantile(lo.fd.ci,(0.95))
X.lo.lo<- coefs.c(4,3);X.lo.hi<- coefs.c(4,5);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd3<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.3<-quantile(lo.fd.ci,(0.05));ci.hi.3<-quantile(lo.fd.ci,(0.95))
X.lo.lo<- coefs.c(4,4);X.lo.hi<- coefs.c(4,6);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd4<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.4<-quantile(lo.fd.ci,(0.05));ci.hi.4<-quantile(lo.fd.ci,(0.95))
X.lo.lo<- coefs.c(4,5);X.lo.hi<- coefs.c(4,7);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd5<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.5<-quantile(lo.fd.ci,(0.05));ci.hi.5<-quantile(lo.fd.ci,(0.95))
#create data.frame
v<- rbind( "0 to 2", "1 to 3",  "2 to 4",  "3 to 5",	"4 to 6",	"5 to 7")
div.values<- rbind( 1,  2,  3,	4,	5,	6)
bind.fd<- rbind(lo.fd0, lo.fd1,  lo.fd2,	lo.fd3,	lo.fd4,	lo.fd5)
bind.lo.ci.lo<-rbind(ci.lo.0, ci.lo.1,  ci.lo.2,	ci.lo.3,	ci.lo.4,	ci.lo.5)
bind.lo.ci.hi<- rbind(ci.hi.0, ci.hi.1,  ci.hi.2,  ci.hi.3,	ci.hi.4,	ci.hi.5)
Model<- c("Small Friend Network", "Small Friend Network", "Small Friend Network", "Small Friend Network", "Small Friend Network", "Small Friend Network")
bind.all.fd.lo<- data.frame(div.values, bind.fd, bind.lo.ci.lo, bind.lo.ci.hi, Model, v)
#combine low/high data.frames
total <- rbind(bind.all.fd.lo, bind.all.fd.hi)

fd.plot <- ggplot(total, aes(x = v, y = bind.fd, shape= Model, colour = Model, group = Model)) + geom_point(aes(),size = 7, position=position_dodge(width=1/2))+ 
  geom_linerange(aes(x = v, ymin = X5., ymax = X95.), lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = v, y = bind.fd, ymin = X5.,ymax = X95.),lwd = 1/2, position = position_dodge(width = 1/2))+
   scale_shape_manual(values = c(20, 19)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values=c( "blue", "black")) + labs(x = "Years on Facebook") + labs(y = expression( "Pr(Vote)" ~ Delta)) + geom_hline(aes(yintercept=0), colour="firebrick", linetype="dashed")+
  theme(text = element_text(size=30))+ theme(legend.position="none") #remove legend (repeats from pred prob plot)
print(fd.plot)

##################
#second differences
#create functions
sd.ci.sim <- function(beta.hat, Sigma.hat, X.hi.hi.value, X.lo.hi.value, X.hi.lo.value, X.lo.lo.value){
  n.sims <- 1000  
  beta.tilde <- mvrnorm(n.sims, beta.hat, Sigma.hat) 
  sd.tilde <- numeric(n.sims) 
  for (i in 1:n.sims) {  
    sd.tilde[i] <- ((plogis(X.hi.hi.value%*%beta.tilde[i, ])) - (plogis(X.lo.hi.value%*%beta.tilde[i, ]))) - ((plogis(X.hi.lo.value%*%beta.tilde[i, ])) - (plogis(X.lo.lo.value%*%beta.tilde[i, ]))) 
  }
  return (sd.tilde)
}

#create points for graph 
X.hi.lo<-coefs.c(6,0);X.hi.hi<-coefs.c(6,2); X.lo.lo<-coefs.c(4,0);X.lo.hi<-coefs.c(4,2); sd.hat0<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat))); second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo); ci.lo0<-quantile(second.difference.ci,(0.05));ci.hi0<-quantile(second.difference.ci,(0.95));
X.hi.lo<-coefs.c(6,1);X.hi.hi<-coefs.c(6,3); X.lo.lo<-coefs.c(4,1);X.lo.hi<-coefs.c(4,3); sd.hat1<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat))); second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo); ci.lo1<-quantile(second.difference.ci,(0.05));ci.hi1<-quantile(second.difference.ci,(0.95));
X.hi.lo<-coefs.c(6,2);X.hi.hi<-coefs.c(6,4);X.lo.lo<-coefs.c(4,2);X.lo.hi<-coefs.c(4,4);sd.hat2<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat)));second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo);ci.lo2<-quantile(second.difference.ci,(0.05));ci.hi2<-quantile(second.difference.ci,(0.95));
X.hi.lo<-coefs.c(6,3);X.hi.hi<-coefs.c(6,5);X.lo.lo<-coefs.c(4,3);X.lo.hi<-coefs.c(4,5);sd.hat3<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat)));second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo);ci.lo3<-quantile(second.difference.ci,(0.05));ci.hi3<-quantile(second.difference.ci,(0.95));
X.hi.lo<-coefs.c(6,4);X.hi.hi<-coefs.c(6,6);X.lo.lo<-coefs.c(4,4);X.lo.hi<-coefs.c(4,6);sd.hat4<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat)));second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo);ci.lo4<-quantile(second.difference.ci,(0.05));ci.hi4<-quantile(second.difference.ci,(0.95));
X.hi.lo<-coefs.c(6,5);X.hi.hi<-coefs.c(6,7);X.lo.lo<-coefs.c(4,5);X.lo.hi<-coefs.c(4,7);sd.hat5<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat)));second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo);ci.lo5<-quantile(second.difference.ci,(0.05));ci.hi5<-quantile(second.difference.ci,(0.95));
#create data.frame
v<- rbind( "0 to 2", "1 to 3",  "2 to 4",  "3 to 5",	"4 to 6",	"5 to 7")
div.values<- rbind( 1,  2,  3,	4,	5,	6)
bind.sd.hat<- rbind(sd.hat0,sd.hat1, sd.hat2, sd.hat3, sd.hat4, sd.hat5)
bind.ci.lo<-rbind(ci.lo0, ci.lo1,  ci.lo2,	ci.lo3,	ci.lo4,	ci.lo5)
bind.ci.hi<- rbind(ci.hi0, ci.hi1,  ci.hi2,  ci.hi3,	ci.hi4,	ci.hi5)
bind.all.sd<- data.frame(div.values, bind.sd.hat, bind.ci.lo, bind.ci.hi, v)
bind<- bind.all.sd 
#plot
sd.plot <- ggplot(bind, aes(x = v, y = bind.sd.hat))+ scale_color_manual(values=c( "blue"))+ geom_linerange(aes(x = v, ymin = X5.,ymax = X95.),lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = v, y = bind.sd.hat, ymin = X5.,ymax = X95.),lwd = 1/2, position = position_dodge(width = 1/2))+ geom_point(aes(),size = 7, position=position_dodge(width=1/2))+ scale_shape_manual(values = c(20, 19)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Years on Facebook")+ labs(y = expression( "Pr(Vote)" ~ Delta))+ theme(text = element_text(size=30))+ geom_hline(aes(yintercept=0), colour="firebrick", linetype="dashed")
print(sd.plot)

########################
#Model.1 from secondary analysis
Model.1 <- glm(General2012 ~ Friendslog2012*YearsOnFb2012*AgeBi+  AgeBi +Friendslog2012+ YearsOnFb2012+ Female+ PartyID2012+ State, family = binomial,data = d) 
display(Model.1, detail = TRUE)
m<- Model.1
#set mean/sd values using facebook subset
s <- d[ which(d$Facebook2012>=1),] #restrict s2 to fb data only (do not impute true NAs Facebook = 0)
#x1= Friendslog2012
fr0<-0
fr1<- mean(s$Friendslog2012) 
fr2<-fr1+ sd(s$Friendslog2012) #mean +1sd (5.575641)
#x2= YearsOnFb2012
yr0<-0
yr1<-mean(s$YearsOnFb2012) #mean
yr2<-yr1+ sd(s$YearsOnFb2012)#mean +1sd
#x3= Age
a1<-1
a2<-0
#Predicted Probabilities
beta.hat<- coef(m)
Sigma.hat <- vcov(m)
#x1= Friendslog2012, x2= YearsOnFb2012, x3= Age, all other vars set to means/mode  
coefs.c<- function(x1,x2, x3){ X.c <- c(1,x1,x2,x3,1,1,0,0,1,0,0,(x1*x2), (x1*x3), (x2*x3),(x1*x2*x3) );return(X.c)}
#set values for x1:x3
#create functions
#probabilities
predicted.prob <- function(X.c.value, beta.hat.value ){p.hat <- plogis(X.c.value%*%beta.hat.value); return(p.hat)}
#simulate confidence intervals 
p.ci.sim <- function(beta.hat.value, Sigma.hat.value, X.c.value){n.sims <- 10000;  p.tilde <- numeric(n.sims);  for (i in 1:n.sims) {beta.tilde <- mvrnorm(1, beta.hat.value, Sigma.hat.value); p.tilde[i] <- plogis(X.c.value%*%beta.tilde)};return(p.tilde)}
#create points for graph (low): 
setX.c<-coefs.c(fr0,yr0,a1);prob1<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.1<-quantile(predicted.prob.ci,(0.05));ci.hi.1<-quantile(predicted.prob.ci,(0.95))
setX.c<-coefs.c(fr1,yr1,a1);prob2<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.2<-quantile(predicted.prob.ci,(0.05));ci.hi.2<-quantile(predicted.prob.ci,(0.95))
setX.c<-coefs.c(fr2,yr2,a1);prob3<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.3<-quantile(predicted.prob.ci,(0.05));ci.hi.3<-quantile(predicted.prob.ci,(0.95))
#create data.frame
div.values<- rbind(1,2,3)
bind.prob<- rbind(prob1,  prob2,	prob3)
bind.lo.ci.lo<-rbind(ci.lo.1,  ci.lo.2,	ci.lo.3)
bind.lo.ci.hi<- rbind(ci.hi.1,  ci.hi.2,  ci.hi.3)
Model<- c("agelow", "agelow","agelow")
bind.all.prob.lo<- data.frame(div.values, bind.prob, bind.lo.ci.lo, bind.lo.ci.hi, Model)
##create points for graph (high): 
setX.c<-coefs.c(fr0,yr0,a2);prob1<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.1<-quantile(predicted.prob.ci,(0.05));ci.hi.1<-quantile(predicted.prob.ci,(0.95))
setX.c<-coefs.c(fr1,yr1,a2);prob2<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.2<-quantile(predicted.prob.ci,(0.05));ci.hi.2<-quantile(predicted.prob.ci,(0.95))
setX.c<-coefs.c(fr2,yr2,a2);prob3<-predicted.prob(setX.c,beta.hat);predicted.prob.ci<-p.ci.sim(beta.hat,Sigma.hat,setX.c);ci.lo.3<-quantile(predicted.prob.ci,(0.05));ci.hi.3<-quantile(predicted.prob.ci,(0.95))
#create data.frame
div.values<- rbind(1,2,3)
bind.prob<- rbind(prob1,  prob2,	prob3)
bind.lo.ci.lo<-rbind(ci.lo.1,  ci.lo.2,	ci.lo.3)
bind.lo.ci.hi<- rbind(ci.hi.1,  ci.hi.2,  ci.hi.3)
Model<- c("agehi", "agehi","agehi")
bind.all.prob.hi<- data.frame(div.values, bind.prob, bind.lo.ci.lo, bind.lo.ci.hi, Model)
#combine low/high data.frames
total <- rbind(bind.all.prob.lo, bind.all.prob.hi)
total$Model<-ifelse(total$Model=="agelow",  "18-25 Years","> 25 Years")
#graph predicted probabilities
dev.off() 
prob.plot.2 <- ggplot(total, aes( x = div.values, y = bind.prob, shape= Model, colour = Model, group = Model)) + geom_point(aes(),size = 8, position=position_dodge(width=1/2))+
  geom_linerange(aes(x = div.values, ymin = X5.,ymax = X95.),lwd = 1, position = position_dodge(width = 1/2)) + geom_pointrange(aes(x = div.values, y = bind.prob, ymin = X5.,ymax = X95.),lwd = 1/2, position = position_dodge(width = 1/2))+
  scale_shape_manual(name="Age",values = c(18,18))  + scale_color_manual(name="Age",values=c( "navyblue", "lightskyblue"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Social Pressure Score")+ labs(y = "Pr(Vote)")+ theme(text = element_text(size=24)) + theme(legend.position = c(0.75, 0.3))+
  scale_x_continuous(breaks=c(1,2,3 ), labels=c(expression( bar(x) ~"- 1sd" ),expression(bar(x)),expression( bar(x) ~"+ 1sd" )))
#view
prob.plot.2

##################
#first differences (between 0, x + 1s) 
#create points for graph (high )
X.hi.lo<-coefs.c(fr0,yr0,a2);X.hi.hi<-coefs.c(fr2,yr2,a2);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd1<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.1<-quantile(hi.fd.ci,(0.05));ci.hi.1<-quantile(hi.fd.ci,(0.95));ci.lo.1.1<-quantile(hi.fd.ci,(0.025));ci.hi.1.1<-quantile(hi.fd.ci,(0.975))
#create data.frame
v<- rbind( "1")
div.values<- rbind( 1)
bind.fd<- rbind( hi.fd1)
bind.hi.ci.lo<-rbind(ci.lo.1)
bind.hi.ci.hi<- rbind(ci.hi.1)
bind.hi.ci.lo.1<-rbind( ci.lo.1.1)
bind.hi.ci.hi.1<- rbind( ci.hi.1.1)
Model<- c("hi")
bind.all.fd.hi<- data.frame(div.values, bind.fd, bind.hi.ci.lo, bind.hi.ci.hi, bind.hi.ci.lo.1, bind.hi.ci.hi.1, Model, v)
#create points for graph (low )
X.lo.lo<-coefs.c(fr0,yr0,a1);X.lo.hi<-coefs.c(fr2,yr2,a1);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd1<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.1<-quantile(lo.fd.ci,(0.05));ci.hi.1<-quantile(lo.fd.ci,(0.95));ci.lo.1.1<-quantile(lo.fd.ci,(0.025));ci.hi.1.1<-quantile(lo.fd.ci,(0.975))
#create data.frame
v<- rbind( "1")
div.values<- rbind( 1)
bind.fd<- rbind( lo.fd1)
bind.lo.ci.lo<-rbind( ci.lo.1)
bind.lo.ci.hi<- rbind( ci.hi.1)
bind.lo.ci.lo.1<-rbind( ci.lo.1.1)
bind.lo.ci.hi.1<- rbind( ci.hi.1.1)
Model<- c("lo")
bind.all.fd.lo<- data.frame(div.values, bind.fd, bind.lo.ci.lo, bind.lo.ci.hi, bind.lo.ci.lo.1, bind.lo.ci.hi.1, Model, v)
#combine low/high data.frames
total1 <- rbind(bind.all.fd.lo, bind.all.fd.hi)
total1<- data.frame(total1) #add in percents with blanks (first two, last two only)
total1$Model<-ifelse(total1$Model=="lo",  "18-25 ","> 25")
#plot
dev.off() 
fd.plot.2 <- ggplot(total1, aes( x = v, y = bind.fd, shape= Model, colour = Model)) + geom_point(aes(),size = 7, position=position_dodge(width=1/2)) + 
  geom_linerange(aes(x = v, ymin = X5., ymax = X95.), lwd = 1, position = position_dodge(width = 1/2)) +geom_pointrange(aes(x = v, y = bind.fd, ymin = X2.5.,ymax = X97.5.), lwd = 1/2, position = position_dodge(width = 1/2)) +
  scale_shape_manual(name="Age", values = c(18, 18))  +  scale_color_manual(name="Age", values=c( "navyblue", "lightskyblue"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = expression( Delta ~ "Social Pressure Score" ))+labs(y = expression( "Pr(Vote)" ~ Delta)) +
  theme(text = element_text(size=24)) +theme(legend.position = c(0.85, 0.85)) + 
  geom_hline(aes(yintercept=0), colour="firebrick", linetype="dashed")+scale_x_discrete(breaks=c(1 ), labels=c(expression( bar(x) ~"- 1sd, " ~bar(x) ~"+ 1sd" )))
fd.plot.2

##################
#first differences (between 0, xbar , xbar + 1sd) 
#create points for graph (high )
X.hi.lo<-coefs.c(fr0,yr0,a2);X.hi.hi<-coefs.c(fr1,yr1,a2);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd1<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.1<-quantile(hi.fd.ci,(0.05));ci.hi.1<-quantile(hi.fd.ci,(0.95));ci.lo.1.1<-quantile(hi.fd.ci,(0.025));ci.hi.1.1<-quantile(hi.fd.ci,(0.975))
X.hi.lo<-coefs.c(fr1,yr1,a2);X.hi.hi<-coefs.c(fr2,yr2,a2);p.hi.lo<-predicted.prob(X.hi.lo,beta.hat);p.hi.hi<-predicted.prob(X.hi.hi,beta.hat);hi.fd2<-first.difference(p.hi.hi,p.hi.lo);hi.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.hi.lo);ci.lo.2<-quantile(hi.fd.ci,(0.05));ci.hi.2<-quantile(hi.fd.ci,(0.95));ci.lo.2.2<-quantile(hi.fd.ci,(0.025));ci.hi.2.2<-quantile(hi.fd.ci,(0.975))
#create data.frame
v<- rbind( 1,2)
div.values<- rbind( 1,2)
bind.fd<- rbind( hi.fd1, hi.fd2)
bind.hi.ci.lo<-rbind(ci.lo.1,ci.lo.2)
bind.hi.ci.hi<- rbind(ci.hi.1,ci.hi.2)
bind.hi.ci.lo.1<-rbind(ci.lo.1.1,ci.lo.2.2)
bind.hi.ci.hi.1<- rbind(ci.hi.1.1,ci.hi.2.2)
Model<- c("hi", "hi")
bind.all.fd.hi<- data.frame(div.values, bind.fd, bind.hi.ci.lo, bind.hi.ci.hi,bind.hi.ci.lo.1, bind.hi.ci.hi.1, Model, v)
#create points for graph (low )
X.lo.lo<-coefs.c(fr0,yr0,a1);X.lo.hi<-coefs.c(fr1,yr1,a1);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd1<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.1<-quantile(lo.fd.ci,(0.05));ci.hi.1<-quantile(lo.fd.ci,(0.95));ci.lo.1.1<-quantile(lo.fd.ci,(0.025));ci.hi.1.1<-quantile(lo.fd.ci,(0.975))
X.lo.lo<-coefs.c(fr1,yr1,a1);X.lo.hi<-coefs.c(fr2,yr2,a1);p.hi.lo<-predicted.prob(X.lo.lo,beta.hat);p.hi.hi<-predicted.prob(X.lo.hi,beta.hat);lo.fd2<-first.difference(p.hi.hi,p.hi.lo);lo.fd.ci<-fd.ci.sim(beta.hat,Sigma.hat,X.lo.hi,X.lo.lo);ci.lo.2<-quantile(lo.fd.ci,(0.05));ci.hi.2<-quantile(lo.fd.ci,(0.95));ci.lo.2.2<-quantile(lo.fd.ci,(0.025));ci.hi.2.2<-quantile(lo.fd.ci,(0.975))
#create data.frame
v<- rbind( 1,2)
div.values<- rbind( 1,2 )
bind.fd<- rbind( lo.fd1, lo.fd2)
bind.lo.ci.lo<-rbind( ci.lo.1, ci.lo.2)
bind.lo.ci.hi<- rbind( ci.hi.1, ci.hi.2)
bind.lo.ci.lo.2<-rbind( ci.lo.1.1, ci.lo.2.2)
bind.lo.ci.hi.2<- rbind( ci.hi.1.1, ci.hi.2.2)
Model<- c("lo", "lo")
bind.all.fd.lo<- data.frame(div.values, bind.fd, bind.lo.ci.lo, bind.lo.ci.hi, bind.lo.ci.lo.2, bind.lo.ci.hi.2, Model, v)
#combine low/high data.frames
total3 <- rbind(bind.all.fd.lo, bind.all.fd.hi)
total3<- data.frame(total3) 
total3$Model<-ifelse(total3$Model=="lo",  "18-25","> 25")
#plot
fd.plot.3 <- ggplot(total3, aes( x = v, y = bind.fd, shape= Model, colour = Model)) +
  geom_point(aes(),size = 7, position=position_dodge(width=1/2)) + 
  geom_linerange(aes(x = v, ymin = X5., ymax = X95.), lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = v, y = bind.fd, ymin = X2.5.,  ymax = X97.5.), lwd = 1/2, position = position_dodge(width = 1/2)) +
  scale_shape_manual(name="Age", values = c(18, 18))  + scale_color_manual(name="Age", values=c( "navyblue", "lightskyblue"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = expression( Delta ~ "Social Pressure Score" ))+
  labs(y = expression( "Pr(Vote)" ~ Delta))+theme(text = element_text(size=24)) +theme(legend.position = c(0.75, 0.75)) + 
  geom_hline(aes(yintercept=0), colour="firebrick", linetype="dashed")+ scale_x_continuous(breaks=c(1,2), labels=c(expression( bar(x) ~"- 1sd, " ~bar(x) ),expression( bar(x) ~"," ~bar(x) ~"+ 1sd" )))
fd.plot.3

#second differences (between 0, x + 1s)
#create points for graph 
X.hi.lo<-coefs.c(fr0,yr0,a2);X.hi.hi<-coefs.c(fr2,yr2,a2); X.lo.lo<-coefs.c(fr0,yr0,a1);X.lo.hi<-coefs.c(fr2,yr2,a1); sd.hat0<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat))); second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo); ci.lo0<-quantile(second.difference.ci,(0.05));ci.hi0<-quantile(second.difference.ci,(0.95));ci.lo0.1<-quantile(second.difference.ci,(0.025));ci.hi0.1<-quantile(second.difference.ci,(0.975));
#create data.frame
v<- rbind( 1)
div.values<- rbind( 1)
bind.sd.hat<- rbind(sd.hat0)
bind.ci.lo<-rbind(ci.lo0)
bind.ci.hi<- rbind(ci.hi0)
bind.ci.lo.1<-rbind(ci.lo0.1)
bind.ci.hi.1<- rbind(ci.hi0.1)
Model<- "0 to 1sd"
bind.all.sd<- data.frame(div.values, bind.sd.hat, bind.ci.lo, bind.ci.hi, bind.ci.lo.1, bind.ci.hi.1, v)
bind<- bind.all.sd #may have to run twice 
#plot
sd.plot.2 <- ggplot(bind, aes( x = v, y = bind.sd.hat, shape= Model, colour = Model)) + 
  geom_point(aes(),size = 9, position=position_dodge(width=1/2)) +
  geom_errorbar(aes(x = v, ymin = X5., ymax = X95.), lwd = 1, position = position_dodge(width = .1), width= .00) + 
  geom_pointrange(aes(x = v,y = bind.sd.hat, ymin = X2.5., ymax = X97.5.), lwd = 1/2, position = position_dodge(width = 1/2)) +
  scale_shape_manual(name="Age", values = c(19))  + 
  scale_color_manual(name="Age", values=c( "steelblue"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = expression( Delta ~ "Social Pressure Score" ))+ labs(y = expression( "Pr(Vote)" ~ Delta))+
  theme(text = element_text(size=24)) + theme(legend.position="none")+ 
  geom_hline(aes(yintercept=0.00), colour="firebrick", linetype="dashed", size=1)+
  scale_x_continuous(breaks=c(1 ), labels=c(expression( bar(x) ~"- 1sd, " ~bar(x) ~"+ 1sd" )))
sd.plot.2


#second differences ( 0, xbar , xbar + 1sd) 
#create points for graph 
X.hi.lo<-coefs.c(fr0,yr0,a2);X.hi.hi<-coefs.c(fr1,yr1,a2); X.lo.lo<-coefs.c(fr0,yr0,a1);X.lo.hi<-coefs.c(fr1,yr1,a1); sd.hat1<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat))); second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo); ci.lo1<-quantile(second.difference.ci,(0.05));ci.hi1<-quantile(second.difference.ci,(0.95));ci.lo1.1<-quantile(second.difference.ci,(0.025));ci.hi1.1<-quantile(second.difference.ci,(0.975))
X.hi.lo<-coefs.c(fr1,yr1,a2);X.hi.hi<-coefs.c(fr2,yr2,a2); X.lo.lo<-coefs.c(fr1,yr1,a1);X.lo.hi<-coefs.c(fr2,yr2,a1); sd.hat2<-((predicted.prob(X.hi.hi,beta.hat))-(predicted.prob(X.lo.hi,beta.hat)))-((predicted.prob(X.hi.lo,beta.hat))-(predicted.prob(X.lo.lo,beta.hat))); second.difference.ci<-sd.ci.sim(beta.hat,Sigma.hat,X.hi.hi,X.lo.hi,X.hi.lo,X.lo.lo); ci.lo2<-quantile(second.difference.ci,(0.05));ci.hi2<-quantile(second.difference.ci,(0.95));ci.lo2.2<-quantile(second.difference.ci,(0.025));ci.hi2.2<-quantile(second.difference.ci,(0.975))
#create data.frame
v<- rbind( 1,2)
Model<- rbind( " 0,1", "1to2")
div.values<- rbind( 1, 2)
bind.sd.hat<- rbind(sd.hat1, sd.hat2)
bind.ci.lo<-rbind(ci.lo1, ci.lo2)
bind.ci.hi<- rbind(ci.hi1, ci.hi2)
bind.ci.lo.1<-rbind(ci.lo1.1, ci.lo2.2)
bind.ci.hi.1<- rbind(ci.hi1.1, ci.hi2.2)
bind.all.sd<- data.frame(div.values, bind.sd.hat, bind.ci.lo, bind.ci.hi,bind.ci.lo.1, bind.ci.hi.1, v)
bind2<- bind.all.sd
#plot
sd.plot.3 <- ggplot(bind2, aes( x = v, y = bind.sd.hat, shape= Model, colour = Model)) + 
  geom_point(aes(),size = 9, position=position_dodge(width=1/2)) + geom_errorbar(aes(x = v, ymin = X5.,  ymax = X95.), lwd = 1, position = position_dodge(width = .1), width= .00) + 
  geom_pointrange(aes(x = v,y = bind.sd.hat, ymin = X2.5., ymax = X97.5.), lwd = 1/2, position = position_dodge(width = 1/2)) + 
  scale_shape_manual(name="Age", values = c(19,19))  + scale_color_manual(name="Age", values=c( "steelblue", "steelblue"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = expression( Delta ~ "Social Pressure Score" ))+ labs(y = expression( "Pr(Vote)" ~ Delta))+ theme(text = element_text(size=24)) + theme(legend.position="none")+ 
  geom_hline(aes(yintercept=0.002), colour="firebrick", linetype="dashed", size=1) +
  scale_x_continuous(breaks=c(1,2), labels=c(expression( bar(x) ~"- 1sd, " ~bar(x) ),expression( bar(x) ~"," ~bar(x) ~"+ 1sd" )))
sd.plot.3
