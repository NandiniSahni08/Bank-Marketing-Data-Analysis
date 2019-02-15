bank <- read.csv("~/Documents/Study/Semester2/Multivariate/bank-additional/bank-additional-full.csv", sep=";")
bank_marketing <- bank 

#View(bank_marketing)
head(bank_marketing)
str(bank_marketing)


#Boxplots to check for any outliers
boxplot(bank_marketing$age, main="Age Box plot",yaxt="n", xlab="Age", horizontal=TRUE)
boxplot(bank_marketing$euribor3m, main="Euribor3m Box plot",yaxt="n", xlab="euribor3m", horizontal=TRUE)
boxplot(bank_marketing$duration, main="Duration Box plot",yaxt="n", xlab="Duration", horizontal=TRUE)

#Plotting Histograms and bargraphs for different coulmns using ggplot
library(ggplot2)
ggplot(bank_marketing,aes(x=bank_marketing$age,fill=bank_marketing$y)) + geom_histogram(binwidth=1) +
  labs(y= "Count", x="Age", title = "Age")
ggplot(bank_marketing, aes(x=bank_marketing$job,fill=bank_marketing$y)) + geom_bar()+
  labs(y= "Count", x="Job", title = "Job")
ggplot(bank_marketing, aes(x=bank_marketing$marital,fill=bank_marketing$y)) + geom_bar() +
  labs(y= "Count", x="Marital", title = "Marital")
ggplot(bank_marketing, aes(x=bank_marketing$education,fill=bank_marketing$y)) + geom_bar()+
  labs(y= "Count", x="Education", title = "Education")
ggplot(bank_marketing, aes(x=bank_marketing$default,fill=bank_marketing$y)) + geom_bar()+
  labs(y= "Count", x="Contact", title = "Contact")
ggplot(bank_marketing, aes(x=bank_marketing$housing,fill=bank_marketing$y)) + geom_bar()+
  labs(y= "Count", x="Contact", title = "Contact")
ggplot(bank_marketing, aes(x=bank_marketing$contact,fill=bank_marketing$y)) + geom_bar()+
  labs(y= "Count", x="Contact", title = "Contact")
ggplot(bank_marketing, aes(x=bank_marketing$loan,fill=bank_marketing$y)) + geom_bar()+
  labs(y= "Count", x="Laon", title = "Loan")
ggplot(bank_marketing, aes(x=bank_marketing$month,fill=bank_marketing$y)) + geom_bar()+
  labs(y= "Count", x="Month", title = "Month")
ggplot(bank_marketing, aes(x=bank_marketing$day_of_week,fill=bank_marketing$y)) + geom_bar()+
  labs(y= "Count", x="Day of week", title = "Day of week")
ggplot(bank_marketing, aes(x=bank_marketing$poutcome,fill=bank_marketing$y)) + geom_bar() +
  labs(y= "Count", x="poutcome", title = "Poutcome")


#Diagonal boxplot
library(SciViews)
bank_marketing_int=bank_marketing[c(11:14)]
pairs(bank_marketing_int, labels=c("Duration","Campaign","pdays","previous"),pch=c(1,16)[as.numeric(bank_marketing_int$y)],font.labels=2)

#3d Scatterplot plotted between age, duration and campaign to analyse the relationship between the three
library(scatterplot3d)
s3d <- scatterplot3d(bank_marketing$age,bank_marketing$duration,bank_marketing$campaign,pch=c(1,16)[as.numeric(bank$y)],xlab="Age", ylab="Duration", angle=45,zlab="Campaign", lty.hide=2,type="h",y.margin.add=0.1,font.axis=2,font.lab=2)

library(MVA)
#Chiplot plotted to analyse the relation between age and duration
mlab = "Age of the Customer"
plab = "Duration"
with(bank_marketing, plot(age, duration, xlab = mlab, ylab = plab, cex.lab = 0.9))
with(bank_marketing, chiplot(age, duration))

#Scatterplots
attach(bank_marketing)
plot(age, duration, pch=c(1,16)[as.numeric(y)],xlab="Age",ylab="Duration")
plot(age, campaign, pch=c(1,16)[as.numeric(y)], xlab="Age", ylab="Campaign")
detach(bank_marketing)

#pairs(bank_marketing, panel = function (x, y, ...) {points(x, y, ...);abline(lm(y ~ x), col = "grey")}, pch = ".", cex = 1.5)

#ScatterplotMatrix
library(car)

scatterplotMatrix(~age+duration+campaign+pdays+previous | bank_marketing$y, data=bank_marketing_int, var.labels=c("age","Duration","Campaign","pdays","previous"),cex.labels=0.7, diagonal="boxplot",smooth=FALSE,reg.line=FALSE,pch=c(1,16),col=rep("black",2), legend.plot=FALSE)

#bvplot
bank_marketing_age_dur=data.frame(bank_marketing$age, bank_marketing$duration)
bvbox(bank_marketing_age_dur, mtitle = "", xlab = mlab, ylab = plab)

y_int=ifelse(bank_marketing$y=='no', 0, 1)

plot(bank_marketing$age, bank_marketing$duration, pch=c(1,16)[y_int],xlab="Age",ylab="Duration")
plot(bank_marketing$age, bank_marketing$campaign, pch=c(1,16)[y_int],xlab="Age", ylab="Campaign")

#Instead of using splom using psych library it includes splom , and give better correlation for factor features
library(psych)
pairs.panels(bank[,c(1:8,21)])
pairs.panels(bank[,c(9:21)])


library(ggplot2)
qplot(bank_marketing$pdays,bank_marketing$duration,data=bank_marketing,colour=y,size=3)









