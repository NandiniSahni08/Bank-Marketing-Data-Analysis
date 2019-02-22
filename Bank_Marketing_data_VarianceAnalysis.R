#Import Data
bank <- read.csv("~/Documents/Study/Semester2/Multivariate/bank-additional/bank-additional-full.csv", sep=";")
bank_marketing <- bank 

#View(bank_marketing)
head(bank_marketing)
str(bank_marketing)

#changing marital from factor to integer married 3, single 2, divorced 1 and unknown 0

bank_marketing_marital=ifelse(bank$marital=='married',3,
                    ifelse(bank$marital=='single',2,
                           ifelse(bank$marital=='divorced',1,0))) 

#Change the category of housing from factor to integer
bank_marketing_housing = ifelse(bank_marketing$housing=='yes',1,0)





#Changing the education column to numerical column by assigning the digits as per the highest in order
bank_marketing_education=ifelse(bank_marketing$education=='illiterate',1,
                                ifelse(bank_marketing$education=='basic.6y',2,
                                       ifelse(bank_marketing$education=='basic.4y',3,
                                              ifelse(bank_marketing$education=='professional.course',4,
                                                     ifelse(bank_marketing$education=='basic.9y',5,
                                                            ifelse(bank_marketing$education=='high.school',6,
                                                                   ifelse(bank_marketing$education=='university.degree',7,0) ))))))



#Changing Jo column from categorical to numerical and assigning them values in descending order
#unknown = 0 ,student=1, unemployed=2, housemaid=3
#self-employed 4, entrepreneur 5, retired 6, management 7,services 8 , technician 9
# admin 11   blue-collar    10               
bank_marketing_job= ifelse(bank_marketing$job== 'admin.', 11,
                           ifelse(bank_marketing$job=='blue-collar', 10,
                                  ifelse(bank_marketing$job=='technician',9,
                                         ifelse(bank_marketing$job=='services',8,
                                                ifelse(bank_marketing$job=='management',7,
                                                       ifelse(bank_marketing$job=='retired',6,
                                                              ifelse(bank_marketing$job=='entrepreneur',5,
                                                                     ifelse(bank_marketing$job=='self-employed',4,
                                                                            ifelse(bank_marketing$job=='housemaid',3,
                                                                                   ifelse(bank_marketing$job=='unemployed',2,
                                                                                          ifelse(bank_marketing$job=='student',1,0)))))))))))
#Changing the value of day_of_week to numerical value as per the days starting from monday = 1 and so on
bank_marketing_days=ifelse(bank_marketing$day_of_week=='mon',1,
                           ifelse(bank_marketing$day_of_week=='tue',2,
                                  ifelse(bank_marketing$day_of_week=='wed',3,
                                         ifelse(bank_marketing$day_of_week=='thu',4,
                                                ifelse(bank_marketing$day_of_week=='fri',5,0)))))


#Changing the value of contact
bank_marketing_contact=ifelse(bank_marketing$contact=='cellular',2,1)

#changing the value of loan
bank_marketing_loan= ifelse(bank_marketing$loan=='yes',1,0)

#changing the category of month
bank_marketing_month=ifelse(bank_marketing$month=='mar',3,
                            ifelse(bank_marketing$month=='apr',4,
                                   ifelse(bank_marketing$month=='may',5,
                                          ifelse(bank_marketing$month=='jun',6,
                                                 ifelse(bank_marketing$month=='jul',7,
                                                        ifelse(bank_marketing$month=='aug',8,
                                                               ifelse(bank_marketing$month=='sep',9,
                                                                      ifelse(bank_marketing$month=='oct',10,
                                                                             ifelse(bank_marketing$month=='nov',11,
                                                                                    ifelse(bank_marketing$month=='dec',12,0))))))))))

#Changing the category of y
bank_y=ifelse(bank$y=='yes',1,0) 

bank_modified=cbind(bank_marketing,bank_marketing_housing)
bank_modified=cbind(bank_modified,bank_marketing_marital)
bank_modified=cbind(bank_modified,bank_marketing_contact)
bank_modified=cbind(bank_modified,bank_marketing_days)
bank_modified=cbind(bank_modified, bank_marketing_month)
bank_modified=cbind(bank_modified, bank_marketing_loan)
bank_modified=cbind(bank_modified, bank_marketing_job)
bank_modified=cbind(bank_modified, bank_marketing_education)

bank_modified=cbind(bank_modified,bank_y)
str(bank_modified)
head(bank_modified)
#View(bank_modified)




#Performing t-test on every categorical column converted into integer columns
t.test(bank_modified$bank_marketing_housing[bank_modified$y=='yes'],bank_modified$bank_housing[bank_modified$y=='no'],var.equal=TRUE)
t.test(bank_modified$bank_marketing_days[bank_modified$y=='yes'],bank_modified$bank_marketing_days[bank_modified$y=='no'],var.equal=TRUE)
t.test(bank_modified$bank_marketing_month[bank_modified$y=='yes'],bank_modified$bank_marketing_month[bank_modified$y=='no'],var.equal=TRUE)
t.test(bank_modified$bank_marketing_job[bank_modified$y=='yes'],bank_modified$bank_marketing_job[bank_modified$y=='no'],var.equal=TRUE)
t.test(bank_modified$bank_marketing_loan[bank_modified$y=='yes'],bank_modified$bank_marketing_loan[bank_modified$y=='no'],var.equal=TRUE)
t.test(bank_modified$bank_marketing_contact[bank_modified$y=='yes'],bank_modified$bank_marketing_contact[bank_modified$y=='no'],var.equal=TRUE)
t.test(bank_modified$bank_marketing_education[bank_modified$y=='yes'],bank_modified$bank_marketing_education[bank_modified$y=='no'],var.equal=TRUE)
t.test(bank_modified$bank_marketing_marital[bank_modified$y=='yes'],bank_modified$bank_marketing_marital[bank_modified$y=='no'],var.equal=TRUE)

#Performing T-test on integer columns
with(data=bank_marketing,t.test(age[y=="no"],age[y=="yes"],var.equal=TRUE))
with(data=bank_marketing,t.test(duration[y=="no"],duration[y=="yes"],var.equal=TRUE))

#Applying LeveneTest on the univariate data
library(car)
leveneTest(age ~ y, data = bank_modified)
leveneTest(duration ~ y, data = bank_modified)
leveneTest(bank_marketing_contact ~ y, data = bank_modified)
leveneTest(bank_marketing_job ~ y, data = bank_modified)
leveneTest(bank_marketing_loan ~ y, data = bank_modified)
leveneTest(bank_marketing_education ~ y, data = bank_modified)
leveneTest(bank_marketing_month ~ y, data = bank_modified)
leveneTest(bank_marketing_housing ~ y, data = bank_modified)
leveneTest(bank_marketing_marital ~ y, data = bank_modified)


#Selecting only integer columns in table ank_marketing_int
bank_marketing_int<- bank_modified[,c(1,11:14,16,17,18,19,20,22:30)]
#View(bank_marketing_int)

#F-Test for univariate variables
var.test(bank_marketing_int$age[bank_marketing_int$bank_y=="1"],bank_marketing_int$age[bank_marketing_int$bank_y=="0" ])
var.test(bank_marketing_int$duration[bank_marketing_int$bank_y=="1"],bank_marketing_int$duration[bank_marketing_int$bank_y=="0" ])
var.test(bank_marketing_int$euribor3m[bank_marketing_int$bank_y=="1"],bank_marketing_int$euribor3m[bank_marketing_int$bank_y=="0" ])


#Computing means and covariance
cm <- colMeans(bank_marketing_int)
S <- cov(bank_marketing_int) #diagonals are variances

#Matrix multiplication: solve(s)->find inverse of the matrix
d <- apply(bank_marketing_int, MARGIN = 1, function(bank_marketing_int)t(bank_marketing_int - cm) %*% solve(S) %*% (bank_marketing_int - cm))
d

qqnorm(bank_marketing_int[,"age"], main = "age")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"age"])

qqnorm(bank_marketing_int[,"duration"], main = "duration")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"duration"])

qqnorm(bank_marketing_int[,"campaign"], main = "campaign")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"campaign"])

qqnorm(bank_marketing_int[,"pdays"], main = "pdays")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"pdays"])

qqnorm(bank_marketing_int[,"bank_marketing_education"], main = "bank_marketing_education")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"bank_marketing_education"])

qqnorm(bank_marketing_int[,"bank_marketing_job"], main = "bank_marketing_job")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"bank_marketing_job"])

qqnorm(bank_marketing_int[,"bank_marketing_loan "], main = "bank_marketing_loan ")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"bank_marketing_loan "])

qqnorm(bank_marketing_int[,"bank_marketing_month "], main = "bank_marketing_month ")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"bank_marketing_month "])

qqnorm(bank_marketing_int[,"bank_marketing_marital "], main = "bank_marketing_marital ")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"bank_marketing_marital "])

qqnorm(bank_marketing_int[,"bank_marketing_housing "], main = "bank_marketing_housing ")
#how nomal looks like - univariate normalization
qqline(bank_marketing_int[,"bank_marketing_housing "])


#individually they had outliers
#all of them together or how they interact with each other
#they look they are normally multivariate
plot(qchisq((1:nrow(bank_marketing_int) - 1/2) / nrow(bank_marketing_int), df = 3),sort(d))
abline(a = 0, b = 1)
                            

#Hotelling Test    
library(Hotelling)
t2testbank_int <- hotelling.test( . ~ bank_y, data=bank_marketing_int) 
cat("T2 statistic =",t2testbank_int$stat[[1]],"\n")

print(t2testbank_int)
View(t2testbank_int)





