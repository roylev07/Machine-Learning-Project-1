

library(mice)
library(reshape2)

library(MASS)
library(fitdistrplus)
library(magrittr)
library(simmer)
library(simmer.plot)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)

library(sqldf)
library(knitr)
library(rmarkdown)

library(qcc)

# ------------------------------------------------------------------------------------------------
  
filePath=choose.files() 
Bank_Data<-read.csv(filePath,header=TRUE)

# חישובי הסתברויות אפריוריות

Y_apriori_Yes <- length(which(Bank_Data$y==1))/length(Bank_Data$y)
Y_apriori_NO <- 1-Y_apriori_Yes
hist(Bank_Data$y)

hist(Bank_Data$age, freq=FALSE)
apriori_age_under_25 <- as.numeric(sqldf("select count (age) from Bank_Data where age <=25" ))/length(Bank_Data$age)
apriori_age_26_30 <- as.numeric(sqldf("select count (age) from Bank_Data where age between 26 and 30" ))/length(Bank_Data$age)
apriori_age_31_40 <- as.numeric(sqldf("select count (age) from Bank_Data where age between 31 and 40" ))/length(Bank_Data$age)
apriori_age_41_55 <- as.numeric(sqldf("select count (age) from Bank_Data where age between 41 and 55" ))/length(Bank_Data$age)
apriori_age_above_56 <- as.numeric(sqldf("select count (age) from Bank_Data where age >=56" ))/length(Bank_Data$age)


barplot(prop.table(table(Bank_Data$job)))
apriori_job_admin <- length(which(Bank_Data$job=="admin."))/(length(Bank_Data$job)-length(which(Bank_Data$job=="unknown")))
apriori_job_blue_collar <- length(which(Bank_Data$job=="blue-collar"))/(length(Bank_Data$job)-length(which(Bank_Data$job=="unknown")))
apriori_job_management <- length(which(Bank_Data$job=="management"))/(length(Bank_Data$job)-length(which(Bank_Data$job=="unknown")))
apriori_job_technician <- length(which(Bank_Data$job=="technician"))/(length(Bank_Data$job)-length(which(Bank_Data$job=="unknown")))
apriori_job_unknown <- length(which(Bank_Data$job=="unknown"))/(length(Bank_Data$job))


barplot(prop.table(table(Bank_Data$marital)))
apriori_marital_divorced <- length(which(Bank_Data$marital=="divorced"))/(length(Bank_Data$marital))
apriori_marital_married <- length(which(Bank_Data$marital=="married"))/(length(Bank_Data$marital))
apriori_marital_single <- length(which(Bank_Data$marital=="single"))/(length(Bank_Data$marital))


barplot(prop.table(table(Bank_Data$education)))
apriori_education_primary <- length(which(Bank_Data$education=="primary"))/(length(Bank_Data$education)-length(which(Bank_Data$education=="unknown")))
apriori_education_secondary <- length(which(Bank_Data$education=="secondary"))/(length(Bank_Data$education)-length(which(Bank_Data$education=="unknown")))
apriori_education_tertiary <- length(which(Bank_Data$education=="tertiary"))/(length(Bank_Data$education)-length(which(Bank_Data$education=="unknown")))
apriori_education_unknown <- length(which(Bank_Data$education=="unknown"))/(length(Bank_Data$education))


barplot(prop.table(table(Bank_Data$default)))
apriori_default_yes <- length(which(Bank_Data$default=="yes"))/(length(Bank_Data$default))
apriori_default_no <- length(which(Bank_Data$default=="no"))/(length(Bank_Data$default))

hist(Bank_Data$balance, freq = FALSE)
balance_new <- sqldf("select balance from Bank_Data where balance<15000")
hist(balance_new$balance,breaks = c(-2000,-1500,-1000,-500,0,500,1000,1500,2000,2500,3000,4000,5000,6000,15000))
apriori_balance_under_zero <- as.numeric(sqldf("select count (balance) from Bank_Data where balance <=0" ))/length(Bank_Data$balance)
apriori_balance_0_500 <- as.numeric(sqldf("select count (balance) from Bank_Data where balance between 1 and 500" ))/length(Bank_Data$balance)
apriori_balance_501_1000 <- as.numeric(sqldf("select count (balance) from Bank_Data where balance between 501 and 1000" ))/length(Bank_Data$balance)
apriori_balance_1001_2000 <- as.numeric(sqldf("select count (balance) from Bank_Data where balance between 1001 and 2000" ))/length(Bank_Data$balance)
apriori_balance_2001_4000 <- as.numeric(sqldf("select count (balance) from Bank_Data where balance between 2001 and 4000" ))/length(Bank_Data$balance)
apriori_balance_above_4001 <- as.numeric(sqldf("select count (balance) from Bank_Data where balance >=4001" ))/length(Bank_Data$balance)


barplot(prop.table(table(Bank_Data$housing)))
job_apriori_housing_yes <- length(which(Bank_Data$housing=="yes"))/(length(Bank_Data$housing))
job_apriori_housing_no <- length(which(Bank_Data$housing=="no"))/(length(Bank_Data$housing))


barplot(prop.table(table(Bank_Data$loan)))
job_apriori_loan_yes <- length(which(Bank_Data$loan=="yes"))/(length(Bank_Data$loan))
job_apriori_loan_no <- length(which(Bank_Data$loan=="no"))/(length(Bank_Data$loan))


barplot(prop.table(table(Bank_Data$contact)))
job_apriori_contact_cellular <- length(which(Bank_Data$contact=="cellular"))/(length(Bank_Data$contact)-length(which(Bank_Data$contact=="unknown")))
job_apriori_contact_telephone <- length(which(Bank_Data$contact=="telephone"))/(length(Bank_Data$contact)-length(which(Bank_Data$contact=="unknown")))
job_apriori_contact_unknown <- length(which(Bank_Data$contact=="unknown"))/(length(Bank_Data$contact))

barplot(prop.table(table(Bank_Data$day)))
apriori_day_start <- as.numeric(sqldf("select count (day) from Bank_Data where day <=10" ))/length(Bank_Data$day)
apriori_day_middle <- as.numeric(sqldf("select count (day) from Bank_Data where day between 11 and 20" ))/length(Bank_Data$day)
apriori_day_end <-  as.numeric(sqldf("select count (day) from Bank_Data where day >=21" ))/length(Bank_Data$day)


barplot(prop.table(table(Bank_Data$month))[c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")])
apriori_month_1 <- length(which(Bank_Data$month=="jan"))/(length(Bank_Data$month))
apriori_month_2 <- length(which(Bank_Data$month=="feb"))/(length(Bank_Data$month))
apriori_month_3 <- length(which(Bank_Data$month=="mar"))/(length(Bank_Data$month))
apriori_month_4 <- length(which(Bank_Data$month=="apr"))/(length(Bank_Data$month))
apriori_month_5 <- length(which(Bank_Data$month=="may"))/(length(Bank_Data$month))
apriori_month_6 <- length(which(Bank_Data$month=="jun"))/(length(Bank_Data$month))
apriori_month_7 <- length(which(Bank_Data$month=="jul"))/(length(Bank_Data$month))
apriori_month_8 <- length(which(Bank_Data$month=="aug"))/(length(Bank_Data$month))
apriori_month_9 <- length(which(Bank_Data$month=="sep"))/(length(Bank_Data$month))
apriori_month_10 <- length(which(Bank_Data$month=="oct"))/(length(Bank_Data$month))
apriori_month_11 <- length(which(Bank_Data$month=="nov"))/(length(Bank_Data$month))
apriori_month_12 <- length(which(Bank_Data$month=="dec"))/(length(Bank_Data$month))


barplot(prop.table(table(Bank_Data$campaign)))
apriori_campaign_1_2 <- as.numeric(sqldf("select count (campaign) from Bank_Data where campaign <=2" ))/length(Bank_Data$campaign)
apriori_campaign_3_5 <- as.numeric(sqldf("select count (campaign) from Bank_Data where campaign between 3 and 5" ))/length(Bank_Data$campaign)
apriori_campaign_6_8 <- as.numeric(sqldf("select count (campaign) from Bank_Data where campaign between 6 and 8" ))/length(Bank_Data$campaign)
apriori_campaign_above_9 <- as.numeric(sqldf("select count (campaign) from Bank_Data where campaign >=9" ))/length(Bank_Data$campaign)


hist(Bank_Data$pdays, freq = FALSE)
apriori_pdays_minus_one <- as.numeric(sqldf("select count (pdays) from Bank_Data where pdays <=-1" ))/length(Bank_Data$pdays)
apriori_pdays_0_200 <- as.numeric(sqldf("select count (pdays) from Bank_Data where pdays between 0 and 200" ))/length(Bank_Data$pdays)
apriori_pdays_201_400 <- as.numeric(sqldf("select count (pdays) from Bank_Data where pdays between 201 and 400" ))/length(Bank_Data$pdays)
apriori_pdays_above_401 <- as.numeric(sqldf("select count (pdays) from Bank_Data where pdays >=401" ))/length(Bank_Data$pdays)



barplot(prop.table(table(Bank_Data$previous)))
apriori_previous_0 <- as.numeric(sqldf("select count (previous) from Bank_Data where previous <=0" ))/length(Bank_Data$previous)
apriori_previous_1_2 <- as.numeric(sqldf("select count (previous) from Bank_Data where previous between 1 and 2" ))/length(Bank_Data$previous)
apriori_previous_3_5 <- as.numeric(sqldf("select count (previous) from Bank_Data where previous between 3 and 5" ))/length(Bank_Data$previous)
apriori_previous_above_6 <- as.numeric(sqldf("select count (previous) from Bank_Data where previous >=6" ))/length(Bank_Data$previous)


barplot(prop.table(table(Bank_Data$poutcome)))
apriori_poutcome_failure <- length(which(Bank_Data$poutcome=="failure"))/(length(Bank_Data$poutcome)-length(which(Bank_Data$poutcome=="unknown")))
apriori_poutcome_other <- length(which(Bank_Data$poutcome=="other"))/(length(Bank_Data$poutcome)-length(which(Bank_Data$poutcome=="unknown")))
apriori_poutcome_success <- length(which(Bank_Data$poutcome=="success"))/(length(Bank_Data$poutcome)-length(which(Bank_Data$poutcome=="unknown")))
apriori_poutcome_unknown <- length(which(Bank_Data$poutcome=="unknown"))/(length(Bank_Data$poutcome))


barplot(prop.table(table(Bank_Data$Gender)))
apriori_gender_male <- length(which(Bank_Data$Gender=="0"))/(length(Bank_Data$Gender)-length(which(Bank_Data$Gender=="Unknown")))
apriori_gender_female <- length(which(Bank_Data$Gender=="1"))/(length(Bank_Data$Gender)-length(which(Bank_Data$Gender=="Unknown")))
apriori_gender_unknown <- length(which(Bank_Data$Gender=="Unknown"))/(length(Bank_Data$Gender))

#------------------------------------------------------------------------------
# קשרים בין משתנים

#יצירת מוסבר עם "כן" ו"לא" במקום 0 ו-1
Bank_Data_yes_no <- Bank_Data
Bank_Data_yes_no$y[Bank_Data_yes_no$y == "1"] <- "yes"
Bank_Data_yes_no$y[Bank_Data_yes_no$y == "0"] <- "no"
Bank_Data_yes_no$y <- as.factor(Bank_Data_yes_no$y)
  
# קשר בין השכלה למשכנתא
education_no_unknown <- sqldf("select education from Bank_Data where education !='unknown'")
housung_no_education_unknown <- sqldf("select housing from Bank_Data where education !='unknown'")
plot(education_no_unknown$education,housung_no_education_unknown$housing,xlab="Education",ylab="Housing")

# קשר בין עבודה ללקיחת הלואה
job_no_unknown <- sqldf("select job from Bank_Data where job !='unknown'")
loan_no_job_unknown <- sqldf("select loan from Bank_Data where job !='unknown'")
plot(job_no_unknown$job,loan_no_job_unknown$loan, xlab="Job", ylab="Loan")

# קשר בין השכלה ליתרת חשבון
balance_no_education_unknown <- sqldf("select balance from Bank_Data where education !='unknown'")
boxplot(balance_no_education_unknown$balance~education_no_unknown$education,ylab="Balance")

# היטמאפ בין משתנים מספריים והמוסבר
Numeric_Bank_Data <- cbind(Bank_Data$age,Bank_Data$balance,Bank_Data$day,Bank_Data$campaign,Bank_Data$pdays, Bank_Data$previous,Bank_Data$y)
colnames(Numeric_Bank_Data) <- c("age","balance","day","campaign","pdays","previous","y")
cormat <- round(cor(Numeric_Bank_Data),2)
melted_cormat <- melt(cormat)
print(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#קשר בין השכלה למוסבר
y_no_education_unknown <- sqldf("select y from Bank_Data_yes_no where education !='unknown'")
plot(education_no_unknown$education, y_no_education_unknown$y,xlab="Education", ylab="Y" )
chisq.test(Bank_Data$education,Bank_Data$y)

#קשר בין לקיחת הלוואה למוסבר
plot(Bank_Data$loan,Bank_Data_yes_no$y, xlab="Loan",ylab="Y")
chisq.test(Bank_Data$loan,Bank_Data_yes_no$y)

#קשר בין משכנתא למוסבר
plot(Bank_Data$housing,Bank_Data_yes_no$y, xlab="Housing",ylab="Y")
chisq.test(Bank_Data$housing,Bank_Data_yes_no$y)

#קשר בין אופן יצירת קשר למוסבר
contact_no_unknown <- sqldf("select contact from Bank_Data where contact !='unknown'")
y_no_contact_unknown <- sqldf("select y from Bank_Data_yes_no where contact !='unknown'")
plot(contact_no_unknown$contact,y_no_contact_unknown$y, xlab="Contact",ylab="Y")

#קשר בין גיל למוסבר
Bank_Data_yes_no$age[Bank_Data_yes_no$age>=56] <- "E"
Bank_Data_yes_no$age[Bank_Data_yes_no$age<=25] <- "A"
Bank_Data_yes_no$age[Bank_Data_yes_no$age<=30] <- "B"
Bank_Data_yes_no$age[Bank_Data_yes_no$age<=40] <- "C"
Bank_Data_yes_no$age[Bank_Data_yes_no$age<=55] <- "D"
Bank_Data_yes_no$age <- as.factor(Bank_Data_yes_no$age)
plot(Bank_Data_yes_no$age,Bank_Data_yes_no$y, xlab="Age",ylab="Y")
# כמשתנה רציף
ageY <- sqldf("select age, count(age) as count, sum(y) as sumY, y from Bank_Data group by age")
newAgeY <- data.frame(groupedAge=ageY$age, prob=(ageY$sumY/ageY$count))
plot(x=newAgeY$groupedAge,y=newAgeY$prob, xlab = "Age", ylab= "Probability to accept")
lines(lowess(x=newAgeY$groupedAge,y=newAgeY$prob),lwd=3,col="purple")

#קשר בין עבודה למוסבר
y_no_job_unknown <- sqldf("select y from Bank_Data_yes_no where job !='unknown'")
plot(job_no_unknown$job,y_no_job_unknown$y,xlab="Job", ylab="Y")

#קשר בין מצב משפחתי למוסבר
plot(Bank_Data$marital,Bank_Data_yes_no$y, xlab="Marital", ylab="Y")

#קשר בין יתרת חשבון למוסבר
Bank_Data_yes_no$grouped_balance <- findInterval(Bank_Data_yes_no$balance,c(-2000,0,500,1000,2000,4000,40000))
Bank_Data_yes_no$grouped_balance <- as.factor(Bank_Data_yes_no$grouped_balance)
plot(Bank_Data_yes_no$grouped_balance,Bank_Data_yes_no$y, xlab="Grouped Balance",ylab="Y")

#קשר בין יום למוסבר
Bank_Data_yes_no$grouped_day <- findInterval(Bank_Data_yes_no$day,c(0,11,21,32))
Bank_Data_yes_no$grouped_day <- as.factor(Bank_Data_yes_no$grouped_day)
plot(Bank_Data_yes_no$grouped_day,Bank_Data_yes_no$y, xlab="Grouped Day",ylab="Y")

#קשר בין חודש למוסבר

Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="jan"] <- 1
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="feb"] <- 2
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="mar"] <- 3
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="apr"] <- 4
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="may"] <- 5
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="jun"] <- 6
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="jul"] <- 7
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="aug"] <- 8
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="sep"] <- 9
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="oct"] <- 10
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="nov"] <- 11
Bank_Data_yes_no$month_num[Bank_Data_yes_no$month=="dec"] <- 12
Bank_Data_yes_no$month_num <- as.factor(Bank_Data_yes_no$month_num)
plot(Bank_Data_yes_no$month_num,Bank_Data_yes_no$y,xlab = "Month", ylab = "Y")
Bank_Data_yes_no$month_num <- findInterval(Bank_Data_yes_no$month_num,c(0,4,7,10,13))
Bank_Data_yes_no$month_num <- as.factor(Bank_Data_yes_no$month_num)
plot(Bank_Data_yes_no$month_num,Bank_Data_yes_no$y,xlab="Quarters",ylab="Y")

#קשר בין קמפיין למוסבר
#כמשתנה רציף
campaignY <- sqldf("select campaign, count(campaign) as count, sum(y) as sumY, y from Bank_Data group by campaign")
newcampaignY <- data.frame(groupedcampaign=campaignY$campaign, prob=(campaignY$sumY/campaignY$count))
plot(x=newcampaignY$groupedcampaign,y=newcampaignY$prob, xlab = "campaign", ylab= "Probability to accept")
lines(lowess(x=newcampaignY$groupedcampaign,y=newcampaignY$prob),lwd=3,col="purple")


#קשר בין מספר הימים שעברו מהפעם האחרונה שנוצר קשר למוסבר
#כמשתנה רציף
pdaysY <- sqldf("select pdays, count(pdays) as count, sum(y) as sumY, y from Bank_Data group by pdays")
newpdaysY <- data.frame(groupedpdays=pdaysY$pdays, prob=(pdaysY$sumY/pdaysY$count))
plot(x=newpdaysY$groupedpdays,y=newpdaysY$prob, xlab = "pdays", ylab= "Probability to accept")
lines(lowess(x=newpdaysY$groupedpdays,y=newpdaysY$prob),lwd=3,col="purple")

#קשר בין מספר הפעמים שיצרו קשר עם הלקוח לפני הקמפיין הנוכחי
#כמשתנה רציף
previousY <- sqldf("select previous, count(previous) as count, sum(y) as sumY, y from Bank_Data group by previous")
newpreviousY <- data.frame(groupedprevious=previousY$previous, prob=(previousY$sumY/previousY$count))
plot(x=newpreviousY$groupedprevious,y=newpreviousY$prob, xlab = "previous", ylab= "Probability to accept")
lines(lowess(x=newpreviousY$groupedprevious,y=newpreviousY$prob),lwd=3,col="purple")

#קשר בין תוצאות הקמפיין הקודם למוסבר
poutcome_no_unknown <- sqldf("select poutcome from Bank_Data where poutcome !='unknown'")
y_no_poutcome_unknown <- sqldf("select y from Bank_Data_yes_no where poutcome !='unknown'")
plot(poutcome_no_unknown$poutcome,y_no_poutcome_unknown$y, xlab="poutcome",ylab="Y")

#קשר בין מין למוסבר
gender_no_unknown <- sqldf("select gender from Bank_Data where gender !='Unknown'")
y_no_gender_unknown <- sqldf("select y from Bank_Data_yes_no where gender !='Unknown'")
plot(gender_no_unknown$Gender,y_no_gender_unknown$y, xlab="Gender", ylab="Y")
Bank_Data_yes_no$Gender <- as.factor(Bank_Data_yes_no$Gender)



#--------------------------------------------------------------------------------



#סיכום הנתונים
print(summary(Bank_Data))


#דיסקרטיזציה של קמפיין והקשר עם המוסבר
Bank_Data_yes_no$grouped_campaign <- findInterval(Bank_Data_yes_no$campaign,c(0,3,5,7,60))
Bank_Data_yes_no$grouped_campaign <- as.factor(Bank_Data_yes_no$grouped_campaign)
plot(Bank_Data_yes_no$grouped_campaign, Bank_Data_yes_no$y, xlab="Grouped Campaign", ylab="Y")


#דיסקרטיזציה של פריובויס והקשר עם המוסבר
Bank_Data_yes_no$grouped_previous <- findInterval(Bank_Data_yes_no$previous,c(0,1,6,40))
Bank_Data_yes_no$grouped_previous <- as.factor(Bank_Data_yes_no$grouped_previous)
plot(Bank_Data_yes_no$grouped_previous, Bank_Data_yes_no$y, xlab="Grouped previous", ylab="Y")


#----------------------------------------------------------------------------

#הכנת הנתונים לקראת השלמת החסרים
drops <- c("default","contact","Gender")
Data_After_Cleaning_Features <- Bank_Data[ , !(names(Bank_Data) %in% drops)]
Data_After_Cleaning_Features[Data_After_Cleaning_Features=="unknown"] <- NA

temp_Data_Without_Missing_Values <- mice(data=Data_After_Cleaning_Features, m=5, method="pmm", maxit=50, seed=50)
Data_Without_Missing_Values <- complete(temp_Data_Without_Missing_Values)

#ביצוע דיסקריטיזציה לנתונים הרלוונטיים
Discrete_Data_Without_Missing_Values <- Data_Without_Missing_Values
Discrete_Data_Without_Missing_Values$balance <- findInterval(Discrete_Data_Without_Missing_Values$balance,c(-2000,0,500,1000,2000,4000,40000))
Discrete_Data_Without_Missing_Values$age <- findInterval(Discrete_Data_Without_Missing_Values$age,c(0,25,31,41,56,100))
Discrete_Data_Without_Missing_Values$campaign <- findInterval(Discrete_Data_Without_Missing_Values$campaign,c(0,3,5,7,60))
