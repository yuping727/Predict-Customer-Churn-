
##### develop machine learning models to predict customer churn 
####  import dataset 
# summary 
summary= read.csv(file.choose(), header=T)
head(summary,50)
dim(summary) # 38921 12 

# transaction 
transaction= read.csv (file.choose(), header=T)
head(transaction)
dim(transaction) # 482826  10

# online
online = read.csv(file.choose(), header=T)
dim(online) # 220511  3

# loan 
loan= read.csv(file.choose(), header=T)
dim(loan) # 228104   5



#### convert variable data types
# in summary dataset, convert factor to numeric data type 
summary$Age = as.numeric(as.character(summary$Age))
# NumberofSavingsProducts
summary$NumberofSavingsProducts = as.numeric(as.character(summary$NumberofSavingsProducts))
table(summary$NumberofSavingsProducts)

# in summary dataset, convert numeric to factor data type 
summary$DBINDICATOR = as.factor(summary$DBINDICATOR)
summary$CCINDICATOR = as.factor(summary$CCINDICATOR)
summary$INDIRECT = as.factor(summary$INDIRECT)

# check the number of missing values 
sapply(summary, function(x) sum(is.na(x)))

# delete rows with NAs 
summary= na.omit(summary)
dim(summary)  # 38827  12 

# in summary dataset, add a new reponse variable "Churn" to distinguish customers for those who have loans and those who don't have loans 
for (i in 1:nrow(summary)) {
  if ((summary$CountofLoans[i] == 0) & (summary$Status[i] == "Closed")) {
    summary$Churn[i] = "Close"
  } else if ((summary$CountofLoans[i] == 0) & (summary$Status[i] == "Open")){
    summary$Churn[i] = "Open"
  } else if ((summary$CountofLoans[i] == 0) & (summary$Status[i] == "Chargeoff")) {
    summary$Churn[i] = "Chargeoff"
  } else if ((summary$CountofLoans[i] > 0) & (summary$Status[i] == "Closed")){
    summary$Churn[i] = "Close_L"
  } else if ((summary$CountofLoans[i] > 0) & (summary$Status[i] == "Open")){
    summary$Churn[i] = "Open_L"
  } else if ((summary$CountofLoans[i] > 0) & (summary$Status[i] == "Chargeoff")){
    summary$Churn[i] = "Chargeoff_L"
  }
}

# convert the data type of 'Churn' from character to factor 
summary$Churn = as.factor(summary$Churn)
summary(summary)
str(summary)


### extract new variables from OpenDate, CloseDate, ChargeOffDate for modeling 
# convert OpenDate, CloseDate, ChargeOffDate to date type 
summary$OpenDate =as.Date(summary$OpenDate, "%m/%d/%y")
summary$CloseDate = as.Date(summary$CloseDate, "%m/%d/%y")
summary$ChargedOffDate = as.Date(summary$ChargedOffDate, "%m/%d/%y")

head(summary,50)

# add a new variable "Length", which is the number of days between (OpenDate and CloseDate) or between (OpenDate and ChargedOffDate) or 
# between (OpenDate and CurrentDate) (Note: CurrentDate is the day when the dataset was extracted from database)
last_date = as.Date('2019/07/31', '%Y/%m/%d')
for (i in 1:nrow(summary)) {
  if (!is.na(summary$ChargedOffDate[i])) {
    summary$Length[i] = summary$ChargedOffDate[i] - summary$OpenDate[i]
  } else if (!is.na(summary$CloseDate[i])){
    summary$Length[i] = summary$CloseDate[i] - summary$OpenDate[i]
  } else 
    summary$Length[i] = last_date - summary$OpenDate[i]
}



## merge summary with transaction, left join 
# first convert MonthEndDate to Date type 
transaction$MonthEndDate= as.Date(transaction$MonthEndDate, "%m/%d/%y")

summary_tran= merge(x =summary, y= transaction, by="Memberid", all.x= TRUE)
dim(summary_tran)  # 481737  23 
head(summary_tran)
sapply(summary_tran, function(x) sum(is.na(x)))    # missing values 245 


# subset the dataset where (ChargedOffDate - MonthEndDate) or (CloseDate - MonthEndDate) or (2019/07/31 - MonthEndDate)<= 365
# first create the new variable 'Gap'
# this code takes a long time to run 
for (i in 1:nrow(summary_tran)) {
  if (!is.na(summary_tran$ChargedOffDate[i])) {
    summary_tran$Gap[i] = summary_tran$ChargedOffDate[i] - summary_tran$MonthEndDate[i]
  } else if (!is.na(summary_tran$CloseDate[i])){
    summary_tran$Gap[i] = summary_tran$CloseDate[i] - summary_tran$MonthEndDate[i]
  } else summary_tran$Gap[i] = last_date - summary_tran$MonthEndDate[i]
}

# then subset the dataset where Gap is less or equal to 365 
summary_tran2= summary_tran[which(summary_tran$Gap <= 365), ]
dim(summary_tran2)    # 296961 24 

# check the number of missing values in each columns 
sapply(summary_tran2, function(x) sum(is.na(x)))



## merge summary_tran with online, left join 
online$MonthEndDate= as.Date(online$MonthEndDate, "%m/%d/%y")
str(online)
summary_tran_online <- merge(x=summary_tran2, y= online,by=c("Memberid","MonthEndDate"), all.x= TRUE)
dim(summary_tran_online)  # 296961 25
sapply(summary_tran_online, function(x) sum(is.na(x)))  # 161779 missing for 'count' variable 
# the percentage of missing value for 'count'
161779/296961  # 0.544782
# replace the missing values of Count to 0 
summary_tran_online$Count[is.na(summary_tran_online$Count)] = 0 

# export summary_tran_online
write.csv(summary_tran_online,file= "summary_tran_online.csv", row.names= FALSE)




#### subset the whole dataset (summary_tran_online) according to Churn
## dataset for customers who do not have loan 
# https://stackoverflow.com/questions/1195826/drop-factor-levels-in-a-subsetted-data-frame
library(dplyr)
summary_tran_online1 <- summary_tran_online %>% filter(summary_tran_online$Churn == "Close" | summary_tran_online$Churn == "Open"
                                                       | summary_tran_online$Churn == "Chargeoff") %>% droplevels()
levels(summary_tran_online1$Churn)
summary(summary_tran_online1)   # Churn    Chargeoff: 11444   Close: 42674    Open:188361
dim(summary_tran_online1) #  242479   25

# export summary_tran_online1, which is the dataset for customers who do not have loans 
write.csv(summary_tran_online1,file= "summary_tran_online1.csv", row.names= FALSE)


## dataset for customers who have loans 
summary_tran_online2 <- summary_tran_online %>% filter(summary_tran_online$Churn == "Close_L" | summary_tran_online$Churn == "Open_L"
                                                       | summary_tran_online$Churn == "Chargeoff_L" )%>% droplevels()
dim(summary_tran_online2)  # 54482  25
summary(summary_tran_online2)  # Churn  Chargeoff_L: 2551    Close_L: 7149     Open_L:44782



########### merge summary_tran_online2 with loan, left join 
#summary_tran_online_loan<- merge(x=summary_tran_online2, y= loan,by=c("Memberid","MonthEndDate"), all.x= TRUE)
#sapply(summary_tran_online_loan, function(x) sum(is.na(x)))  # lots missing values for NumberofPayments, SumofPaymentAmount, LoanCollateralCodeDesc

loan$MonthEndDate= as.Date(loan$MonthEndDate, "%m/%d/%y")
loan_1= loan[-c(5)]  # remove the column "LoanCollateralCodeDesc" 
str(loan_1)  # 228104  4 
loan_2 = loan[-c(2,3,4)]
# unique memberid and its corresponding "LoanCollateralCodeDesc" in loan dataset 
loan_3 = unique(loan_2)
head(loan_3)
# merge summary_tran_online2 with loan_1
temp1 <- merge(x=summary_tran_online2, y= loan_1,by=c("Memberid","MonthEndDate"), all.x= TRUE)
dim(temp1)  # 56626    27
sapply(temp1, function(x) sum(is.na(x)))  #  19832  is missing for NumberofPayments and SumofPaymentAmount

# replace the missing values of NumberofPayments, SumofPaymentAmount to 0 
temp1$NumberofPayments[is.na(temp1$NumberofPayments)] = 0 
temp1$SumofPaymentAmount[is.na(temp1$SumofPaymentAmount)] = 0 
head(temp1)
# merge temp1 with loan_3 to get the last column "LoanCollateralCodeDesc" 
summary_tran_online_loan = merge(x=temp1, y= loan_3, by= "Memberid", all.x = TRUE)



### delete columns that will not be used in the following analysis 
# for customers with loans, we name the dataset ‘loan_customer’
str(summary_tran_online_loan)
loan_customer = summary_tran_online_loan[c(-1, -2,-3,-4,-5,-6)]
str(loan_customer)
dim(loan_customer)  # 64532    22
sapply(loan_customer, function(x) sum(is.na(x)))  # 2968 missing for "LoanCollateralCodeDesc"
loan_customer= na.omit(loan_customer)
dim(loan_customer)  # 61564    22

# for customers do not have loans 
str(summary_tran_online1)
member_customer = summary_tran_online1[c(-1, -2,-3,-4,-5,-6)]
dim(member_customer)  # 242479     19
str(member_customer)
sapply(member_customer, function(x) sum(is.na(x)))  # no missing values 



#### in loan_customer dataset, reduce the levels of factor variable 'LoanCollateralCodeDesc'
table(loan_customer$LoanCollateralCodeDesc)
# define function
LoanCode= function(code){
  code= as.character(code)
  
  if (code== 'Business 1st Mortgage' | code=='Business 2nd Mortgage' | code=='Business Construction Loan' |code=='Business Equipment Loan' |
      code== 'Business Line of Credit' | code== 'Business New Vehicle Loan' | code== 'Business Term Loan' | code=='Business Used Vehicle Loan'){
    return('Business')
  } else if (code =='Indirect-New Auto'| code=='Indirect-New Motorcycle' | code=='Indirect-New Prem MC' | code=='Indirect-Used Auto' |
             code== 'Indirect-Used Motorcycle' | code=='Indirect-Used Prem MC'| code=='LendPro Indirect Used'){
    return('Indirect')
  } else if (code =='LendPro Direct Used' |code=='New Auto' | code=='New Boat' | code=='New Boat' | code=='New Motorcycle'| code=='New RV' | 
             code=='Personal Line of Credit' | code=='Used Auto' | code=='Used Motorcycle'| code=='Used RV'){
    return('Personal')
  }
    else if (code=='Share Certificate' | code=='Share Secured') {
      return('Shared')
    } else {
      return(code)
    }
}
# apply 
loan_customer$LoanCollateralCodeDesc = sapply(loan_customer$LoanCollateralCodeDesc,LoanCode)
# check 
table(loan_customer$LoanCollateralCodeDesc) # now we have 9 levels of LoanCollateralCodeDesc
# change the LoanCollateralCodeDesc back to factor 
loan_customer$LoanCollateralCodeDesc = as.factor(loan_customer$LoanCollateralCodeDesc)
str(loan_customer)

# export loan_customer and member_customer datasets 
write.csv(loan_customer,file= "loan_customer.csv", row.names= FALSE)
write.csv(member_customer,file= "member_customer.csv", row.names= FALSE)


## exploratory data analysis 
# for loan_customer dataset 
# correlation between numeric variables
library(corrplot)
numeric.var =sapply(loan_customer, is.numeric)
corr.matrix = cor(loan_customer[,numeric.var])
corrplot(corr.matrix, main = "correlation plot for numerical variables", type= "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE)
# (NumberofTotalTransactions and NumberofDebitCardTransactions), (SUMofFEESCHARGED and FEESCHARGED), (tenure and length) have high correlations 

# remove NumberofDebitCardTransactions, FEESCHARGED, length 
loan_customer$NumberofDebitCardTransactions = NULL 
loan_customer$FEESCHARGED = NULL
loan_customer$Length = NULL
dim(loan_customer)  # 61564   19

# for member_customer
# correlation between numeric variables
library(corrplot)
numeric.var =sapply(member_customer, is.numeric)
corr.matrix = cor(member_customer[,numeric.var])
corrplot(corr.matrix, main = "correlation plot for numerical variables", type= "upper", tl.pos = "td",
         method = "circle", diag = FALSE)
# (NumberofTotalTransactions and NumberofDebitCardTransactions), (SUMofFEESCHARGED and FEESCHARGED), (tenure and length) have high correlations
member_customer$NumberofDebitCardTransactions = NULL 
member_customer$FEESCHARGED = NULL
member_customer$Length = NULL
dim(member_customer)  #  242479   16



##### barplot for discrete variables 
# https://jtsulliv.github.io/churn-eda/
library(tidyverse)
# DBINDICATOR
# loan_customer without DebitCard are more likely to churn 
ggplot(loan_customer) + geom_bar(aes(x = DBINDICATOR, fill = Churn), position = "dodge")
loan_customer %>% group_by(DBINDICATOR, Churn) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# CCINDICATOR
# loan_customer without Credit Card are more likely to churn
ggplot(loan_customer) + geom_bar(aes(x = CCINDICATOR, fill = Churn), position = "dodge")
loan_customer %>% group_by(CCINDICATOR) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
loan_customer %>% group_by(CCINDICATOR, Churn) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# NumberofSavingsProducts
# loan_customer with less number of Saving Products are more likely to churn 
ggplot(loan_customer) + geom_bar(aes(x = NumberofSavingsProducts, fill = Churn), position = "dodge")
loan_customer %>% group_by(NumberofSavingsProducts, Churn) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
# LoanCollateralCodeDesc
# loan_customer with "Indirect" Loan type are more likely to churn
ggplot(loan_customer) + geom_bar(aes(x = LoanCollateralCodeDesc, fill = Churn), position = "dodge")
print(loan_customer %>% group_by(LoanCollateralCodeDesc, Churn) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)))



###### compare the total fee charges for loan_customer without Debit Card, those without Credit Card, and those with only 1 saving product
# total fee charges for loan_customer without Debit Card  # -17644.07
loan_customer %>% select(DBINDICATOR, Churn, SUMofFEESCHARGED) %>% filter(DBINDICATOR == 0, Churn != "Open_L") %>% 
summarize(n = n(), total = sum(SUMofFEESCHARGED))   

# total fee charges for loan_customer without Credit Card  # -162909
loan_customer %>% select(CCINDICATOR, Churn, SUMofFEESCHARGED) %>% filter(CCINDICATOR == 0, Churn == "Open_L") %>% 
summarize(n = n(), total = sum(SUMofFEESCHARGED))

# total fee charges for loan_customer with only 1 saving product   # -39651.43
loan_customer %>% select(NumberofSavingsProducts, Churn, SUMofFEESCHARGED) %>% filter(NumberofSavingsProducts == 1, Churn == "Open_L") %>% 
summarize(n = n(), total = sum(SUMofFEESCHARGED))

# total fee charges for loan_customer with Indirect Loan   # -78266.1
loan_customer %>% select(LoanCollateralCodeDesc, Churn, SUMofFEESCHARGED) %>% filter(LoanCollateralCodeDesc == 'Indirect', Churn == "Open_L") %>% 
summarize(n = n(), total = sum(SUMofFEESCHARGED))
###### baesd on the results, we should focus our efforts on loan_customer without Credit Card. 



#### histogram for continuous variables 
##### Use ggplot2 to create a histogram of Age, colored by NewStatus
library(ggplot2)
library(dplyr)
# Use ggplot2 to create a histogram of Age colored by Churn 
ggplot(loan_customer,aes(Age)) + geom_histogram(aes(fill=Churn), color='black', binwidth = 1) + theme_bw()
# histogram of Tenure colored by by Churn  
ggplot(loan_customer,aes(Tenure)) + geom_histogram(aes(fill=Churn), color='black', binwidth = 1) + theme_bw()
# NumberofDirectDeposits
ggplot(loan_customer,aes(NumberofDirectDeposits)) + geom_histogram(aes(fill=Churn), color='black', binwidth = 1) + theme_bw()
# NumberofBillPayTransactions
ggplot(loan_customer,aes(NumberofBillPayTransactions)) + geom_histogram(aes(fill=Churn), color='black', binwidth = 1) + theme_bw()
# NumberofTransactionsConductedinBranch
ggplot(loan_customer,aes(NumberofTransactionsConductedinBranch)) + geom_histogram(aes(fill=Churn), color='black', binwidth = 1) + theme_bw()
# Count
ggplot(loan_customer,aes(Count)) + geom_histogram(aes(fill=Churn), color='black', binwidth = 1) + theme_bw()
# NumberofPayments
ggplot(loan_customer,aes(NumberofPayments)) + geom_histogram(aes(fill=Churn), color='black', binwidth = 1) + theme_bw()


####### modeling 
### logistic regression 
#### for loan_customer
# split data into training and testing sets 
# https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4
library(caTools)
set.seed(101)
sample1<- sample.split(loan_customer$Churn,SplitRatio=0.7)
training1<-subset(loan_customer,sample1 == T)
testing1<-subset(loan_customer,sample1 == F)
# confirm the splitting is correct 
dim(training1) # 43095    19
dim(testing1) # 18469    19
# multinomial logistic regression model 
# training the model
library(nnet)
glm.loan <- multinom(Churn ~ .,data=training1)
summary(glm.loan) 
# prediction on the testing dataset
glm.pred= predict(glm.loan, testing1)
#prediction accuracy
mean(glm.pred == testing1$Churn)   #   0.8744924
#confusion matrix
table(predicted=glm.pred, actual= testing1$Churn)













