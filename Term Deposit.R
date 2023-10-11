### Set working directory ##
setwd("/Volumes/GoogleDrive/My Drive/Documents/MGT_7177/ST-Assignment 2")


### load in libraries
library(tidyverse)
library(readxl) 
library(psych)
library(gridExtra)
library(factoextra)
library(dplyr)
library(vtable)
library(car)
library(caret)
library(lmtest)


### load in data
musei1 <- read_excel("banksv.xlsx")



### UNDERSTANDING BANK SAVINGS DATASET


### Understanding data, looking for outliers, missing values
glimpse(musei1)

### looking at the first 10 rows of the dataset as well as the last 7 rows of the observations
head(musei1, 10)
tail(musei1, 7)
names(musei1)  ### observing variables in the banksv dataset

### summarise data
summary(musei1)


## understanding the selected variables for the analyses 
SH1 <- c('poutcome','month','pdays', 'contact', 'previous', 'default','job', 'day_of_week','cons.price.idx','cons.conf.idx','euribor3m','campaign','age')
summary(musei1[SH1])

## Checking summary statistics of the unclean selected variables above
describe(musei1[SH1])




### DATA QUALITY AND FORMATTING ISSUES  ##

### Checking for outliers in Age using ggplot  ###

histo1<- ggplot(musei1) +
  geom_histogram(aes(age), bins = 50) +
  labs(title = "Age Outlier  Fig. 1       -  Histogram", x= "Age(years)")

boxplt1<- ggplot(musei1) +
  geom_boxplot(aes(age))+
  labs(title = "Age Outlier  Fig. 2       -  Boxplot", x= "Age(years)")

### combining the above visualisation histo1 and boxplt1
grid.arrange(histo1, boxplt1)

### subset to remove outliers in Sale.Price  ###
histo2 <- ggplot(musei1[musei1$age < 100,]) +
  geom_histogram(aes(age), bins = 50) +
  labs(title = "Age_Clean  Fig. 3           -    Histogram", x= "Age(years)")

boxplt2 <- ggplot(musei1[musei1$age< 100,]) +
  geom_boxplot(aes(age)) +
  labs(title = "Age_Clean  Fig. 4           -    Boxplot", x= "Age(years)")

### combining the above visualisation histo2 and boxplt2
grid.arrange(histo2, boxplt2)

### Assigning age outlier as NA
musei1$age[musei1$age > 100] <- NA

### summarise Age
summary(musei1$age) ### 2 NAs observed

### Remove 2 NAs from age outlier and replace with mean value
musei1$age[is.na(musei1$age)] <- mean(musei1$age, na.rm=TRUE)
summary(musei1$age)

### Checking for DQ issues in pdays  ###
summary(musei1$pdays) ### 40 NAs observed


### Remove 40 NAs from pdays variable and replace with mean value
musei1$pdays[is.na(musei1$pdays)] <- mean(musei1$pdays, na.rm=TRUE)
summary(musei1$pdays)


###Converting character variables to as.factor in preparation for analyses
musei1 <- musei1 %>% mutate_if(is.character, as.factor)

### Checking for DQ issues in defaults  ###
summary(musei1$default) ### error in categorical variable

### Convert n to no
musei1$default[musei1$default == "n"] <- "no"
summary(musei1$default) ### n remains with a value of 0

### Remove n with a value of zero using droplevel function
musei1$default<- droplevels(musei1$default)
summary(musei1$default) ### error tectified in default variable


### Checking for DQ issues in month  ###
summary(musei1$month) ### error in categorical variable

### Convert march to mar
musei1$month[musei1$month == "march"] <- "mar"
summary(musei1$month) ### march remains with a value of 0

### Remove march with a value of zero using droplevel function
musei1$month<- droplevels(musei1$month)
summary(musei1$month) ### error rectified in default variable


### Summary of data after fixing data quality issues

### summarise data
summary(musei1)


## checking out the descriptive statistics of the selected variables after cleaning
SH1 <- c('poutcome','month','pdays', 'contact', 'previous', 'default','job', 'day_of_week','cons.price.idx','cons.conf.idx','euribor3m','campaign','age')
summary(musei1[SH1])

## Creating summary statistics of the clean selected variables above
describe(musei1[SH1])




## HYPOTHESES ##

## h1 Poutcome is positively related to subscription
## h2 Month is positively related to subscription 
## h3 Pdays is positively related to Subscription
## h4 Contact is positively related to Subscription
## h5 Previous is positively related to Subscription



## VISUALISATION ##


## subscribed by duration using ggplot2
ggplot(musei1, mapping=aes(x=poutcome)) +
  geom_bar() +
  labs(title="Subscribed by Poutcome",x="Poutcome (feature)", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x= poutcome,
           fill = as.factor(subscribed))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 40,hjust = 1)) +
  labs(title="Subscribed by Poutcome",x="Poutcome (feature)")


## subscribed by Month using ggplot2
ggplot(musei1, mapping=aes(x=month)) +
  geom_bar(bins=50) +
  labs(title="Subscribed by Month",x="Month(seconds)", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x= month,
           fill = as.factor(subscribed))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 40,hjust = 1)) +
  labs(title="Subscribed by Month",x="Month (seconds)")


## subscribed by contact using ggplot2
ggplot(musei1, mapping=aes(x=contact)) +
  geom_bar(bins=100) +
  labs(title="Subscribed by Contact",x="Contact(feature)", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x= contact,
           fill = as.factor(subscribed))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 40,hjust = 1)) +
  labs(title="Subscribed by Contact",x="Contact (feature)")



## subscribed by previous using ggplot2
ggplot(musei1, mapping=aes(x=previous)) +
  geom_bar(bins=20) +
  labs(title="Subscribed by Previous",x="Previous (No. of contacts)", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x= previous,
           fill = as.factor(subscribed))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 360,hjust = 1)) +
  labs(title="Subscribed by Previous",x="Previous (No. of contacts)")



## subscribed by default using ggplot2
ggplot(musei1, mapping=aes(x=default)) +
  geom_bar(bins=50) +
  labs(title="Subscribed by Default",x="Default", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x= default,
           fill = as.factor(subscribed))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 40,hjust = 1)) +
  labs(title="Subscribed by Default",x="Default")


## subscribed by job using ggplot2
ggplot(musei1, mapping=aes(x=job)) +
  geom_bar(bins=50) +
  labs(title="Subscribed by Job",x="Job", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x=job,
           fill = as.factor(subscribed))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 40,hjust = 1)) +
  labs(title="Subscribed by Job",x="Job")



## subscribed by campaign using ggplot2
ggplot(musei1, mapping=aes(x=campaign)) +
  geom_bar(bins=50) +
  labs(title="Subscribed by Campaign",x="Campaign(No. of contacts)", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x= campaign,
           fill = as.factor(subscribed))) +
  geom_histogram(position = "stack") +
  theme(axis.text.x = element_text(angle = 360,hjust = 1)) +
  labs(title="Subscribed by campaign",x="Campaign (No. of contacts)")



## subscribed by cons.price.idx using ggplot2
ggplot(musei1, mapping=aes(x=cons.price.idx)) +
  geom_histogram(bins=20) +
  labs(title="Subscribed by Consumer Price Index",x="Consumer Price Index(%)", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x=cons.price.idx,
           fill = as.factor(subscribed))) +
  geom_histogram(position = "stack") +
  theme(axis.text.x = element_text(angle = 360,hjust = 1)) +
  labs(title="Subscribed by CPI",x="Consumer Price Index(%)")




## subscribed by age using ggplot2
ggplot(musei1, mapping=aes(x=age)) +
  geom_histogram(bins= 40) +
  labs(title="Subscribed by Age",x="Age(Years)", y="Subscribed (no/yes)") +
  facet_wrap(~subscribed)

ggplot(musei1,
       aes(x=age,
           fill = as.factor(subscribed))) +
  geom_histogram(position = "stack") +
  theme(axis.text.x = element_text(angle = 360,hjust = 1)) +
  labs(title="Subscribed by Age",x="Age(years)")




## MEASURES OF ASSOCIATION (MA)

## subscribed and poutcome
chisq.test(musei1$poutcome, musei1$subscribed)

## subscribed and month
chisq.test(musei1$month, musei1$subscribed)

## subscribed and pdays
t.test(musei1$pdays, as.numeric(musei1$subscribed))

## subscribed and previous
t.test(musei1$previous, as.numeric(musei1$subscribed))

## subscribed and default
chisq.test(musei1$default, musei1$subscribed)

## subscribed and job
chisq.test(musei1$job, musei1$subscribed)

## subscribed and day of week
chisq.test(musei1$day_of_week, musei1$subscribed) 

## subscribed and cons.price.idx
t.test(musei1$cons.price.idx, as.numeric(musei1$subscribed))

## subscribed and cons.conf.idx
t.test(musei1$cons.conf.idx, as.numeric(musei1$subscribed))

## subscribed and euribor3m
t.test(musei1$euribor3m, as.numeric(musei1$subscribed))

## subscribed and contact
chisq.test(musei1$contact, musei1$subscribed)

## subscribed and age
chisq.test(musei1$age, as.numeric(musei1$subscribed))



## MULTIPLE LOGISTIC REGRESSION

### Check for missing values on selected data to avoid NA during check for model accuracy on test data (20%)
musei1 <- musei1[!is.na(musei1$poutcome),]
musei1 <- musei1[!is.na(musei1$month),]
musei1 <- musei1[!is.na(musei1$pdays),]
musei1 <- musei1[!is.na(musei1$contact),]
musei1 <- musei1[!is.na(musei1$previous),]
musei1 <- musei1[!is.na(musei1$default),]
musei1 <- musei1[!is.na(musei1$job),]
musei1 <- musei1[!is.na(musei1$day_of_week),]
musei1 <- musei1[!is.na(musei1$cons.price.idx),]
musei1 <- musei1[!is.na(musei1$cons.conf.idx),]
musei1 <- musei1[!is.na(musei1$euribor3m),]
musei1 <- musei1[!is.na(musei1$campaign),]

### Set seed to keep values of regression constant
set.seed(1846)
index <- createDataPartition(musei1$subscribed, times =1, p =0.8, list= FALSE)

train <- musei1[index,]
test  <- musei1[-index,]



### model A
formula <- subscribed ~ poutcome + month + pdays + contact + previous ### Setting the formula
modelA <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelA) 


### model B
formula <- subscribed ~ poutcome + month + pdays + contact + previous + default ### Setting the formula
modelB <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelB) 


### model C
formula <- subscribed ~ poutcome + month + pdays + contact + previous + default + job ### Setting the formula
modelC <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelC) 


### model D
formula <- subscribed ~ poutcome + month + pdays + contact + previous + default + job + day_of_week### Setting the formula
modelD <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelD) 


### model E
formula <- subscribed ~ poutcome + month + pdays + contact + previous + default + job + day_of_week + cons.price.idx ### Setting the formula
modelE <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelE) 


### model F
formula <- subscribed ~ poutcome + month + pdays + contact + previous + default + job + day_of_week + cons.price.idx + cons.conf.idx ### Setting the formula
modelF <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelF) 


### model G
formula <- subscribed ~ poutcome + month + pdays + contact + previous + default + job + day_of_week + cons.price.idx + cons.conf.idx + euribor3m ### Setting the formula
modelG <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelG) 


### model H
formula <- subscribed ~ poutcome + month + pdays+ contact + previous + default + job + day_of_week + cons.price.idx + cons.conf.idx + euribor3m + campaign ### Setting the formula
modelH <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelH) 



### model I
formula <- subscribed ~ poutcome + month + pdays + contact + previous + default + job + day_of_week + cons.price.idx + cons.conf.idx + euribor3m + campaign + age### Setting the formula
modelI <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelI) ### This model is dropped because it doesnt improve the previous model(modelH)



### Checks for model accuracies

### check modelA accuracy of the test data (20%)
predictions <- predict(modelA, test, type = "response")
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)

### check modelB accuracy of the test data (20%)
predictions <- predict(modelB, test, type = "response")
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)

### check modelC accuracy of the test data (20%)
predictions <- predict(modelC, test, type = "response")
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)

### check modelD accuracy of the test data (20%)
predictions <- predict(modelD, test, type = "response")
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)

### check modelE accuracy of the test data (20%)
predictions <- predict(modelE, test, type = "response")
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)

### check modelF accuracy of the test data (20%)
predictions <- predict(modelF, test, type = "response")
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)

### check modelG accuracy of the test data (20%)
predictions <- predict(modelG, test, type = "response")
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)


### check modelH accuracy of the test data (20%)
predictions <- predict(modelH, test, type = "response")
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)


### model accuracy using confusion matrix
confusionMatrix(data = class_pred, test$subscribed)

### View the odds ratio for ModelH
exp(modelH$coefficients)


#check the R squared value on the training data

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")   #Function source: Field et al., 2012
}



### run logisticPseudoR2s for all models (A-H)
logisticPseudoR2s(modelA)
logisticPseudoR2s(modelB)
logisticPseudoR2s(modelC)
logisticPseudoR2s(modelD)
logisticPseudoR2s(modelE)
logisticPseudoR2s(modelF)
logisticPseudoR2s(modelG)
logisticPseudoR2s(modelH)



### ASSUMPTION CHECKING AND THINGS THAT COULD GO WRONG

### Evaluate the model assumption
### Add the predicted probability to the dataframe using the fitted() function
train$predictedProbabilities <- fitted(modelH)

### checking probability of subscribed, and the actual outcome
head(data.frame(train$predictedProbabilities, train$subscribed))
tail(data.frame(train$predictedProbabilities, train$subscribed))

### Analysing the Residuals using the standardised residuals to check the model fit.
### As a rule of thumb only 5% should lie outside of ± 1.96 
### and about 1% should lie outside of ± 2.58. Cases above 3 are a cause for concern.
train$standardisedResiduals <- rstandard(modelH) ### Also known as the errors in unit standard deviations

### counting how many of the standardised residuals is/are above 1.96. Only 5% above is acceptable
sum(train$standardisedResiduals > 1.96) ### 1464 data is > 1.96 which is < 5% of 32923 train data = 1646.15, thus satisfies assumption

### summarise standardisedResiduals to check for values above 3
summary(train$standardisedResiduals) ### No value above 3.0, thus satisfies assumption

### Examining Influential Cases using cooks distance
train$cook <- cooks.distance(modelH)

### check for cooks distance greater that 1
sum(train$cook > 1) ### assumption satisfied since none is greater than 1

### Checking for Multicolinearity having in mind value above 10 is not good.
vif(modelH) ### variables poutcome and pdays have multicolinearity issues since they are > 10
### each variable was removed at different intervals to see if modelH will impprove
### None of the variables improved modelH, each having a VIC of 18372 and 18368 respectively
### greater than  ModelH whose VIC is 18345(when both variables are kept in the model)
###  see Appendix 5




### Testing for the Linearity of the Logit

### There are 4 continous variables in ModelH and checking that they are linearly related 
### to the log of the outcome variable (subscribed). 
### Run the logistic regression model including predictors that are the interaction 
### between each predictor and the log of itself. 
### This test is essential in order to know how the model performs in a business environment. It performs
### better than the accuracy in the residuals training dataset which is not reliable on a data it hasnt seen before

### Create the interaction terms of the variable(numeric) with its log.
train$cpiLogInt <- log(train$cons.price.idx)*train$cons.price.idx
train$eb3LogInt <- log(train$euribor3m)*train$euribor3m
train$camLogInt <- log(train$campaign)*train$campaign
train$pdyLogInt <- log(train$pdays)*train$pdays



### build formula
formula<- subscribed ~  poutcome + month + pdays+ contact + previous + default + job + day_of_week + cons.price.idx + cons.conf.idx + euribor3m + campaign + cpiLogInt + eb3LogInt + camLogInt + pdyLogInt  ### Setting the formula
modelLogInt <- glm(formula, data = train, family = "binomial")

### summarise the model
summary(modelLogInt) 

### Independent residuals : run dwtest . value between 1.5 and 2.5 is good
dwtest(modelH) ### DW = 1.9 This falls in between 1.5 and 2.5
