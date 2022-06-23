

library(pacman)
p_load(ggthemes, tidyverse, Amelia, corrplot, caTools)
#
###########################################
#input relevant data into Rscript
ccmr = read.csv("CurrentCustomerMktgResults.csv")
had = read.csv("householdAxiomData.csv")
hcd = read.csv("householdCreditData.csv")
hvd = read.csv("householdVehicleData.csv")
str(ccmr)
str(had)
str(hcd)
str(hvd)
############################################
############################################
#converted the dependent variable into a factor 
ccmr$Y_AcceptedOffer = as.factor(ccmr$Y_AcceptedOffer)
levels(ccmr$Y_AcceptedOffer)
ccmr$Accepted_offer = ifelse(ccmr$Y_AcceptedOffer == "Accepted", 1,0)
############################################
############################################
##Converted time character to minute format and produced call length variable
library(data.table)
ccmr$CallStart = as.ITime(ccmr$CallStart)
ccmr$CallEnd = as.ITime(ccmr$CallEnd)
ccmr$callLength = ccmr$CallEnd - ccmr$CallStart

################################################
###merged relevant data from different files together, after selecting relevant data
had1 = had %>% select(HHuniqueID, Age, Marital, Job, headOfhouseholdGender, Education)
hcd1 = hcd %>% select(HHuniqueID,RecentBalance, HHInsurance, CarLoan)

Data_full = merge(ccmr, had1, by ="HHuniqueID")
Data_full = merge(Data_full, hcd1, by ="HHuniqueID")
#remove redundancies
Data_full = select(Data_full, -Y_AcceptedOffer, -Communication)

##################################################
#checked for missing values; there are a lot of packages to do this, I sometimes use 
#Amelia when dealing with a small data size
library(Amelia)
missmap(Data_full, main="National City Bank - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

###############################################
#replace missing values with other
library(tidyr)
Data_full = Data_full %>% replace_na(list(Education = "other", Job ="other"))

#Performed some EDA for presentation analysis to the Bank's chief product officer
ggplot(Data_full,aes(Accepted_offer)) + geom_bar()

#Explore period of month where acceptance rate was greater####
pb1 =  ggplot(Data_full, aes(LastContactDay)) + geom_bar(aes(fill = factor(Accepted_offer))) + 
  theme_dark() + scale_x_continuous(breaks = seq (min(0), max(30), by = 5))  + 
  scale_fill_discrete(name = "Accepted Offer", labels = c("Rejected", " Accepted"))
pb1

#compared accepted offers along with age variables and recent balances
pl = ggplot(Data_full,aes(Age, RecentBalance)) + geom_point(aes(color=factor(Accepted_offer))) + 
  scale_y_continuous(breaks = seq(min(0), max(100000), by = 7000)) + scale_x_continuous(
    breaks = seq (min(0), max(100), by = 10)) + theme_economist(base_size = 10) 
pl +  scale_color_discrete(name = "Accepted Offer", labels = c("Rejected", " Accepted"))


###########################
###Lastcontactmonth
Data_full$newaccepted_offer = as.factor(Data_full$Accepted_offer)
pl2 = ggplot(Data_full, aes(LastContactMonth)) + geom_bar(aes(fill = newaccepted_offer))
pl2 +  scale_fill_discrete(name = "Accepted Offer", labels = c("Rejected", " Accepted"))

######################################################################
###################################################Household insurance
pl3 = ggplot(Data_full, aes(HHInsurance)) + geom_bar(aes(fill = newaccepted_offer)) + theme_classic()
pl3 +  scale_fill_discrete(name = "Accepted Offer", labels = c("Rejected", " Accepted"))

###############################################################
##############################################CarLoans
pl4 = ggplot(Data_full, aes(CarLoan)) + geom_bar(aes(fill = newaccepted_offer)) + theme_tufte()
pl4 +  scale_fill_discrete(name = "Accepted Offer", labels = c("Rejected", " Accepted"))

#############################################################
#######################################Marital_Status
pl5 = ggplot(Data_full, aes(Marital)) + geom_bar(aes(fill = newaccepted_offer)) + theme_classic()
pl5 +  scale_fill_discrete(name = "Accepted Offer", labels = c("Rejected", " Accepted"))

##################################################
##########################################Education
pl6 = ggplot(Data_full, aes(Education)) + geom_bar(aes(fill = newaccepted_offer)) + theme_base()
pl6 +  scale_fill_discrete(name = "Accepted Offer", labels = c("Rejected", " Accepted"))

###################################################&&&&
#Attempted simple correlation between numerical variables
data_corr = sapply(Data_full, is.numeric)
cor.data = cor(Data_full[,data_corr])
print(cor.data)

#################################################
#################################################
library(corrplot)
corrplot(cor.data,method='color')

#removed redundant variables
Data_full = select(Data_full, -HHuniqueID, -past_Outcome, -dataID, -CallStart, -CallEnd, -Accepted_offer)
str(Data_full)

#######################################
#Converted other variables to factored variables to prepare data for regression
Data_full$Marital = as.factor(Data_full$Marital)
Data_full$Job = as.factor(Data_full$Job)                
Data_full$headOfhouseholdGender = as.factor(Data_full$headOfhouseholdGender)
Data_full$Education = as.factor(Data_full$Education)            


######################################################
#####################################################
#I ran a logistic model on the whole data set at first 
#I did this so I could view the most statistically significant variables for the whole data set
log.model = glm(formula= newaccepted_offer ~ . , family = binomial(link='logit'),data = Data_full)
summary(log.model)

################ I split the data
library(caTools)
set.seed(101)

split = sample.split(Data_full$newaccepted_offer, SplitRatio = 0.70)

final.train = subset(Data_full, split == TRUE)
final.test = subset(Data_full, split == FALSE)
############################################################
###########################################################
#I reran the model on my final training set#
final.log.model = glm(formula=newaccepted_offer ~ . , family = binomial(link='logit'),data = final.train)
summary(final.log.model)
##########################################################
##I proceeded to check my prediction accuracy
############################################################
fitted.probabilities = predict(final.log.model,newdata=final.test,type='response')

###################################################################
#################################################################
##I calculated from my predicted values

fitted.results = ifelse(fitted.probabilities > 0.5,1,0)
misClasificError = mean(fitted.results != final.test$newaccepted_offer)
print(paste('Accuracy',1-misClasificError)) ####When I ran this line of code, accuracy was 80.25%

##################################
#Then I created a confusion matrix to give more information about level of accuracy
table(final.test$newaccepted_offer, fitted.probabilities > 0.5)







