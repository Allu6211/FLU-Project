vaccinetrain=read.csv(file.choose(),na.strings = c("","NA"))
vaccinetest=read.csv(file.choose(),na.strings = c("","NA"))
vaccinelabel=read.csv(file.choose(),na.strings = c("","NA"))

# EDA - Exploratory Data Analysis
#Seasonal Vaccine 

table(vaccinelabel$seasonal_vaccine) # Balanced data 

#H1N1 Vaccine 

table(vaccinelabel$h1n1_vaccine) # Imbalanced data
# All Varaibles as per problem description are non numeric and categorical - Frequancy table & Cross tabulations 
# Hypothesis test - Chi2 test of independence 
# Do majority people have knowledge  of H1n1 

table(vaccinetrain$h1n1_knowledge) # littke knowledge 

# Howmany litte knowledge took H1n1 

table(vaccinelabel$h1n1_vaccine,vaccinetrain$h1n1_knowledge)
barplot(table(vaccinelabel$h1n1_vaccine,vaccinetrain$h1n1_knowledge),legend=T ,args.legend = (x="topleft"))

#Test null no association between seasonal vaccination & chronic_med_condition 

table(vaccinelabel$seasonal_vaccine,vaccinetrain$chronic_med_condition)
# Null - No association between both var 
# Alt - Association between both var 

chisq.test(vaccinelabel$seasonal_vaccine,vaccinetrain$chronic_med_condition)
# P value 2.2e-16 is less than 0.05 Reject Null hypothesis 

# Test Null no association between health insurance and h1n1 vaccine 

prop.table(table(vaccinelabel$h1n1_vaccine,vaccinetrain$health_insurance
                ))

proportions(table(vaccinelabel$h1n1_vaccine,vaccinetrain$health_insurance))

# prop.table or proportions()- Convert table ti percentages

chisq.test(vaccinetrain$health_insurance,vaccinelabel$h1n1_vaccine)

# DATA PREPROCESSING 

# Step 1 - Missing values - Find NA's

sort(colSums(is.na(vaccinetrain)),decreasing = T)
table(vaccinetrain$employment_occupation)
table(vaccinetrain$employment_industry)
table(vaccinetrain$health_insurance)

chisq.test(vaccinelabel$seasonal_vaccine,vaccinetrain$employment_industry)
chisq.test(vaccinelabel$seasonal_vaccine,vaccinetrain$employment_occupation)

# employment_industry , employment_occupation ,health insurance are masked data though both are significant will impute the data as "missing"

# Combine both train and test for preprocessing

combinedf=rbind(vaccinetrain,vaccinetest) # row wise concatination
combinedf$employment_occupation=ifelse(is.na(combinedf$employment_occupation),"missing",combinedf$employment_occupation)
combinedf$employment_industry=ifelse(is.na(combinedf$employment_industry),"missing",combinedf$employment_industry)
combinedf$health_insurance=ifelse(is.na(combinedf$health_insurance),"missing",combinedf$health_insurance)
sort(colSums(is.na(combinedf)),decreasing = T)

# Convert all independent var into factors or dummy encoding 

combinedfdummy= lapply(combinedf[-1],factor)
combinedfdummy=data.frame(combinedfdummy)
library(mice)
imp=mice(combinedfdummy,m=2,maxit = 2,method="rf")
combinedfdummyimp=complete(imp)

# NAIVE BAYES Algorithm - classification algorithm which uses bayes theoram or conitional probability as basis 
#                  p(B/A)*p(A)
#p(A/B)= ---------------------------------------------
#                    p(B)
#p(B/A) is called posterior probability or probability of event after evidance is seen 
#p(A) = Priori probability or probability before evidance 
# Bayes rule in data science -X - independent var 
# y - dependent variable 
#                         p(X/y)*p(y)
#   p(y/X)     =   ---------------------------------------               
#                            p(X)
#  p(y/X) - probability of y occuring evidance X has already occured 
#  p(y) & p(X) - probability of events y & X occuring 

#  Naive Bayes Formula for Multiple independent var & multiple classes in dependent variables (more than 2 levels/class)
#
#                              p(X1/y=k)*p(X2/y=k)*p(X3/y=k)...*p(Xn/y=k)*p(y=k)
# p(y=k/X1,X2,X3,....Xn) = ------------------------------------------------------------
#                                     p(X1)*p(X2)*p(X3)......p(Xn)
#
#y - dependent var 
#k - Number of level of classes in dependent var 
# X1,X2,X3 ,... Xn - Independent var 

# Types of Naive Bayes 
# 1 - Bernoulli Naive Bayes classifier - If majority of independent var are binary 
# 2 - Multinomial Naive bayes classifier - if dependent var has more than 2 levels or classes or groups 
# 3 - Gaussian Naive Bayes - If independent var are mojority numerical 

# Vaccine prediction data 

sort(colSums(is.na(combinedfdummyimp)))

#Logistic Regression with seasonal_Vaccine 

vaccinetraindf=combinedfdummyimp[1:26707,]
vaccinetestdf=combinedfdummyimp[26708:53415,]

# Adding dependent variable back to the data frame 
vaccinetraindf['seasonal_vaccine']=vaccinelabel$seasonal_vaccine
logit=glm(seasonal_vaccine~.,data=vaccinetraindf,family = "binomial")
summary(logit)
logitpredict=predict(logit,type="response")
table(Actual=vaccinetraindf$seasonal_vaccine,predict=logitpredict>0.50)
(11626+9312)/(11626+2646+3123+9312)
library(caret)
logitpredict=ifelse(logitpredict>0.50,1,0)
confusionMatrix(as.factor(vaccinetraindf$seasonal_vaccine),as.factor(logitpredict),positive = "1")
confusionMatrix(as.factor(vaccinetraindf$seasonal_vaccine),as.factor(logitpredict),positive = "1",mode = "prec_recall")

#Naive Bayes 


install.packages('e1071')
library(e1071)
nb=naiveBayes(as.factor(seasonal_vaccine)~.,data = vaccinetraindf)
print(nb)
nbpredict=predict(nb,vaccinetraindf)
head(nbpredict)
confusionMatrix(as.factor(vaccinetraindf$seasonal_vaccine),as.factor(nbpredict),positive = "1")


#h1n1 vaccine prediction 

vaccinetraindf['h1n1_vaccine']=vaccinelabel$h1n1_vaccine
h1n1nb=naiveBayes(as.factor(h1n1_vaccine)~.,data = vaccinetraindf[-36])
print(h1n1nb)
h1n1predict=predict(h1n1nb,vaccinetraindf[-36])
confusionMatrix(as.factor(vaccinetraindf$h1n1_vaccine),as.factor(h1n1predict),positive = "1")

