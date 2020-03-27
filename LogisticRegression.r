#Install required libraries
install.packages("ggplot2")
install.packages("corrplot")
install.packages("cowplot")
install.packages("dplyr")

#Load the libraries
library(ggplot2)
library(MASS)
library(corrplot)
library(cowplot)
library(dplyr)

#Load Data
credit_risk_data <- read.csv("/cloud/project/Credit_Risk_Train_data.csv")

#View Data
View(credit_risk_data)

str(credit_risk_data)
summary(credit_risk_data)

#Remove Loan Id from the data as it is not required.
credit_risk_data=credit_risk_data[-1]

#Check if any null or empty values are present
colSums(is.na(credit_risk_data))
colSums(credit_risk_data=='')

#Histogram Plot for Loan Amount, Applicant Income and Co-Applicant Income
hist_loan_amt <- ggplot(credit_risk_data, aes(x = LoanAmount)) + geom_histogram(binwidth = 10) +
  geom_vline(xintercept = mean(credit_risk_data$LoanAmount), color = "indianred") +
  geom_vline(xintercept = median(credit_risk_data$LoanAmount), color = "cornflowerblue")
hist_app_income <- ggplot(credit_risk_data, aes(x = ApplicantIncome)) + geom_histogram(binwidth = 1000) +
  geom_vline(xintercept = mean(credit_risk_data$ApplicantIncome), color = "indianred") +
  geom_vline(xintercept = median(credit_risk_data$ApplicantIncome), color = "cornflowerblue")
hist_CO_app_income <- ggplot(credit_risk_data, aes(x = CoapplicantIncome)) + geom_histogram(binwidth = 1000) +
  geom_vline(xintercept = mean(credit_risk_data$CoapplicantIncome), color = "indianred") +
  geom_vline(xintercept = median(credit_risk_data$CoapplicantIncome), color = "cornflowerblue")

plot_grid(hist_loan_amt, hist_app_income, hist_CO_app_income, labels = "AUTO")

ggplot(data = credit_risk_data, aes(x = LoanAmount, y = Loan_Status, col = Property_Area)) + geom_point()

#Create a function to get mode values
Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if(all(ta == tam))
    mod = NA
  else if(is.numeric(x))
    mod = as.numeric(names(ta))[ta==tam]
  else
    mod = names(ta)[ta==tam]
  return(mod)
}

#create a copy of dataset as backup.
credit_risk_data_copy = credit_risk_data

#Get mode value from the Mode function for character values.
credit_risk_data$Gender[is.na(credit_risk_data$Gender)] = Mode(credit_risk_data$Gender)
credit_risk_data$Married[is.na(credit_risk_data$Married)] = Mode(credit_risk_data$Married)
credit_risk_data$Dependents[is.na(credit_risk_data$Dependents)] = Mode(credit_risk_data$Dependents)
credit_risk_data$Credit_History[is.na(credit_risk_data$Credit_History)] = Mode(credit_risk_data$Credit_History)

#Get mean values for nummeric data
credit_risk_data$LoanAmount[is.na(credit_risk_data$LoanAmount)] <- mean(credit_risk_data$LoanAmount, na.rm = T)
credit_risk_data$Loan_Amount_Term[is.na(credit_risk_data$Loan_Amount_Term)] <- mean(credit_risk_data$Loan_Amount_Term, na.rm = T)

summary(credit_risk_data)

#Create a function to get replace outliers with lower or upper cutoff
replace_outliers = function(x){
  m = mean(x)
  s = sd(x)
  lc = m-3*s
  uc= m+3*s
  n = sum(x>uc |  x<lc )
  val = list(num=n,lower_cutoff=lc,upper_cutoff=uc)
  return(val)
}

#Replace the outliers of Co-Applicant Income
lc=replace_outliers(credit_risk_data$CoapplicantIncome)$lower_cutoff
uc=replace_outliers(credit_risk_data$CoapplicantIncome)$upper_cutoff
credit_risk_data$CoapplicantIncome[credit_risk_data$CoapplicantIncome>uc]=uc
credit_risk_data$CoapplicantIncome[credit_risk_data$CoapplicantIncome<lc]=lc

#Replace the outliers of Loan Amount
credit_risk_data$LoanAmount=as.numeric(credit_risk_data$LoanAmount)
lc=replace_outliers(credit_risk_data$LoanAmount)$lower_cutoff
uc=replace_outliers(credit_risk_data$LoanAmount)$upper_cutoff
credit_risk_data$LoanAmount[credit_risk_data$LoanAmount>uc]=uc
credit_risk_data$LoanAmount[credit_risk_data$LoanAmount<lc]=lc

#Replace the outliers of Applicant Income
lc=replace_outliers(credit_risk_data$ApplicantIncome)$lower_cutoff
uc=replace_outliers(credit_risk_data$ApplicantIncome)$upper_cutoff
credit_risk_data$ApplicantIncome[credit_risk_data$ApplicantIncome>uc]=uc
credit_risk_data$ApplicantIncome[credit_risk_data$ApplicantIncome<lc]=lc

summary(credit_risk_data)

table(credit_risk_data$Property_Area)

#Create Dummy variables for Categorical variables with 2 values 
credit_risk_data$Dummy_Gender=ifelse(credit_risk_data$Gender=="Male",1,0)
credit_risk_data$Dummy_Married=ifelse(credit_risk_data$Married=="Yes",1,0)
credit_risk_data$Dummy_Education=ifelse(credit_risk_data$Education=="Graduate",1,0)
credit_risk_data$Dummy_Self_employed=ifelse(credit_risk_data$Self_Employed=="Yes",1,0)

#Create Dummy variables for Categorical variables with 3 values
credit_risk_data$Dummy_Urban=ifelse(credit_risk_data$Property_Area=="Urban",1,0)
credit_risk_data$Dummy_Rural=ifelse(credit_risk_data$Property_Area=="Rural",1,0)
credit_risk_data$Dummy_Semiurban=ifelse(credit_risk_data$Property_Area=="Semiurban",1,0)

credit_risk_data$Dummy_Dep=as.numeric(substr(credit_risk_data$Dependents,1,1))

#Target Variable
credit_risk_data$Loan_Status=ifelse(credit_risk_data$Loan_Status=="Y",1,0)

#Correlation plot
numeric <- credit_risk_data[sapply(credit_risk_data, is.numeric)]
descrCor <- cor(numeric)
corrplot(descrCor)

#Model Building
cr_df_train=select(credit_risk_data,-Gender,-Married,-Education,-Self_Employed,-Dependents,-Property_Area) 

train_data = select(cr_df_train,-Loan_Status)
model1=glm(Loan_Status~., data=cr_df_train, family=binomial("logit"))
summary(model1)

#Accuracy tesing using Confusion Matrix
fitted.results1 = predict(model1, newdata=train_data, type='response')
fitted.results1 = ifelse(fitted.results1 >=0.5,1,0)
cf1 = table(predicted = fitted.results1, actual = cr_df_train$Loan_Status)

# True Negative - Actual & Predicted is 0/N
TN = cf1[1,1] 

# True Positive - Actual & Predicted is 1/Y
TP = cf1[2,2] 

# False Positive - Actual is 0/N but Predicted is 1/Y
FP = cf1[2,1] 

# False Nefgative - Actual is 1/Y but Predicted is 0/N
FN = cf1[1,2] 

# Total Observations
TO = TN+TP+FP+FN 

# Accuracy of Confusion Matrix
accuracy = (TP+TN)/TO 
accuracy # 80.63%

sensitivity = TP/(TP+FN) # True Positive Rate
sensitivity

specificity = TN/(TN+FP) # True Negative Rate
specificity





#Prediction on Test data set
# Load the test/validation dataset
credit_risk_test_data <- read.csv("/cloud/project/Credit_Risk_Train_data.csv/Credit_Risk_Validate_data.csv")

# Prepare data for logistic regression model same as training dataset like NA treatment, dummy variables, etc.
credit_risk_test_data=credit_risk_test_data[-1] #Removing Loan_ID as it has no logical corelation

# Null values treatment
credit_risk_test_data$LoanAmount[is.na(credit_risk_test_data$LoanAmount)] <- mean(credit_risk_test_data$LoanAmount, na.rm = T)
credit_risk_test_data$Loan_Amount_Term[is.na(credit_risk_test_data$Loan_Amount_Term)] <- mean(credit_risk_test_data$Loan_Amount_Term, na.rm = T)
credit_risk_test_data$Gender[is.na(credit_risk_test_data$Gender)] = Mode(credit_risk_test_data$Gender)
credit_risk_test_data$Married[is.na(credit_risk_test_data$Married)] = Mode(credit_risk_test_data$Married)
credit_risk_test_data$Dependents[is.na(credit_risk_test_data$Dependents)] = Mode(credit_risk_test_data$Dependents)
credit_risk_test_data$Credit_History[is.na(credit_risk_test_data$Credit_History)] = Mode(credit_risk_test_data$Credit_History)

# Dummy variables creation for categorical attributes
credit_risk_test_data$Dummy_Gender=ifelse(credit_risk_test_data$Gender=="Male",1,0)
credit_risk_test_data$Dummy_Married=ifelse(credit_risk_test_data$Married=="Yes",1,0)
credit_risk_test_data$Dummy_Education=ifelse(credit_risk_test_data$Education=="Graduate",1,0)
credit_risk_test_data$Dummy_Self_employed=ifelse(credit_risk_test_data$Self_Employed=="Yes",1,0)
credit_risk_test_data$Dummy_Urban=ifelse(credit_risk_test_data$Property_Area=="Urban",1,0)
credit_risk_test_data$Dummy_Rural=ifelse(credit_risk_test_data$Property_Area=="Rural",1,0)
credit_risk_test_data$Dummy_Semiurban=ifelse(credit_risk_test_data$Property_Area=="Semiurban",1,0)
credit_risk_test_data$Dummy_Dep=as.numeric(substr(credit_risk_test_data$Dependents,1,1)) # take first character

credit_risk_test_data$outcome=ifelse(credit_risk_test_data$outcome=="Y",1,0) # target response variable

# Remove corresponding variables for dummy and outcome
test_data=select(credit_risk_test_data,-Gender,-Married,-Education,-Self_Employed,-Dependents,-Property_Area,-outcome)

# Validation of our model using validation dataset
fitted.results2 = predict(model1, newdata=test_data, type='response')

# If results are more than 50% then convert to 1 else 0
fitted.results2 = ifelse(fitted.results2 >=0.5,1,0)

cf2 = table(predicted = fitted.results2, actual = cr_df_test$outcome)
cf2 # Check Confuxion Matrix

TN = cf2[1,1] # True Negative - Actual & Predicted is 0/N
TP = cf2[2,2] # True Positive - Actual & Predicted is 1/Y
FP = cf2[2,1] # False Positive - Actual is 0/N but Predicted is 1/Y
FN = cf2[1,2] # False Nefgative - Actual is 1/Y but Predicted is 0/N
TO = TN+TP+FP+FN # Total Observations

accuracy = (TP+TN)/TO # Accuracy or Prevalance of Confusion Matrix
accuracy 

sensitivity = TP/(TP+FN) # True Positive Rate
sensitivity # 

specificity = TN/(TN+FP) # True Negative Rate
specificity

error = (FP+FN)/TO # Error Rate
error


