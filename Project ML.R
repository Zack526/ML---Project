#Fin project 
# Clear the workspace
# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")       # Clear the console
options(scipen = 4) # Remove scientific notation for numbers
# Prepare needed libraries
packages <- c("ggplot2" # Best plotting
              , "gridExtra" # Arranging multiple plots
              , "stargazer" # For comparing models
              , "margins" # For marginal effects in logit
              , "MASS" # For proper CIs in logit
              , "sampleSelection" # To load RAND dataset
)
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i], dependencies = TRUE)
  }
  library(packages[i], character.only=TRUE)
  library(ISLR)
  library(tibble)
  library(caret)
  library(MASS)
  library(stats)
  library(gridExtra)
  library(caret)
  library(pROC)
  library(data.table)
  library(bestglm)
  library(car)
  library(DAAG)
  library(latticeExtra)
  library(bootstrap)
  library(rpart.plot)
  library(gbm)
  library(randomForest)

}
rm(packages)


#Download income.RData
#loads data from my computer, depends on where u run it from
load("~/Downloads/income.RData")

# check if anything is missing, nothing is but if there was the idea would be to drop it 
table (complete.cases (adult))

## creates a new var that combines total gain and loss into one 
adult$totgain <- adult$capital.gain - adult$capital.loss

# Split data into train (90%) and test (10%)

set.seed(1000)
rows.train <- sample(1:nrow(adult), 0.9*nrow(adult), replace = FALSE)
adult.train <- adult[rows.train, ] 
adult.test <- adult[-rows.train, ]
rm(rows.train)



#data explore, 
summary(adult)
# income 

summary(adult.train["income"])
# Looking at sex 

table <- table(adult.train$sex)
prop.table(table)

# capital
summary(adult.train$capital.gain)
summary(adult.train$capital.loss)

#v= data 1 and 2 is used to look at non zero cap
data1 <- adult.train [which(adult.train$capital.gain > 0),]
data2 <- adult.train [which(adult.train$capital.loss > 0),]
hist1 <- ggplot(data1) + aes(x=as.numeric(capital.gain), group=income, fill=income) + 
  geom_histogram(bins=25) + ggtitle('Histogram of Capital gain')

hist2 <- ggplot(data2) + aes(x=as.numeric(capital.loss), group=income, fill=income) + 
  geom_histogram(bins=25) + ggtitle('Histogram of loss gain')

grid.arrange (hist1, hist2, nrow = 2)
# age
summary(adult.train$age)
boxplot (age ~ income, data = adult.train, 
         main = "Age distribution for different income levels",
         xlab = "Income Levels", ylab = "Age", col = "salmon")

#set up to look at age (thanks r guide)
incomeless50 <- (adult.train$income == "<=50K") 
xlimit<- c (min (adult.train$age), max (adult.train$age))
ylimit<- c (0, 1600)

#graph age by income
hist3<- qplot (age, data = adult.train[incomeless50,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = income)

hist4<- qplot (age, data = adult.train[!incomeless50,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = income)

grid.arrange (hist3, hist4, nrow = 2)

 

# explore the corr between con variables 
corMat<- cor (adult.train[, c("age", "education.num", "capital.gain", "capital.loss" )])
corMat

#eplore workclass
summary(adult.train$workclass)

table2 <- table(adult.train$workclass)
prop.table(table2)


qplot (income
       , data = adult.train
       ,fill = workclass) + facet_grid (. ~ workclass)
# education
summary(adult.train$education.num)

#hours.per.week
summary(adult.train$hours.per.week)

#hours
summary(adult.train$hours)

#hours and work class
qplot (income
       , data = adult.train
       ,fill = workclass) + facet_grid (. ~ hours)

#occupation 
table2 <- table(adult.train$occupation)
prop.table(table2)

#region

table3 <- table(adult.train$region)
prop.table(table3)

#marital.status
table4 <- table(adult.train$marital.status)
prop.table(table4)

#relationship 
table5 <- table(adult.train$relationship)
prop.table(table5)

table6 <- table(adult.train$race)
prop.table(table6)

# simple reg model 
x <- c("education.num",
       "I(education.num^2)"
       ,"I(education.num^3)"
       , "hours"
       , "age"
       ,"I(age^2)"
       ,"I(age^3)"
       ,"I(age^4)"
       , "workclass"
       , "occupation"
       , "region"
       ,"marital.status"
       ,"relationship"
       ,"race"
       , "capital.gain"
       ,"hours.per.week"
       , "capital.loss")
y <- "income.group"
model.x <- paste(x, sep = " ", collapse = " + ") 
model <- as.formula(paste(y, " ~ ", model.x, sep = "")) 



#Estimate models
# Linear probability model
lpm.a <- lm(model, adult.train) 

lpm.m <- lm(model, adult.train[adult.train$sex == "Female", ])
lpm.f <- lm(model, adult.train[adult.train$sex == "Male", ])
lmp.age <-lm(model, adult.train[adult.train$age > 40, ])
lmp.cap <-lm(model, adult.train[adult.train$capital.gain > 0, ])
summary(lpm.a)
summary(lpm.m)
summary(lpm.f)
summary(lmp.age)
summary(lmp.cap)

#lpm still seems to be the best model 


model_summ <- summary(lpm.a)

lmp.amse <- mean(model_summ$residuals^2)

lmp.amse
#Improving linear regression model using cross validation

cv <- cv.lm(data = adult.train, lpm.a, m=10, plotit = TRUE, printit = TRUE)



#Use a fitting Logistic regression to train the data  
log.a <- glm(model, adult.train, family = binomial)


#validate the logistic regression using precision metrics

logit.a <- predict(log.a, newdata = adult.test, type = "response")
logit.a.pred <- ifelse(logit.a >= 0.5, 1, 0) #test prediction model by using a threshold of 0.5 to classify predictions
logit.b.pred <- ifelse(logit.a >= 0.3, 1, 0) #compare to a threshold of 0.3
logcm2 <- table(adult.test$income,logit.b.pred)
logcm2
logcm <- table(adult.test$income,logit.a.pred)
View(logcm2)
(logcm)

#Calculate precision metrics: sensitivity rate, specificity rates for both models  
log.asensitivity <- (logcm[2,2]) /(logcm[2,2] + logcm[2,1])
log.bsensitivity <- logcm2[2,2] /(logcm2[2,2] + logcm2[2,1])
#View(log.asensitivity)
#View(log.bsensitivity)
log.bsensitivity
log.asensitivity

log.aspecificity <- logcm[1,1] / (logcm[1,1] + logcm[1,2])
log.bspecificity <- logcm2[1,1] /(logcm2[1,1] + logcm2[1,2])
# Sensitivity tells us how often precise the model is at predicting income >50K
#View(log.aspecificity)
log.aspecificity
#View(log.bspecificity)
log.bspecificity
#Specificity rate tells us how precise the model is at predicting income <50K


# Now let's calculate classification accuracy using model a
log.bclassification <- (logcm2[1,1] + logcm2[2,2])/(logcm2[1,1] + logcm2[1,2] + logcm2[2,1] + logcm2[2,2])
#View(log.bclassification)
log.bclassification
log.bclasserror <- (logcm2[2,1] + logcm2[1,2])/(logcm2[1,1] + logcm2[1,2] + logcm2[2,1] + logcm2[2,2])
log.bclasserror
#View(log.bclasserror)

#False positive rate
fpr1 <- (1 - (log.aspecificity))
fpr2 <- (1 - (log.bspecificity))

fpr2
fpr1

precision1 <- (logcm[2,2]) / (logcm[2,2] + logcm[1,2])
precision1
precision2 <- (logcm2[2,2]) / (logcm2[2,2] + logcm2[1,2])
precision2
 