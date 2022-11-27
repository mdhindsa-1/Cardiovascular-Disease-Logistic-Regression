library(ggplot2)
library(ggrepel)
library(dplyr)
library(splitstackshape)

# Set the directory to the respective folder, this will differ for everyone
setwd('/Users/sarahrodgers/Documents/Grad School/Spring 2021/STAT 6021 - Linear Regression/Project 2')

# Importing the data
df_org<-read.csv('cardio_train.csv')

##########
## Cleaning
#########

df <- cSplit(df_org, "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio", sep=";")
df <- df %>% rename("ID" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_01", 
                    "Age" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_02",
                    "Gender" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_03" , 
                    "Height" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_04", 
                    "Weight" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_05", 
                    "AP_HI" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_06", 
                    "AP_LO" =  "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_07", 
                    "Cholesterol" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_08", 
                    "Glucose" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_09", 
                    "Smoke" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_10", 
                    "Alcohol" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_11", 
                    "Active" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_12", 
                    "Cardio" = "id.age.gender.height.weight.ap_hi.ap_lo.cholesterol.gluc.smoke.alco.active.cardio_13")

# Assign variables to be factors
is.factor(df$Cardio)
df$Cardio <- factor(df$Cardio)
is.factor(df$Cardio)

is.factor(df$Cholesterol)
df$Cholesterol <- factor(df$Cholesterol)
is.factor(df$Cholesterol)

is.factor(df$Gender)
df$Gender <- factor(df$Gender)
is.factor(df$Gender)

is.factor(df$Glucose)
df$Glucose <- factor(df$Glucose)
is.factor(df$Glucose)

is.factor(df$Smoke)
df$Smoke <- factor(df$Smoke)
is.factor(df$Smoke)

is.factor(df$Alcohol)
df$Alcohol <- factor(df$Alcohol)
is.factor(df$Alcohol)

is.factor(df$Active)
df$Active <- factor(df$Active)
is.factor(df$Active)

# Convert Age from days to years 
df$Age <- floor(df$Age/365.25)

# Grouping Glucose and Cholesterol
levels(df$Glucose) <- list('Group 1' = c("1"),
                           'Group 2' = c("2","3"))


levels(df$Cholesterol) <- list('Group 1' = c("1"),
                               'Group 2' = c("2","3"))

# Adding a BMI predictor 
df$BMI <- (df$Weight/(df$Height)^2)*703

##########
## EDA
#########

# Is there any data missing
any(is.null(df))

# Correlation Matrices
num_vars <- df %>% select(Age, Height, Weight, AP_HI, AP_LO)
corrs <- cor(num_vars)
corrs

# Heatmap 
library(reshape2)
cor_df <- cor(num_vars, method = "pearson")
melted_df <- melt(cor_df)
head(melted_df)

library(ggplot2)
ggheatmap <- ggplot(data = melted_df, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  ggtitle("Correlation Matrix of Numeric Predictors") +
  labs(y="", x = "")

ggheatmap + 
  geom_text(aes(Var2, Var1, label = round(value, digits=4)), color = "white", size = 4)

# Boxplots
# Add additional graphics that show the distribution of developing cardiovascular disease across other predictors (box plots?)
ggplot(df, aes(x = factor(Cardio), 
               fill = factor(Cardio))) +
  geom_bar() + 
  ggtitle("Cardiovascular Disease Distribution") +
  theme_classic() + 
  scale_fill_brewer(palette = "Paired") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

# Other boxplots
par(mfrow=c(1,3))
boxplot(df$Age~df$Cardio, main = "Boxplot of Age for subjects with Cardiovascular Disease")
boxplot(df$Height~df$Cardio, main = "Boxplot of Height for subjects with Heart Disease")
boxplot(df$Weight~df$Cardio, main = "Boxplot of Weight for subjects with Heart Disease")
par(mfrow=c(1,2))
boxplot(df$AP_HI~df$Cardio, main = "Boxplot of Systolic BP for subjects with CD")
boxplot(df$AP_LO~df$Cardio, main = "Boxplot of Diastolic BP for subjects with CD")

# Zoom in on the BP plots
par(mfrow=c(2,2))
boxplot(df$AP_HI~df$Cardio, main = "Boxplot of Systolic BP for subjects with CD", ylim = c(50, 200))
# Finding IQRs for AP_HI
df_NoCardio <- df %>% filter(.,df$Cardio == 0)
df_YesCardio <- df %>% filter(.,df$Cardio == 1)
quantile(df_NoCardio$AP_HI)
quantile(df_YesCardio$AP_HI)

boxplot(df$AP_LO~df$Cardio, main = "Boxplot of  Diastolic BP for subjects with CD", ylim = c(50, 200))

# Finding IQRs for AP_LO
df_NoCardio <- df %>% filter(.,df$Cardio == 0)
df_YesCardio <- df %>% filter(.,df$Cardio == 1)
quantile(df_NoCardio$AP_LO)
quantile(df_YesCardio$AP_LO)

par(mfrow=c(2,3))
library(ggpubr)
library(ggplot2)
library(ggrepel)
# Stacked bar charts for the categorical predictors
A <- ggplot(df, aes(x = factor(Cardio), 
               fill = factor(Cholesterol))) +
  geom_bar() + 
  ggtitle("CD by Cholesterol") +
  theme_classic() +
  scale_fill_brewer(palette="Paired") +
  geom_text_repel(stat='count', aes(label=..count..))

B <- ggplot(df, aes(x = factor(Cardio), 
               fill = factor(Glucose))) +
  geom_bar() + 
  ggtitle("CD by Glucose") +
  theme_classic() +
  scale_fill_brewer(palette="Paired") +
  geom_text_repel(stat='count', aes(label=..count..))

C <- ggplot(df, aes(x = factor(Cardio), 
               fill = factor(Smoke))) +
  geom_bar() + 
  ggtitle("CD by Smoke") +
  theme_classic() +
  scale_fill_brewer(palette="Paired") +
  geom_text_repel(stat='count', aes(label=..count..))

D <- ggplot(df, aes(x = factor(Cardio), 
               fill = factor(Alcohol))) +
  geom_bar() + 
  ggtitle("CD by Alcohol") +
  theme_classic() +
  scale_fill_brewer(palette="Paired") +
  geom_text_repel(stat='count', aes(label=..count..))

E <- ggplot(df, aes(x = factor(Cardio), 
               fill = factor(Active))) +
  geom_bar() + 
  ggtitle("CD by Activity") +
  theme_classic() +
  scale_fill_brewer(palette="Paired") +
  geom_text_repel(stat='count', aes(label=..count..))

F <- ggplot(df, aes(x = factor(Cardio), 
               fill = factor(Gender))) +
  geom_bar() + 
  ggtitle("CD by Gender") +
  theme_classic()+
  scale_fill_brewer(palette="Paired") +
  geom_text_repel(stat='count', aes(label=..count..))

ggarrange(A, B, C, D, E, F + rremove("x.text"), 
          ncol = 3, nrow = 2)

#####################
## Assessing Outliers
#####################

# Since we have a binary response, we decided it was not logical to identify outliers in the response. 
# Therefore we did not assess high leverage points or the externally studentized residuals.

####Cook's distance 
# Looking for outliers without AP_HI (Explain why we dropped it)
resultalls <- glm(df$Cardio~df$Age+df$Gender+df$Height+df$Weight+
                    df$AP_LO+df$Cholesterol+df$Glucose+df$Smoke+df$Alcohol+df$Active, family = 'binomial')
n<-70000
p<-11
##influential observations
COOKS<-cooks.distance(resultalls)
COOKS[COOKS>qf(0.5,p,n-p)]
# Returns 0 (take language from HW 10)

#####################
# Subsetting the Data
#####################

# Subset the data on people who are 52 years or older
df_52 <- df %>% filter(.,df$Age >= 52)
# Check the distributions of subjects with and without cardiovascular disease
ggplot(df_52, aes(x = factor(Cardio), 
               fill = factor(Cardio))) +
  geom_bar() + 
  ggtitle("Cardiovascular Disease Distribution for People older than 52") +
  theme_classic() + 
  scale_fill_brewer(palette = "Paired") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

#####################
# Test and Training
#####################
set.seed(111)

# Split the dataset of people who are 52 years or older into test and training
sample<-sample.int(nrow(df_52), floor(.50*nrow(df_52)))
train<-df_52[sample, ]
test <-df_52[-sample, ]
test <- test[-1,]

# Check the distributions of subjects with and without cardiovascular disease for training and test
ggplot(train, aes(x = factor(Cardio), 
                  fill = factor(Cardio))) +
  geom_bar() + 
  ggtitle("Cardiovascular Disease Distribution for People older than 52 - Training Dataset") +
  theme_classic() + 
  scale_fill_brewer(palette = "Paired") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

ggplot(test, aes(x = factor(Cardio), 
                  fill = factor(Cardio))) +
  geom_bar() + 
  ggtitle("Cardiovascular Disease Distribution for People older than 52 - Test Dataset") +
  theme_classic() + 
  scale_fill_brewer(palette = "Paired") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

#####################
# Building the model 
#####################

library(leaps)
allreg.model <- regsubsets(train$Cardio~train$Age+train$Gender+train$Height+train$Weight+
                             train$AP_LO+train$Cholesterol+train$Glucose+train$Smoke+train$Alcohol+train$Active, data = train)

allreg <- glm(train$Cardio~train$Age+train$Gender+train$Height+train$Weight+
                 train$AP_LO+train$Cholesterol+train$Glucose+train$Smoke+train$Alcohol+train$Active, family=binomial, data=train)

best <- as.data.frame(summary(allreg.model)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg.model)$rsq
best$adjr2 <- summary(allreg.model)$adjr2
best$mse <- (summary(allreg.model)$rss)/(dim(train)[1]-best$p)
best$cp <- summary(allreg.model)$cp
best$bic <- summary(allreg.model)$bic
best

##sort by various criteria
best[order(best$adjr2, decreasing = TRUE),]
# ADJ2 returns: Age, Height, Weight, AP_LO, Cholesterol, Smoke, Alcohol, and Active
best[order(best$mse),]
# MSE returns: Age, Height, Weight, AP_LO, Cholesterol, Smoke, Alcohol, and Active
best[order(best$cp),]
# CP returns: Age, Height, Weight, AP_LO, Cholesterol, Smoke, Alcohol, and Active
best[order(best$bic),]
# BIC returns: Age, Height, Weight, AP_LO, Cholesterol, Smoke, and Active

##intercept only model
regnull <- glm(train$Cardio~1, family="binomial", data=train)

## backward elimination regression
step(allreg, scope=list(lower=regnull, upper=allreg), direction="backward")
step(allreg, scope=list(lower=regnull, upper=allreg), direction="forward")
step(allreg, scope=list(lower=regnull, upper=allreg), direction="both")

# Backward & Both - drops Glucose, Gender
# Forward - keeps everything

# Backward and Stepwise
backward_both_model <- glm(train$Cardio~train$Age + train$Height + train$Weight + train$AP_LO + 
                    train$Cholesterol + train$Smoke + train$Alcohol + 
                    train$Active, family=binomial, data=train)

preds_backward <- predict(backward_both_model, newdata=test, type="response")
rates <- prediction(preds_backward, test$Cardio)

# Forward
forward_model <- glm(train$Cardio~train$Age + train$Height + train$Weight + train$AP_LO + 
                    train$Cholesterol + train$Glucose + train$Alcohol + 
                    train$Active + train$Smoke + train$Gender, family=binomial, data=train)

preds_forward <- predict(forward_model, newdata=test, type="response")
rates <- prediction(preds_forward, test$Cardio)

# ROC plot, we did not generate it because it is a rare event, add more context in the report

# Confusion Matrix
final_model <- glm(train$Cardio~train$Age + train$Height + train$Weight + train$AP_LO + 
                             train$Cholesterol + train$Smoke + train$Active, family=binomial, data=train)
preds <- predict(final_model, newdata=test, type="response")

table(test$Cardio, preds>0.5) # 3681/(8292+3681) = 0.3074417
# We are more concerned about False Negatives (classifying someone survived as someone who died)
# Lowering the threshold since we are more interested in the FNR, increases the FPR and reduces FNR
table(test$Cardio, preds>0.4)
# FNR-Backwards=  974/(974+10999) =  0.0813497

##############
# Hypothesis Tests
#############

# Model Deviance to confirm Glucose and Gender can be dropped
#H0 : Glucose = Gender = 0.
#Ha : at least one βk above is not zero
result<-glm(train$Cardio~train$Age+train$Height+train$Weight+
              train$AP_LO+train$Cholesterol+train$Smoke+train$Alcohol+train$Active+train$Gender+train$Glucose, family="binomial", data=train)
summary(result)

resultreduced<- glm(train$Cardio~train$Age+train$Height+train$Weight+
                      train$AP_LO+train$Cholesterol+train$Smoke+train$Alcohol+train$Active, family="binomial", data=train)
summary(resultreduced)

1-pchisq(result$deviance-resultreduced$deviance,2)
# Returns 1 - Therefore we fail to reject the null hypothesis and conclude that Glucose and Gender can be dropped from the model

# Can alcohol be dropped - Use Walds Test
summary(resultreduced)
# It can be seen that the p value for Alcohol is 0.10678, therefore we can remove it from the model because it is not significant

# Is the model useful in predicting people who develop cardiovascular disease
# Used Chi-Square Model Deviance
# Null Hypothesis: All predictors all equal 0
# Alternative Hypothesis: At least one of the predictors above does not equal 0
set.seed(111)
result <- glm(train$Cardio~train$Age+train$Height+train$Weight+
                 train$AP_LO+train$Cholesterol+train$Smoke+train$Active, family="binomial", data=train)

summary(result)
1-pchisq(result$null.deviance-result$deviance,7)

#pvalue is 0. Our data supports the claim that our logistic regression model is useful in estimating the log odds of developing Heart Disease.

# Assessing if the BMI predictor is helpful
result_BMI<-glm(train$Cardio~train$Age+train$Height+train$Weight+train$BMI+
                  train$AP_LO+train$Cholesterol+train$Smoke+train$Active, family="binomial", data=train)
summary(result_BMI)
# Using Walds Test, the p value is above 0.05, therefore this predictor is not significant and therefore can be dropped from the model. 
# A future consideration would be assessing if Weight and Height are dropped and BMI is included instead, does the model improve

#############
# Results
#############

# Probability and Odds
# Scenario 1
# Unhealthy - 62 years old, 180 (5'9), 100, 97, 1, 1, 0
x = -4.8936545 + 0.0901056*62 - 0.0099160*180 + 0.0094765*100 + 0.0014699*97 + 0.6646955*1 - 0.1887472*1 - 0.2107440*0
odds = exp(x)
odds# 1.606714
prob = exp(x)/(1+exp(x))
prob #0.6163753

# Scenario 2
# Healthy - 35 years old, 160, 135, 95
x = -4.8936545 + 0.0901056*35 - 0.0099160*160 + 0.0094765*61 + 0.0014699*95 + 0.6646955*0 - 0.1887472*0 - 0.2107440*1
odds = exp(x)
odds#  0.05963244
prob = exp(x)/(1+exp(x))
prob #0.05627654

# Scenario 3
x = -4.8936545 + 0.0901056*70 - 0.0099160*175 + 0.0094765*70 + 0.0014699*100 + 0.6646955*0 - 0.1887472*0 - 0.2107440*1
odds = exp(x)
odds#  1.320537
prob = exp(x)/(1+exp(x))
prob #0.5690653

## Confidence Intervals for estimates
qnorm(1-(.05/2))
# Age
(0.0901056 - 0.0041782*1.96) 
(0.0901056 + 0.0041782*1.96) 
# 0.08191633, 0.09829487
# Diastolic Blood Pressure
(0.0014699 - 0.0001765*1.96) 
(0.0014699 + 0.0001765*1.96) 
# 0.00112396, 0.00181584

# Weight
(0.009476 - 0.0005210*1.96) 
(0.009476 + 0.0005210*1.96) 
# 0.00845484, 0.01049716

## ROC Curve
library(ROCR)

preds<-predict(result,newdata=test, type="response")
rates<-prediction(preds, test$Cardio)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for CD")
lines(x = c(0,1), y = c(0,1), col="red")

# Part c 
# Findings the AUC
auc<-performance(rates, measure = "auc")
auc

