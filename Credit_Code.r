set.seed(123)
setwd()
library(dplyr)
library(ggplot2)


credit_df <- read.csv("Data.csv", header = TRUE, na.strings = c(""," ","NA"))
str(credit_df)
summary(credit_df)
attach(credit_df)

##################################
# Variable Data types conversion #
##################################
credit_df$StatecodeNew <- as.factor(credit_df$StatecodeNew)
credit_df$finance_charges <- as.numeric(credit_df$finance_charges)
credit_df$credit_rating <- as.factor(credit_df$credit_rating)
credit_df$finance_amount <- as.numeric(credit_df$finance_amount)
credit_df$avarage_emi <- as.numeric(credit_df$avarage_emi)
credit_df$noir <- as.numeric(credit_df$noir)
credit_df$noisb <- as.numeric(credit_df$noisb)
credit_df$gih <- as.numeric(credit_df$gih)
credit_df$gis <- as.numeric(credit_df$gis)
credit_df$gic <- as.numeric(credit_df$gic)
credit_df$TypeNew <- as.factor(credit_df$TypeNew)
str(credit_df)
summary(credit_df)

##############################################################################################
#Identify Outliers in each predictor variables                                               #
##############################################################################################
colnames(credit_df)
str(credit_df)
library(ggplot2)
summary(credit_df$finance_rate)
qplot(data = credit_df,x=finance_rate)
ggplot(credit_df,aes(y=finance_rate,x=1)) + geom_boxplot()

library(sqldf)
zero_rate <- sqldf("select count(*) from credit_df where finance_rate = 0")
zero_rate
perc_zerocnt <- zero_rate/count(credit_df)
perc_zerocnt
zero_rate_df <- sqldf("select * from credit_df where finance_rate = 0")
View(zero_rate_df)

# We are not removing these records at this moment as:
# 1. No of such records are huge i.e. around 19%
# 2. We need to see how other variables values look like 
# 3. Business wise, We don't know what kind of loans have been given with 0% rate. But it might be possible that 
# these loans are small amount loan with no interest. 

summary(credit_df$finance_charges)
qplot(data = credit_df,x=finance_charges)
ggplot(credit_df,aes(y=finance_charges,x=1)) + geom_boxplot()
fin_chg_one <- sqldf("select count(*) from credit_df where finance_charges=1")
fin_chg_one
#lot of records (i.e. 1511) with 1 RS finance charges
#Check if there is a relation between minimal finance charges and zero  finance rate

x_rate_charge <- sqldf("select count(*) from credit_df where finance_rate = 0 AND finance_charges=1")
x_rate_charge

# So it is observed that all loans with 0% interest rate incur only 1 rupee of finance charges and 
# business wise its possible.Hence we are not removing these records at this moment.

summary(credit_df$finance_amount)
qplot(data = credit_df,x=finance_amount)
ggplot(credit_df,aes(y=finance_amount,x=1)) + geom_boxplot()

default_fin <- sqldf("SELECT COUNT(*) from credit_df where finance_amount=1")
default_fin
sqldf("SELECT COUNT(*) from credit_df where finance_amount=1 AND avarage_emi !=1")
sqldf("SELECT * from credit_df where finance_amount=1 AND avarage_emi =1")

over_emi <- sqldf("SELECT COUNT(*) from credit_df where finance_amount<avarage_emi")
over_emi

# There a huge no of records(i.e. 6168) whose Average_Emi is greater than the financed amount, that means 
# that EMI calculated here is not only for this loan but overall loan which customer has taken so far, so 
# we cannot remove such records. So now we need to check the noir i.e. Income to installment ratio)

summary(credit_df$avarage_emi)
qplot(data = credit_df,x=avarage_emi)
ggplot(credit_df,aes(y=avarage_emi,x=1)) + geom_boxplot()
sqldf("select count(*) from credit_df where avarage_emi = 1")
sqldf("select count(*) from credit_df where avarage_emi = 1 AND finance_amount >1" )
sqldf("select * from credit_df where avarage_emi = 1 AND finance_amount >1" )

summary(credit_df$noir)
qplot(data = credit_df,x=noir)
ggplot(credit_df,aes(y=noir,x=1)) + geom_boxplot()
sqldf("select count(*) from credit_df where noir =1")
sqldf("select * from credit_df where noir=1")
# This record cannot be removed as other parameters like noisb, gih are strong

summary(credit_df$noisb)
qplot(data = credit_df,x=noisb)
ggplot(credit_df,aes(y=noisb,x=1)) + geom_boxplot()
sqldf("select count(*) from credit_df where noisb >21")
# This scenario can be possible where Average bank balance is far greater than EMI

summary(credit_df$gih)
qplot(data = credit_df,x=gih)
ggplot(credit_df,aes(y=gih,x=1)) + geom_boxplot()
sqldf("select count(*) from credit_df where gih =1")
gih_df <- sqldf("select * from credit_df where gih =1")
View(gih_df)

summary(credit_df$gis)
qplot(data = credit_df,x=gis)
ggplot(credit_df,aes(y=gis,x=1)) + geom_boxplot()
sqldf("select count(*) from credit_df where gis > 1")

summary(credit_df$gic)
qplot(data = credit_df,x=gic)
ggplot(credit_df,aes(y=gic,x=1)) + geom_boxplot()
sqldf("select count(*) from credit_df where gic >1")

#Replace NA in gis and gic with One#
credit_df[is.na(credit_df)] <- 1

###########################################
# Split the file into Training and testing# 
###########################################
smp_size <- floor(0.75 * nrow(credit_df))
smp_size

set.seed(123)
train_ind <- sample(seq_len(nrow(credit_df)), size = smp_size)

train_df <- credit_df[train_ind,]
test_df <- credit_df[-train_ind,]

summary(train_df)
str(train_df)
str(test_df)
############################################
# MODEL 1- Decision tree - library RPART
###########################################
library(rpart)
library(rpart.plot)
library(caret)

## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)

## calling the rpart function to build the tree
rpart_model <- rpart(formula = TypeNew~.,method = "class", data = train_df, control = r.ctrl)

library(rattle)
library(RColorBrewer)
fancyRpartPlot(rpart_model)

## to find how the tree performs
printcp(rpart_model)
plotcp(rpart_model)

#Select the best tree
opt <- which.min(rpart_model$cptable[,"xerror"])
cp <- rpart_model$cptable[opt,"CP"]

#Prune Tree
ptree <- prune(rpart_model,cp = cp)
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")

#Model Evaluation
train_df$predict.class <- predict(rpart_model, train_df, type="class")
train_df$predict.score <- predict(rpart_model, train_df)
head(train_df)
table(train_df$predict.class,train_df$TypeNew)
mean(train_df$predict.class==train_df$TypeNew)
mean(train_df$predict.class!=train_df$TypeNew)

#Fitting model with test data
test_df$predict.class <- predict(rpart_model, test_df, type="class")
test_df$predict.score <- predict(rpart_model, test_df)
head(test_df)
dtree_cm <- confusionMatrix(test_df$predict.class,test_df$TypeNew)
#####################################
#MODEL2: Linear Discrminant Analysis
#####################################
library(MASS)
colnames(train_df)
attach(train_df)
train_df1 <- subset(train_df,select = -c(predict.class,predict.score,credit_rating))
str(train_df1)
summary(train_df1)
attach(train_df1)
lda.fit <- lda(TypeNew~.,data = train_df1)
lda.fit
#plot(lda.fit)
lda.pred <- predict(lda.fit,train_df1)
lda.pred
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class,train_df1$TypeNew)
mean(lda.class==train_df1$TypeNew)
mean(lda.class!=train_df1$TypeNew)

#Fitting model with test data
lda.pred1 <- predict(lda.fit,test_df)
lda.pred1
lda1.class <- lda.pred1$class
table(lda1.class,test_df$TypeNew)
mean(lda1.class==test_df$TypeNew)
mean(lda1.class!=test_df$TypeNew)
lda_cm <- confusionMatrix(lda1.class,test_df$TypeNew)

#################################################
#Model 3 - Logistic regression model fit
#################################################
attach(train_df1)
str(train_df1)
glm.fit <- glm(TypeNew~.,data =train_df1,family = binomial) 
summary(glm.fit)
glm.probs <- predict(glm.fit,train_df1,type="response")
names(glm.probs)
glm.pred <- rep("NA",6000)
glm.pred[glm.probs>0.5]=1
glm.pred[glm.probs<=0.5]=0

table(glm.pred,train_df1$TypeNew)
mean(glm.pred==train_df1$TypeNew)
mean(glm.pred!=train_df1$TypeNew)
View(train_df1)

#Fitting model with test data
glm.testprob <- predict(glm.fit,test_df,type="response")
glm.testpred <- rep("NA",2001)
glm.testpred[glm.testprob>0.5]=1
glm.testpred[glm.testprob<=0.5]=0

table(glm.testpred,test_df$TypeNew)
mean(glm.testpred==test_df$TypeNew)
mean(glm.testpred!=test_df$TypeNew)
glm_cm <- confusionMatrix(glm.testpred,test_df$TypeNew)

#################################################
#Model 4 - Random Forest 
#################################################

str(train_df1)
summary(train_df1)
train_df2 <- train_df1
str(train_df2)

rf_model <- train(TypeNew~., tuneLength=3, data=train_df2, method="ranger",
                  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))

plot(rf_model)

pred <- predict(rf_model, test_df)
train_df$predict.score <- predict(rpart_model, train_df)
head(train_df)
table(pred,test_df$TypeNew)
rf_cm <- confusionMatrix(pred,test_df$TypeNew)
