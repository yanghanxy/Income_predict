
rm(list = ls())
Sys.setenv(LANG = "en")

setwd(dir="D:/UPC/2016_Spring/ML/Project/")
#write.table(X.adult.complete, file = "adult.data.csv", sep = ",", col.names = NA )
#X.adult.complete = read.table("adult.data.csv", sep=",", header=TRUE, fill=FALSE, strip.white=T)[-1]

# read X.adult
X.adult = read.table("D:/UPC/2016_Spring/ML/Project/Adult_dataset/adult.data", sep=",", header=F, fill=FALSE, strip.white=T)

colnames(X.adult) = c("age", "workclass", "fnlwgt", "education", "education_num", 
                      "marital_status", "occupation", "relationship", "race", "sex",
                      "capital_gain", "capital_loss", "hours_per_week", "native_country", 
                      "income_class")

#################################################################################################
#################################################################################################
#################################################################################################

X.adult[["fnlwgt"]]=NULL
X.adult[["education"]]=NULL

#plot(X.adult[1:3000,])

par(mfrow = c(1,4))
boxplot(X.adult$age, main="boxplot age")
boxplot(X.adult$capital_gain, main="boxplot capital_gain")
boxplot(X.adult$capital_loss, main="boxplot capital_loss")
boxplot(X.adult$hours_per_week, main="boxplot hours_per_week")

par(mfrow = c(2,4))
barplot(prop.table(table(X.adult$workclass)), main = "barplot workclass")
barplot(prop.table(table(X.adult$marital_status)), main = "barplot marital_status")
barplot(prop.table(table(X.adult$education)), main = "barplot education")
barplot(prop.table(table(X.adult$occupation)), main = "barplot occupation")
barplot(prop.table(table(X.adult$relationship)), main = "barplot relationship")
barplot(prop.table(table(X.adult$race)), main = "barplot race")
barplot(prop.table(table(X.adult$sex)), main = "barplot sex")
barplot(prop.table(table(X.adult$native_country)), main = "barplot native_country")

#################################################################################################
#################################################################################################
#################################################################################################


X.adult.caploss= X.adult[X.adult$capital_loss!=0,]
X.adult.capgain= X.adult[X.adult$capital_gain!=0,]
X.adult = rbind(X.adult.caploss, X.adult.capgain)

par(mfrow = c(1,2))
boxplot(X.adult$capital_gain, main="boxplot capital_gain")
boxplot(X.adult$capital_loss, main="boxplot capital_loss")

max <- quantile(X.adult$capital_gain,0.75, na.rm=TRUE) + (IQR(X.adult$capital_gain, na.rm=TRUE) * 3)
X.adult <- X.adult[X.adult$capital_gain < as.numeric(max),]

par(mfrow = c(1,2))
boxplot(X.adult$capital_gain, main="boxplot capital_gain")
boxplot(X.adult$capital_loss, main="boxplot capital_loss")


#################################################################################################
#################################################################################################
#################################################################################################

X.adult$workclass = as.character(X.adult$workclass)
X.adult$occupation = as.character(X.adult$occupation)
X.adult$native_country = as.character(X.adult$native_country)
X.adult$race = as.character(X.adult$race)
X.adult$marital_status = as.character(X.adult$marital_status)

X.adult$marital_status[X.adult$marital_status=="Never-married"] = "Never-Married"
X.adult$marital_status[X.adult$marital_status=="Married-AF-spouse"] = "Married"
X.adult$marital_status[X.adult$marital_status=="Married-civ-spouse"] = "Married"
X.adult$marital_status[X.adult$marital_status=="Married-spouse-absent"] = "Not-Married"
X.adult$marital_status[X.adult$marital_status=="Separated"] = "Not-Married"
X.adult$marital_status[X.adult$marital_status=="Divorced"] = "Not-Married"
X.adult$marital_status[X.adult$marital_status=="Widowed"] = "Widowed"

X.adult$native_country[X.adult$native_country=="Cambodia"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Canada"] = "British-Commonwealth"     
X.adult$native_country[X.adult$native_country=="China"] = "China"        
X.adult$native_country[X.adult$native_country=="Columbia"] = "South-America"     
X.adult$native_country[X.adult$native_country=="Cuba"] = "Other"         
X.adult$native_country[X.adult$native_country=="Dominican-Republic"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Ecuador"] = "South-America"      
X.adult$native_country[X.adult$native_country=="El-Salvador"] = "South-America"  
X.adult$native_country[X.adult$native_country=="England"] = "British-Commonwealth"
X.adult$native_country[X.adult$native_country=="France"] = "Euro_1"
X.adult$native_country[X.adult$native_country=="Germany"] = "Euro_1"
X.adult$native_country[X.adult$native_country=="Greece"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="Guatemala"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Haiti"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Holand-Netherlands"] = "Euro_1"
X.adult$native_country[X.adult$native_country=="Honduras"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Hong"] = "China"
X.adult$native_country[X.adult$native_country=="Hungary"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="India"] = "British-Commonwealth"
X.adult$native_country[X.adult$native_country=="Iran"] = "Other"
X.adult$native_country[X.adult$native_country=="Ireland"] = "British-Commonwealth"
X.adult$native_country[X.adult$native_country=="Italy"] = "Euro_1"
X.adult$native_country[X.adult$native_country=="Jamaica"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Japan"] = "Other"
X.adult$native_country[X.adult$native_country=="Laos"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Mexico"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Nicaragua"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Peru"] = "South-America"
X.adult$native_country[X.adult$native_country=="Philippines"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Poland"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="Portugal"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="Puerto-Rico"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Scotland"] = "British-Commonwealth"
X.adult$native_country[X.adult$native_country=="South"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="Taiwan"] = "China"
X.adult$native_country[X.adult$native_country=="Thailand"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Trinadad&amp;Tobago"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="United-States"] = "United-States"
X.adult$native_country[X.adult$native_country=="Vietnam"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Yugoslavia"] = "Euro_2"

X.adult$workclass = gsub("^Federal-gov","Federal-Govt",X.adult$workclass)
X.adult$workclass = gsub("^Local-gov","Other-Govt",X.adult$workclass)
X.adult$workclass = gsub("^State-gov","Other-Govt",X.adult$workclass)
X.adult$workclass = gsub("^Private","Private",X.adult$workclass)
X.adult$workclass = gsub("^Self-emp-inc","Self-Employed",X.adult$workclass)
X.adult$workclass = gsub("^Self-emp-not-inc","Self-Employed",X.adult$workclass)
X.adult$workclass = gsub("^Without-pay","Not-Working",X.adult$workclass)
X.adult$workclass = gsub("^Never-worked","Not-Working",X.adult$workclass)

X.adult$occupation = gsub("^Adm-clerical","Admin",X.adult$occupation)
X.adult$occupation = gsub("^Armed-Forces","Military",X.adult$occupation)
X.adult$occupation = gsub("^Craft-repair","Blue-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Exec-managerial","White-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Farming-fishing","Blue-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Handlers-cleaners","Blue-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Machine-op-inspct","Blue-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Other-service","Service",X.adult$occupation)
X.adult$occupation = gsub("^Priv-house-serv","Service",X.adult$occupation)
X.adult$occupation = gsub("^Prof-specialty","Professional",X.adult$occupation)
X.adult$occupation = gsub("^Protective-serv","Other-Occupations",X.adult$occupation)
X.adult$occupation = gsub("^Sales","Sales",X.adult$occupation)
X.adult$occupation = gsub("^Tech-support","Other-Occupations",X.adult$occupation)
X.adult$occupation = gsub("^Transport-moving","Blue-Collar",X.adult$occupation)

X.adult$race[X.adult$race=="White"] = "White"
X.adult$race[X.adult$race=="Black"] = "Black"
X.adult$race[X.adult$race=="Amer-Indian-Eskimo"] = "Amer-Indian"
X.adult$race[X.adult$race=="Asian-Pac-Islander"] = "Asian"
X.adult$race[X.adult$race=="Other"] = "Other"

is.na(X.adult) = X.adult=='?'
is.na(X.adult) = X.adult==' ?'
#X.adult = na.omit(X.adult)

X.adult$marital_status = factor(X.adult$marital_status)
X.adult$native_country = factor(X.adult$native_country)
X.adult$workclass = factor(X.adult$workclass)
X.adult$occupation = factor(X.adult$occupation)
X.adult$race = factor(X.adult$race)
X.adult$sex = factor(X.adult$sex)
X.adult$relationship = factor(X.adult$relationship)

par(mfrow = c(2,4))
barplot(prop.table(table(X.adult$workclass)), main = "barplot workclass")
barplot(prop.table(table(X.adult$marital_status)), main = "barplot marital_status")
barplot(prop.table(table(X.adult$occupation)), main = "barplot occupation")
barplot(prop.table(table(X.adult$relationship)), main = "barplot relationship")
barplot(prop.table(table(X.adult$race)), main = "barplot race")
barplot(prop.table(table(X.adult$sex)), main = "barplot sex")
barplot(prop.table(table(X.adult$native_country)), main = "barplot native_country")
barplot(prop.table(table(X.adult$income_class)), main = "barplot income_class")

#################################################################################################
#################################################################################################
#################################################################################################

# impute missing values
library(mice)
X.adult.mice <- mice(X.adult, MaxNWts = 10000)
X.adult.complete = complete(X.adult.mice)
#summary(X.adult.complete)

#################################################################################################
#################################################################################################
#################################################################################################

# visiualization
library(FactoMineR)
PCA = PCA(X = X.adult.complete, quali.sup = c(2,4,5,6,7,8,12,13))
biplot(PCA$quali.sup$coord[c("<=50K", ">50K"),],PCA$var$coord, xlim = c(-0.6,0.6), ylim = c(-0.6,0.6), main = "biplot of PCA")
abline(h=0,v=0,col="gray")
MCA = MCA(X = X.adult.complete[,c(2,4,5,6,7,8,12,13)], quali.sup = 8)

#################################################################################################
#################################################################################################
#################################################################################################

N = nrow(X.adult.complete)
split = 1:N
size = round(N*3/4)
split.training = sample(x = split, size = size, replace = FALSE)
split.testing = split[-split.training]
X.adult.training = X.adult.complete[split.training,]
X.adult.testing = X.adult.complete[split.testing, ]

#################################################################################################
#################################################################################################
#################################################################################################
# Naive Bayes
library(e1071)
model.naivebayes <- naiveBayes(income_class ~ ., data = X.adult.training)
pred.naivebayes <- predict(model.naivebayes, newdata=X.adult.testing)
(tab.naivebayes <- table(Truth=X.adult.testing$income_class, Preds=pred.naivebayes) )
(error.naivebayes = 1 - sum(tab.naivebayes[row(tab.naivebayes)==col(tab.naivebayes)])/sum(tab.naivebayes))

#################################################################################################
#################################################################################################
#################################################################################################
# Support Vector Machine
library(e1071)

# Fit an SVM with linear kernel
model.svm.linear <- svm(income_class ~ ., data = X.adult.training, type="C-classification", cost=1, kernel="linear", scale = FALSE)
pred.svm.linear <- predict(model.svm.linear, X.adult.testing)
(tab.svm.linear <- table(Truth=X.adult.testing$income_class, Preds=pred.svm.linear) )
(error.svm.linear = 1 - sum(tab.svm.linear[row(tab.svm.linear)==col(tab.svm.linear)])/sum(tab.svm.linear))

## Fit an SVM with quadratic kernel 
model.svm.poly2 <- svm(income_class ~ ., data = X.adult.training, type="C-classification", cost=1, kernel="polynomial", degree=2, coef0=1, scale = FALSE)
pred.svm.poly2 <- predict(model.svm.poly2, X.adult.testing)
(tab.svm.poly2 <- table(Truth=X.adult.testing$income_class, Preds=pred.svm.poly2) )
(error.svm.poly2 = 1 - sum(tab.svm.poly2[row(tab.svm.poly2)==col(tab.svm.poly2)])/sum(tab.svm.poly2))

## Fit an SVM with cubic kernel
model.svm.poly3 <- svm(income_class ~ ., data = X.adult.training, type="C-classification", cost=1, kernel="polynomial", degree=3, coef0=1, scale = FALSE)
pred.svm.poly3 <- predict(model.svm.poly3, X.adult.testing)
(tab.svm.poly3 <- table(Truth=X.adult.testing$income_class, Preds=pred.svm.poly3) )
(error.svm.poly3 = 1 - sum(tab.svm.poly3[row(tab.svm.poly3)==col(tab.svm.poly3)])/sum(tab.svm.poly3))

## Fit an RBF Gaussian kernel 
model.svm.rbf <- svm(income_class ~ ., data = X.adult.training, type="C-classification", cost=1, kernel="radial", scale = FALSE)
pred.svm.rbf <- predict(model.svm.rbf, X.adult.testing)
(tab.svm.rbf <- table(Truth=X.adult.testing$income_class, Preds=pred.svm.rbf) )
(error.svm.rbf = 1 - sum(tab.svm.rbf[row(tab.svm.rbf)==col(tab.svm.rbf)])/sum(tab.svm.rbf))



#################################################################################################
#################################################################################################
#################################################################################################

# Random Forest
library(randomForest)

(ntrees <- round(10^seq(1,3,by=0.2)))
rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0
ii <- 1
for (nt in ntrees)
{ 
  print(nt)
  
  model.X.random <- randomForest(income_class ~ ., X.adult.training, ntree=nt, proximity=FALSE)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.X.random$err.rate[nt,1]
  
  ii <- ii+1
}
rf.results
plot(rf.results)
lines(rf.results, pch=16)

model.randomforest <- randomForest(income_class ~ ., X.adult.training, ntree=300, proximity=FALSE)
pred.model.randomforest = predict(model.randomforest, X.adult.testing, type = "class")
(tab.model.randomforest <- table(Truth=X.adult.testing$income_class, Preds=pred.model.randomforest) )
(error.model.randomforest = 1 - sum(tab.model.randomforest[row(tab.model.randomforest)==col(tab.model.randomforest)])/sum(tab.model.randomforest))

#################################################################################################
#################################################################################################
#################################################################################################
# Neural Networks

library(nnet)
model.nnet <- nnet(income_class ~., data = X.adult.training, size=10, maxit=20000, decay=0.01)
pred.model.nnet = predict(model.nnet, X.adult.testing, type = "class")
(tab.model.nnet <- table(Truth=X.adult.testing$income_class, Preds=pred.model.nnet) )
(error.model.rbf = 1 - sum(tab.model.nnet[row(tab.model.nnet)==col(tab.model.nnet)])/sum(tab.model.nnet))

#################################################################################################
#################################################################################################
#################################################################################################

library(TunePareto) # for generateCVRuns()

k <- 10
CV.folds <- generateCVRuns(X.adult.complete$income_class, ntimes=1, nfold=k, stratified=TRUE)

## prepare the structure to store the partial results
cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k

## let us first compute the 10-fold CV errors
for (j in 1:k)
{
  # get VA data
  va <- unlist(CV.folds[[1]][[j]])
  # train on TR data
  model.randomforest <- randomForest(income_class ~ ., data = X.adult.complete[-va,], ntree=300, proximity=FALSE)
  # predict TR data
  pred.va = predict(model.randomforest)
  tab <- table(X.adult.complete[-va,]$income_class, pred.va)
  cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  # predict VA data
  pred.va <- predict(model.randomforest, newdata = X.adult.complete[va,])
  tab <- table(X.adult.complete[va,]$income_class, pred.va)
  cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  cv.results[j,"fold"] <- j
}
cv.results
plot(x = cv.results[,2], y = cv.results[,3], ylim = c(0,0.1), ylab = "error", xlab = "kfold", col = 2)
lines(x = cv.results[,2], y = cv.results[,3], col = 2)
par(new=T)
plot(x = cv.results[,2], y = cv.results[,4], ylim = c(0,0.1), ylab = "error",  xlab = "kfold", col = 3)
lines(x = cv.results[,2], y = cv.results[,4], col = 3)
legend("bottomright", c("TR error","VA error"), pch=20, col=c(2:3))

## What one really uses is the average of the last column
(VA.error <- mean(cv.results[,"VA error"]))

## Now a 95% CI around it
dev <- sqrt(VA.error*(1-VA.error)/N)*1.967
sprintf("(%f,%f)", VA.error-dev,VA.error+dev)

