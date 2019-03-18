setwd("~/Desktop/BA Project")
data = read.csv('final_data_mean.csv')
head(data)
data = data[,-1]
data = data[,-33]
head(data)
names(data)

a = c(12:23)
for (i in a){
  data[,i] =  as.numeric(sub('\\$','',as.character(data[,i])))
}
data = data[data$Original.Principal.Amount != 0,]
data = data[data$Disbursed.Amount > 0,]
data["log.principal.amount"] <- NA
data$log.principal.amount = log(data$Original.Principal.Amount)
data["log.disbursed.amount"] <- NA
data$log.disbursed.amount = log(data$Disbursed.Amount)
data["log.GDP"] <- NA
data$log.GDP = log(data$GDP..constant.2010.US..)

names(data)

numeric_data = data
names(numeric_data)
numeric_data = numeric_data[,-c(1,2,3,4,6,7,9,10,11,18,22,24,25,26,27,28,29,30)]
numeric_data$Country = as.factor(numeric_data$Country)

## Finding out correlation
cormat <- round(cor(numeric_data[,-1]),2)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)
ggplot(data = melted_cormat,aes(x = Var1, y = Var2, fill = value)) + geom_tile() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) #correlation plot

## Choosing Features and applying LASSO Regression
names(numeric_data)
num_data = numeric_data 
numeric_data = numeric_data[,-c(3,6,27)]
library(glmnet)
set.seed(1)
train = sample(1:nrow(numeric_data),0.75*nrow(numeric_data))
test = -train
x=model.matrix(numeric_data$Duration~.,data = numeric_data[,-12])
y=numeric_data$Duration
k=5
grid=c(0,0.001,0.01,0.1,1,10,100,1000)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,lambda=grid,nfolds=10)
bestlam=cv.out$lambda.min
bestlam
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
lasso_error=mean((lasso.pred-y[test])^2)
mean((lasso.pred-y[test])^2) #Error Rate for prediction
par(mfrow=c(1,2))
plot(cv.out$glmnet.fit , "norm", label = TRUE) #checking which coefficients go to zero
plot(cv.out$glmnet.fit, "lambda", label=TRUE) #checking which coefficients go to zero 

## Choosing Features and applying Ridge Regression
library(glmnet)
set.seed(1)
#train1 = sample(1:nrow(numeric_data),0.75*nrow(numeric_data))
#test1 = -train1
x1 = model.matrix(numeric_data$Duration~.,data = numeric_data[,-12])
y1 = numeric_data$Duration
k=5
grid=c(0,0.001,0.01,0.1,1,10,100,1000)
cv.out1=cv.glmnet(x1[train,],y1[train],alpha=0,lambda=grid,nfolds=10)
bestlam1=cv.out1$lambda.min
bestlam1
ridge.mod = glmnet(x1[train,], y1[train], alpha=0, lambda=bestlam1)
ridge.pred=predict(ridge.mod, s=bestlam1, newx=x1[test,])
ridge_error=mean((ridge.pred-y1[test])^2)
mean((ridge.pred-y1[test])^2) #Error Rate for prediction
hist(ridge.pred-y1[test], main="Histogram of Residuals", xlab="Residual",ylab="Frequency",yaxs="i",xaxs="i")
par(mfrow=c(1,2))
plot(cv.out1$glmnet.fit , "norm", label = TRUE) #checking which coefficients go to zero
plot(cv.out1$glmnet.fit, "lambda", label=TRUE) #checking which coefficients go to zero

## KNN to predict Duration using k nearest neighbours
names(numeric_data)
data.train = numeric_data[train,]
data.test = numeric_data[test,]
library(class)
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
data_norm <- as.data.frame(lapply(data.train[,-c(1,12)], normalize))
data_norm1 <- as.data.frame(lapply(data.test[,-c(1,12)], normalize))
err_mean = c()
for (i in 1:25){
  temp_err <- knn.cv(train = data_norm, cl = data.train[,12], k=i)
  err_mean[i]=mean(temp_err!=data.train[,12])
}
best_k=which.min(err_mean)
print(best_k)
temp_avg <- knn(train=data_norm, test=data_norm1, cl=data.train[,12], k=best_k)
temp = as.numeric(levels(temp_avg))[temp_avg]
knn_error=mean((temp-data.test[,12])^2)
avg_err=mean((temp-data.test[,12])^2) #Error Rate for prediction
print(avg_err)
hist(temp-data.test[,12], main="Histogram of Residuals", xlab="Residual",ylab="Frequency",yaxs="i",xaxs="i")

## PCA + Linear Regression to predict Duration using 6 components
library(pls)
names(numeric_data)
y2 = numeric_data$Duration
y2.test = y2[test]
pls.fit = plsr(Duration~., data=numeric_data[,-1], subset=train, scale=T, validation ="CV", nfolds=5, segments=5)
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred = predict(pls.fit, numeric_data[test,], ncomp=6)
plsr_error=mean((pls.pred - y2.test)^2)
mean((pls.pred - y2.test)^2) #Error Rate for prediction

## Subest selection and Linear Regression using 10 features
library(ISLR)
library(leaps)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  pred = mat[,xvars]%*%coefi
  return(pred)
}


regfit.best = regsubsets(Duration~., data=data.train, nvmax=10, really.big = T, method="forward")
k=5
set.seed(1)
folds = sample(1:k,nrow(data.train),replace=TRUE)
cv.errors = matrix(NA,k,8, dimnames=list(NULL, paste(1:8)))
for(j in 1:k){
  best.fit=regsubsets(Duration~., data=data.train[folds!=j,], nvmax=10, really.big = T, method="forward")
  for(i in 1:8){
    pred = predict.regsubsets(best.fit,data.train[folds==j,],id=i)
    cv.errors[j,i] = mean((data.train$Duration[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors, 2 ,mean)
mean.cv.errors

best.model = which.min(mean.cv.errors)
best.model
regfit.full = regsubsets(Duration~., data=data.train, really.big = T, nvmax=10, method = "forward")
summary(regfit.full)

pred=predict.regsubsets(regfit.full, data.test, best.model)
actual = data.test$Duration
mean((actual - pred)^2) #Error Rate for prediction

## Linear Regression to predict duration based on
## GDP per capita, GDP per capita annual growth, inflation and interaction terms
data1.mat = model.matrix(~(GDP.per.capita.growth..annual...+GDP.per.capita..current.US..+Inflation..consumer.prices..annual...)^2, numeric_data)
matrix1 <- as.data.frame(data1.mat)
matrix1 = matrix1[,-1]
matrix1 <- as.data.frame(lapply(matrix1, normalize))
matrix1['Duration'] = "NA"
matrix1$Duration = numeric_data$Duration
fit1 = lm(Duration~. , data = matrix1)
summary(fit1) 

plotting = c(lasso_error, ridge_error, knn_error, plsr_error)
barplot(plotting, xlab="Models",ylab="Error Rate",names.arg=c("LASSO","RIDGE","KNN","PLSR"))
