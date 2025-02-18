library(dplyr)
library(forecast)
library(dyn)
library(zoo)
library(ggplot2)
library(leaps)
library(glmnet)
library(leaps)

project=read.csv("data.csv")
str(project)


testdata=project[372:731,]
testdata
testdata1<- do.call(data.frame, replicate(6, rep(FALSE, 360), simplify=FALSE)) # craete data frame 
colnames(testdata1) <- c("testx1", "testx2", "testx3","predicted_y","actual_y","difference")
testdata1$testx1=testdata$G0M910
testdata1$testx2=testdata$A0M005
testdata1$testx3=testdata$U0M129



model1 <- do.call(data.frame, replicate(4, rep(FALSE, 373), simplify=FALSE)) # craete data frame 
colnames(model1) <- c("intercept_model1", "G0M910_model1", "A0M005_model1", "U0M129_model1")
model1$intercept_model1=b0
model1$G0M910_model1=b1
model1$A0M005_model1=b2
model1$U0M129_model1=b3

for(i in 1:360){
    j <- i + 372
    model1 = lm(A0M043[2:j] ~ G0M910[2:j-1] + A0M005[2:j-1] + U0M129[2:j-1], data = project)
    model_summ1=summary(model1)
    
    b0=model1$coefficients[1]
    b1=model1$coefficients[2]
    b2=model1$coefficients[3]
    b3=model1$coefficients[4]
}


for(i in 372:720){
predicted_y=b0+b1*testdata1$testx1+b2*testdata1$testx2+b3*testdata1$testx3
}


## predict(model1$fitted.value)
aaa=rep(0,360)
w=diff(project$A0M043)

for(i in 1:360){
  j <- i + 372
  model2 = lm(w[3:j-2] ~ G0M910[3:j-2] + A0M005[3:j-2] + U0M129[3:j-2], data = project)
  model_summ2=summary(model2)
  
  b00=model2$coefficients[1]
  b11=model2$coefficients[2]
  b22=model2$coefficients[3]
  b33=model2$coefficients[4]
  b44=model2$coefficients[5]
  aaa[i]=b00+b11*testdata1$testx1[i]+b22*testdata1$testx2[i]+b33*testdata1$testx3[i]
}


testdata1$predicted_y= predicted_y
testdata1$actual_y=testdata$A0M043
testdata1$difference=aaa

MSE=mean((aaa-w[373:732-1])^2)
MSE





a=rep(0,360)
for(i in 1:360){
  j <- i + 372
  model3 = lm(w[8:j-1] ~ 
                G0M910[8:j-1] + A0M005[8:j-1] + U0M129[8:j-1]+ w[8:j-2]+
                G0M910[8:j-2] + A0M005[8:j-2] + U0M129[8:j-2]+ w[8:j-3]+
                G0M910[8:j-3] + A0M005[8:j-3] + U0M129[8:j-3]+ w[8:j-4]+
                G0M910[8:j-4] + A0M005[8:j-4] + U0M129[8:j-4]+ w[8:j-5]+
                G0M910[8:j-5] + A0M005[8:j-5] + U0M129[8:j-5]+ w[8:j-6]+
                G0M910[8:j-6] + A0M005[8:j-6] + U0M129[8:j-6]+ w[8:j-7], data = project)
  
  model_summ3=summary(model3)
  
  b0_3=model3$coefficients[1]
  b1_3=model3$coefficients[2]
  b2_3=model3$coefficients[3]
  b3_3=model3$coefficients[4]
  b4_3=model3$coefficients[5]
  
  b5=model3$coefficients[6]
  b6=model3$coefficients[7]
  b7=model3$coefficients[8]
  b8=model3$coefficients[9]
  b9=model3$coefficients[10]
  b10=model3$coefficients[11]
  b11=model3$coefficients[12]
  b12=model3$coefficients[13]
  b13=model3$coefficients[14]
  
  b14=model3$coefficients[15]
  b15=model3$coefficients[16]
  b16=model3$coefficients[17]
  b17=model3$coefficients[18]
  b18=model3$coefficients[19]
  b19=model3$coefficients[20]
  b20=model3$coefficients[21]
  b21=model3$coefficients[22]
  b22=model3$coefficients[23]
  b23=model3$coefficients[24]
  b24=model3$coefficients[25]

  a[i]=b0_3+b1_3*project$G0M910[j]+b2_3*project$A0M005[j]+b3_3*project$U0M129[j]+
    b4_3*w[j-1]+b5*project$G0M910[j-1]+b6*project$A0M005[j-1]+
    b7*project$U0M129[j-1]+b8*w[j-2]+b9*project$G0M910[j-2]+
    b10*project$A0M005[j-2]+b11*project$U0M129[j-2]+b12*w[j-3]+
    b13*project$G0M910[j-3]+b14*project$A0M005[j-3]+b15*project$U0M129[j-3]+
    b16*w[j-4]+b17*project$G0M910[j-4]+b18*project$A0M005[j-4]+
    b19*project$U0M129[j-4]+b20*w[j-5]+b21*project$G0M910[j-5]+
    b22*project$A0M005[j-5]+b23*project$U0M129[j-5]+b24*w[j-6]
     
}
# a[i]=predict(model3$fitted.values)
a
testdata3$predict_diff=a
testdata3$actual_y=testdata$A0M043
MSE=mean((a-w[373:732])^2)
MSE






# 30year=1959/01--1989/12 total 60 unit
model3 <- do.call(data.frame, replicate(5, rep(FALSE, 60), simplify=FALSE)) # craete data frame 
colnames(model3) <- c("intercept_model3", "G0M910_model3", "A0M005_model3", "U0M129_model3","A0M043_model3")
model3$intercept_model3=b0_3
model3$G0M910_model3=b1_3
model3$A0M005_model3=b2_3
model3$U0M129_model3=b3_3
model3$A0M043_model3=b4_3


# 1990/01--2019/12 total 60 unit
testdata=project[372:731,]
testdata
testdata3<- do.call(data.frame, replicate(5, rep(FALSE, 360), simplify=FALSE)) # craete data frame 
colnames(testdata3) <- c("testx1", "testx2", "testx3","testx4","predict_diff")
testdata3$testx1=testdata$G0M910
testdata3$testx2=testdata$A0M005
testdata3$testx3=testdata$U0M129
testdata3$testx4=testdata$A0M043



m3=regsubsets(w[8:j-1] ~ 
                G0M910[8:j-1] + A0M005[8:j-1] + U0M129[8:j-1]+ w[8:j-2]+
                G0M910[8:j-2] + A0M005[8:j-2] + U0M129[8:j-2]+ w[8:j-3]+
                G0M910[8:j-3] + A0M005[8:j-3] + U0M129[8:j-3]+ w[8:j-4]+
                G0M910[8:j-4] + A0M005[8:j-4] + U0M129[8:j-4]+ w[8:j-5]+
                G0M910[8:j-5] + A0M005[8:j-5] + U0M129[8:j-5]+ w[8:j-6]+
                G0M910[8:j-6] + A0M005[8:j-6] + U0M129[8:j-6]+ w[8:j-7],project)
summary(m3)

m3.full=regsubsets(w[8:j-1] ~ 
                     G0M910[8:j-1] + A0M005[8:j-1] + U0M129[8:j-1]+ w[8:j-2]+
                     G0M910[8:j-2] + A0M005[8:j-2] + U0M129[8:j-2]+ w[8:j-3]+
                     G0M910[8:j-3] + A0M005[8:j-3] + U0M129[8:j-3]+ w[8:j-4]+
                     G0M910[8:j-4] + A0M005[8:j-4] + U0M129[8:j-4]+ w[8:j-5]+
                     G0M910[8:j-5] + A0M005[8:j-5] + U0M129[8:j-5]+ w[8:j-6]+
                     G0M910[8:j-6] + A0M005[8:j-6] + U0M129[8:j-6]+ w[8:j-7],project,nvmax = 24)
m3.summary=summary(me.full)
names(me.summary)


m3.summary$rsq

par (mfrow = c(2, 2))
plot (m3.summary$rss , xlab = " Number of Variables ",
        ylab = " RSS ", type = "l") 
plot (m3.summary$adjr2 , xlab = " Number of Variables ",
        ylab = " Adjusted RSq ", type = "l")
which.max (m3.summary$adjr2)



m3.fwd=regsubsets(w[8:j-1] ~ 
                    G0M910[8:j-1] + A0M005[8:j-1] + U0M129[8:j-1]+ w[8:j-2]+
                    G0M910[8:j-2] + A0M005[8:j-2] + U0M129[8:j-2]+ w[8:j-3]+
                    G0M910[8:j-3] + A0M005[8:j-3] + U0M129[8:j-3]+ w[8:j-4]+
                    G0M910[8:j-4] + A0M005[8:j-4] + U0M129[8:j-4]+ w[8:j-5]+
                    G0M910[8:j-5] + A0M005[8:j-5] + U0M129[8:j-5]+ w[8:j-6]+
                    G0M910[8:j-6] + A0M005[8:j-6] + U0M129[8:j-6]+ w[8:j-7],project,nvmax = 24,method = "forward")
summary(m3.fwd)

m3.bwd=regsubsets(w[8:j-1] ~ 
                    G0M910[8:j-1] + A0M005[8:j-1] + U0M129[8:j-1]+ w[8:j-2]+
                    G0M910[8:j-2] + A0M005[8:j-2] + U0M129[8:j-2]+ w[8:j-3]+
                    G0M910[8:j-3] + A0M005[8:j-3] + U0M129[8:j-3]+ w[8:j-4]+
                    G0M910[8:j-4] + A0M005[8:j-4] + U0M129[8:j-4]+ w[8:j-5]+
                    G0M910[8:j-5] + A0M005[8:j-5] + U0M129[8:j-5]+ w[8:j-6]+
                    G0M910[8:j-6] + A0M005[8:j-6] + U0M129[8:j-6]+ w[8:j-7],project,nvmax = 24,method = "backward")
summary(m3.bwd)

coef(m3.full,12)
coef(m3.fwd,12)
coef(m3.bwd,12)


j <- i + 372
x=model.matrix(w[8:j-1] ~ 
                 G0M910[8:j-1] + A0M005[8:j-1] + U0M129[8:j-1]+ w[8:j-2]+
                 G0M910[8:j-2] + A0M005[8:j-2] + U0M129[8:j-2]+ w[8:j-3]+
                 G0M910[8:j-3] + A0M005[8:j-3] + U0M129[8:j-3]+ w[8:j-4]+
                 G0M910[8:j-4] + A0M005[8:j-4] + U0M129[8:j-4]+ w[8:j-5]+
                 G0M910[8:j-5] + A0M005[8:j-5] + U0M129[8:j-5]+ w[8:j-6]+
                 G0M910[8:j-6] + A0M005[8:j-6] + U0M129[8:j-6]+ w[8:j-7], data = project)
y=w[8:j-1]


cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

a=rep(0,360)
for(i in 1:360){
  j <- i + 365
  lasso.mod=glmnet(x,y,alpha = 1,lambda = bestlam)
  lasso.pred=predict(lasso.mod,s=bestlam,newx = x)
}
plot(lasso.mod)
mean((lasso.pred-y)^2)

a=rep(0,360)
for(i in 1:360){
  j <- i + 372
  ridge.mod=glmnet(x,y,alpha = 0,lambda = bestlam)
  ridge.pred=predict(ridge.mod,s=bestlam,newx = x)
}
plot(ridge.mod)
mean((ridge.pred-y)^2)



