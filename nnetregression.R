rm(list = ls())
##### Some R codes for take-home midterm of ISyE 7406
#####

### Read Training Data
midtermtrain <- read.table(file = "http://www2.isye.gatech.edu/~ymei/7406/midtermtrain.csv", sep=",")

dim(midtermtrain);
## dim=2911x202 
## The first two columns are X1 and X2 values, and the last 200 columns are the Y valus


## Testing Data

midtermtest  <- read.table(file = "http://www2.isye.gatech.edu/~ymei/7406/midtermtest.csv", sep=",");
colnames(midtermtest)<- c("X1", "X2")
dim(midtermtest)

### Some example plots for exploratory data analysis
### please add more exploratory analysis 
X1 <- midtermtrain[,1];
X2 <- midtermtrain[,2];

## note that muhat = E(Y) and Vhat = 100*Var(Y)
##
muhat <- apply(midtermtrain[,3:202], 1, mean);
Vhat  <- 100*apply(midtermtrain[,3:202], 1, var);


## we can plot 4 graphs in a single plot
par(mfrow = c(2, 2));
plot(X1, muhat, main = "X1 vs Mean"); 
plot(X2, muhat , main = "X2 vs Mean"); 
plot(X1, Vhat, main = "X1 vs Variance"); 
plot(X2, Vhat, main = "X2 vs Variance");


## let us reset the plot
dev.off()
## now plot the lines one by one for each fixed X2
data0 = data.frame(X1 = X1, X2=X2, muhat = muhat, Vhat = Vhat);
flag <- which(data0$X2 == 0);
plot(data0[flag,1], data0[flag, 3], type="l",
     xlim=range(data0$X1), ylim=range(data0$muhat), xlab="X1", ylab="muhat");
for (j in 1:40){
  flag <- which(data0$X2 == 0.1*j);
  lines(data0[flag,1], data0[flag, 3]);
}
## now plot the lines one by one for each fixed X1
flag2 <- which(data0$X1 == 0);
plot(data0[flag2,2], data0[flag2, 3], type="l",
     xlim=range(data0$X2), ylim=range(data0$muhat), xlab="X2", ylab="muhat");
for (j in 1:40){
  flag2 <- which(data0$X1 == 0.1*j);
  lines(data0[flag2,2], data0[flag2, 3]);
}

## now plot the var lines one by one for each fixed X2
flag <- which(data0$X2 == 0);
plot(data0[flag,1], data0[flag, 4], type="l",
     xlim=range(data0$X1), ylim=range(data0$Vhat), xlab="X1", ylab="Vhat");
for (j in 1:40){
  flag <- which(data0$X2 == 0.1*j);
  lines(data0[flag,1], data0[flag, 4]);
}

## now plot the var lines one by one for each fixed X1
flag2 <- which(data0$X1 == 0)
plot(data0[flag2,2], data0[flag2, 4], type="l",
     xlim=range(data0$X2), ylim=range(data0$Vhat), xlab="X2", ylab="Vhat");
for (j in 1:40){
  flag2 <- which(data0$X1 == 0.1*j);
  lines(data0[flag2,2], data0[flag2, 4]);
}


cor(data0)

#Step 1: Estimating models using an assigned split
###Splitting data into training and testing datasets

set.seed(123)
n = dim(data0)[1] ### total number of observations
n1 = round(n/10)


idx = sort(sample(1:n, n1))
data0.train <- data0[-idx,]
data0.test <- data0[idx,]

###Linear Regression: Linear-linear
model1.mu <-  lm(muhat ~ X1 + X2, data = data0.train)
summary(model1.mu)
plot(model1.mu) 

model1.V <-  lm(Vhat ~ X1+ X2, data = data0.train)
summary(model1.V)
plot(model1.V)

MSE1mod1.mu <-   mean((resid(model1.mu) )^2) #0.0002176589

MSE1mod1.V <-   mean((resid(model1.V) )^2) #0.0005801724

#### testing error 
head(data0.train)
head(data0.test)
summary(model1.mu)
pred1.mu <- predict(model1.mu, data0.test[,c(1,2)])

MSE2mod1.mu <-   mean((pred1.mu - data0.test[,3])^2) #0.0001754192

pred1.V <- predict(model1.V, data0.test[,c(1,2)])

MSE2mod1.V <-   mean((pred1.V - data0.test[,4])^2) #0.0007605581








###Linear Regression: Log-linear

model2.mu <-  lm(log(muhat) ~ X1 + X2, data = data0.train)
summary(model2.mu)
plot(model2.mu) 

model2.V <-  lm(log(Vhat) ~ X1+ X2, data = data0.train)
summary(model2.V)
plot(model2.V)

MSE1mod2.mu <-   mean((resid(model2.mu) )^2) #0.004144252

MSE1mod2.V <-   mean((resid(model2.V) )^2) #0.1341384

#### testing error 

pred2.mu <- predict(model2.mu, data0.test[,c(1,2)])

MSE2mod2.mu <-   mean((pred2.mu - log(data0.test[,3]) )^2) #0.004253205

pred2.V <- predict(model2.V, data0.test[,c(1,2)])

MSE2mod2.V <-   mean((pred2.V - log(data0.test[,4]))^2) #0.1418371





###Polynomial Regression

model3.mu <-  lm(muhat ~ poly(X1,2) + poly(X2,2), data = data0.train)
summary(model3.mu)
plot(model3.mu) 

model3.V <-  lm(Vhat ~ poly(X1,2) + poly(X2,2), data = data0.train)
summary(model3.V)
plot(model3.V)

MSE1mod3.mu <-   mean((resid(model3.mu) )^2) # 9.272783e-05

MSE1mod3.V <-   mean((resid(model3.V) )^2) #0.0004493027

#### testing error 

pred3.mu <- predict(model3.mu, data0.test[,c(1,2)])

MSE2mod3.mu <-   mean((pred3.mu - data0.test[,3] )^2) #7.337377e-05

pred3.V <- predict(model3.V, data0.test[,c(1,2)])

MSE2mod3.V <-   mean((pred3.V - data0.test[,4])^2) #0.0005542392















##Neural Network 

library(nnet)
#averaged nnets trained on entire training dataset
library(caret)

nnetfit.mu2.single <- train(muhat ~ (X1) + (X2), 
                    data0.train, 
                    method='nnet', 
                    linout=TRUE, 
                    preProc = c("center", "scale"),
                    trace = FALSE,
                    #Grid of tuning parameters to try:
                    tuneGrid=expand.grid(.size=c(1:10),.decay=c(0,0.001,0.1)),
                    maxit = 100)
nnetfit.mu2.single

#size = 9, decay = 0: 1st run
#size = 9, decay = 0: 2nd run


nnetfit.mu2  <- avNNet(muhat ~ (X1) + (X2), 
                            data0.train, 
                            size = 9,
                            decay =0,
                            repeats = 5, 
                            method='nnet', 
                            preProc = c("center", "scale"),
                            linout=TRUE, 
                            trace = FALSE,
                            maxit = 500)


nnetfit.V2.single <- train(Vhat ~ X1 + X2, 
                   data0.train , 
                   method='nnet', 
                   preProc = c("center", "scale"),
                   linout=TRUE, 
                   trace = FALSE,
                   #Grid of tuning parameters to try:
                   tuneGrid=expand.grid(.size=c(5:10),.decay=c(0,0.001,0.1)),
                   maxit = 100)
nnetfit.V2.single
#size = 8 and decay = 0 : 1st run
#size = 8 and decay = 0: 2nd run

nnetfit.V2  <- avNNet(Vhat ~ (X1) + (X2), 
                       data0.train, 
                       size = 8,
                       decay =0.0,
                       repeats = 5, 
                       method='nnet', 
                       preProc = c("center", "scale"),
                       linout=TRUE, 
                       trace = FALSE,
                       maxit = 500)


#Out of sample MSE


pred4.mu <- predict(nnetfit.mu2, (data0.test[,c(1,2)]))
MSE2mod4.mu <-   mean((pred4.mu - data0.test[,3] )^2)
sqrt(MSE2mod4.mu)
pred4.V <- predict(nnetfit.V2, (data0.test[,c(1,2)]) )
MSE2mod4.V <-   mean((pred4.V - data0.test[,4] )^2) #[1] 1.668107e-07
sqrt(MSE2mod4.V)

#Actual test set predictions

pred4.mu2 <- predict(nnetfit.mu2, (midtermtest[,c(1,2)])) 

pred4.V2 <- predict(nnetfit.V2, (midtermtest[,c(1,2)]))

output <- as.data.frame(cbind(midtermtest, round(pred4.mu2,6) , round(pred4.V2,6) ))

write.table(output, "nnetoutput.csv",sep = ",",row.names  = FALSE, col.names = FALSE)



