X = matrix(c(1,0,0,1,1,1,1,0,0,1,1,1 ), nrow = 6, ncol = 2, byrow = TRUE)


Y = matrix(c(2,2,5,2,3,6), nrow = 6, ncol =1)

#Part a.i) Estimates of weights
beta_mtx<- solve(t(X)%*%X)%*%t(X)%*%Y

SSE <- t(Y- X%*%beta_mtx)%*% (Y- X%*%beta_mtx)

p <- ncol(X)
n<- nrow(X)
sigma_2 <- SSE/(n-p)

#Hat Matrix
H <- X%*%solve(t(X)%*%X)%*%t(X)

t_val<- qt(.85, df = n-p )

#Part a.ii) 70% CIs for balls A and B
A_wgt_low<- beta_mtx[1] + t_val*sigma_2*sqrt(H[1,1])
A_wgt_upp<- beta_mtx[1] - t_val*sigma_2*sqrt(H[1,1])


B_wgt_low<- beta_mtx[2] + t_val*sigma_2*sqrt(H[1,1])
B_wgt_upp <- beta_mtx[2] - t_val*sigma_2*sqrt(H[1,1])


#part a.iii) 70% PI for ball A

A_wgt_low_p<- beta_mtx[1] + t_val*sigma_2*sqrt(1+ H[1,1])
A_wgt_upp_p<- beta_mtx[1] - t_val*sigma_2*sqrt(1+H[1,1])


#part b) 
#Y2<- matrix(c(2,2.5,5.5), nrow = 3, ncol =1)
Y2<- matrix(c(2,3,4), nrow = 3, ncol =1)
X2 = matrix(c(1,0,0,1,1,1), nrow = 2, ncol = 3)
X2 <- t(X2)

beta_mtx2<- solve(t(X2)%*%X2)%*%t(X2)%*%Y2
SSE2 <- t(Y2- X2%*%beta_mtx2)%*% (Y2- X2%*%beta_mtx2)
SSE2

p2 <- ncol(X2)
n2<- nrow(X2)
sigma_22 <- SSE2/(n2-p2)
sigma_22
H2 <- X2%*%solve(t(X2)%*%X2)%*%t(X2)

t_val2<- qt(.85, df = n2-p2 )

#Part b.ii) 70% CIs for balls A and B
A_wgt_low2<- beta_mtx2[1] + t_val2*sqrt(sigma_22)*sqrt(H2[1,1])

A_wgt_upp2<- beta_mtx2[1] - t_val2*sqrt(sigma_22)*sqrt(H2[1,1])

A_wgt_low_p2<- beta_mtx2[1] + t_val2*sigma_22*sqrt(1+H2[1,1])
A_wgt_upp_p2<- beta_mtx2[1] - t_val2*sigma_22*sqrt(1+H2[1,1])



#part c



###Problem 2)

setwd("C:/Users/Anurag/Dropbox/GaTech/Sem 2/isye7406/homework/")



###Problem 3)

ziptrain <- read.table(file="http://www.isye.gatech.edu/~ymei/7406/Handouts/zip.train.csv",sep = ",")
ziptrain23 <- subset(ziptrain, ziptrain[,1]==2 | ziptrain[,1]==3)


dim(ziptrain23)
sum(ziptrain23[,1] == 2)
summary(ziptrain23)
round(cor(ziptrain23),2)
## To see the letter picture of a row by changing the row observation to a 16x16 matrix
rowindex = 5; ## You can try other "rowindex" values to see other rows
Xval = t(matrix(data.matrix(ziptrain23[,-1])[rowindex,],byrow=TRUE,16,16)[16:1,]);
image(Xval,col=gray(0:1),axes=FALSE) ## Also try "col=gray(0:32/32)"
ziptrain23[rowindex,1]; ## compare the image with the true Y (letter) value

ziptrain23_2 <- subset(ziptrain23, ziptrain23[,1] == 2)

ziptrain23_2 <- ziptrain23_2[,-1]


apply(ziptrain23_2,2, mean)/max()


images_digits_0_9[digit + 1, ]/max(images_digits_0_9[digit + 1, ]) * 255
sapply(ziptrain23_2, )
images_digits_0_9[digit + 1, ] <- apply(DATASET.train[DATASET.train[, 1] ==digit, -1], 2, sum)
?apply


ziptrain23_3<- subset(ziptrain23, ziptrain23[,1] == 3)
ziptrain23_3 <- ziptrain23_3[,-1]

#Finding an average of each number type


### linear Regression
mod1 <- lm( V1 ~ . , data= ziptrain23);
pred1.train <- predict.lm(mod1, ziptrain23[,-1]);
y1pred.train <- 2 + (pred1.train >= 2.5);
mean( y1pred.train != ziptrain23[,1]); #0.005759539



## KNN with kk nbhd
library(class);
kk <- 1; ## can be changed to other values such as 3,5,7,15
xnew <- ziptrain23[,-1];
ypred2.train <- knn(ziptrain23[,-1], xnew, ziptrain23[,1], k=kk);
mean( ypred2.train != ziptrain23[,1])


kk <- 2; ## can be changed to other values such as 3,5,7,15
xnew <- ziptrain23[,-1];
ypred2.train <- knn(ziptrain23[,-1], xnew, ziptrain23[,1], k=kk);
mean( ypred2.train != ziptrain23[,1])


kk <- 3; ## can be changed to other values such as 3,5,7,15
xnew <- ziptrain23[,-1];
ypred2.train <- knn(ziptrain23[,-1], xnew, ziptrain23[,1], k=kk);
mean( ypred2.train != ziptrain23[,1]) #0.005039597

kk <- 5; ## can be changed to other values such as 3,5,7,15
xnew <- ziptrain23[,-1];
ypred2.train <- knn(ziptrain23[,-1], xnew, ziptrain23[,1], k=kk);
mean( ypred2.train != ziptrain23[,1]) #0.005759539


kk <- 7; ## can be changed to other values such as 3,5,7,15
xnew <- ziptrain23[,-1];
ypred2.train <- knn(ziptrain23[,-1], xnew, ziptrain23[,1], k=kk);
mean( ypred2.train != ziptrain23[,1]) #0.006479482

kk <- 15; ## can be changed to other values such as 3,5,7,15
xnew <- ziptrain23[,-1];
ypred2.train <- knn(ziptrain23[,-1], xnew, ziptrain23[,1], k=kk);
mean( ypred2.train != ziptrain23[,1]) #0.009359251




ziptest <- read.table(file="http://www.isye.gatech.edu/~ymei/7406/Handouts/zip.test.csv", sep = ",");
ziptest23 <- subset(ziptest, ziptest[,1]==2 | ziptest[,1]==3);
## Testing error of KNN with kk nbhd
kk <- 1; ## can be changed to other values such as 3,5,7,15
xnew2 <- ziptest23[,-1];
ypred2.test <- knn(ziptrain23[,-1], xnew2, ziptrain23[,1], k=kk);
mean( ypred2.test != ziptest23[,1]) #0.02472527


