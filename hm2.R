#Nancun Li
#Exercise 1

library(bayesm)
data("margarine")
choiceprice <- as.data.frame(margarine$choicePrice)
demos <- as.data.frame(margarine$demos)


#Average and dispersion in choiceprice characteristics.
price <- as.data.frame(choiceprice[,3:12])
dispersion <- diag(var(price))
print(dispersion)
average=apply(as.matrix(choiceprice[,3:12]),2,mean)
print(average)
#Market share, and market share by choiceprice characteristics.
choiceprice$sales <-  ifelse(choiceprice$choice == 1, choiceprice$PPk_Stk,
                ifelse(choiceprice$choice == 2, choiceprice$PBB_Stk,
                  ifelse(choiceprice$choice == 3, choiceprice$PFl_Stk,
                  ifelse(choiceprice$choice == 4, choiceprice$PHse_Stk,
              ifelse(choiceprice$choice == 5, choiceprice$PGen_Stk,
                  ifelse(choiceprice$choice == 6, choiceprice$PImp_Stk,
                 ifelse(choiceprice$choice == 7, choiceprice$PSS_Tub,
                  ifelse(choiceprice$choice == 8, choiceprice$PPk_Tub,
                 ifelse(choiceprice$choice == 9, choiceprice$PFl_Tub,
                 choiceprice$PHse_Tub)))))))))

share <- cbind(sum(choiceprice$sales[choiceprice$choice == 1])/ sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 2]) / sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 3]) / sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 4]) / sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 5]) / sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 6]) / sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 7]) / sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 8]) / sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 9])/ sum(choiceprice$sales),
               sum(choiceprice$sales[choiceprice$choice == 10])/ sum(choiceprice$sales)
)
colnames(share) <- c("pk_stk", "bb_stk", "fl_stk", "hse_stk", "gen_stk", 
                     "imp_stk", "ss_tub", "pk_tub", "fl_tub" ,"hse_tub")

share

share_type <- c(sum(share[,1:6]), sum(share[,7:10]))
share_type <- rbind(c("stick", "tub") , share_type)
share_type

#Illustrate the mapping between observed attributes and choices.
merge(choiceprice, demos, by = "hhid", all.x = TRUE) 


#Exercise 2
#Conditional logit model
price <- as.data.frame(choiceprice[,2:12])
colnames(price) <- c("choice","P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")
prt = NULL
for(i in 1:10){
  prt <- cbind(prt, price[,i+1] - price[,1])
}
k1 <- ifelse(choiceprice$choice == "1", 1, 0)
k2 <- ifelse(choiceprice$choice == "2", 1, 0)
k3 <- ifelse(choiceprice$choice == "3", 1, 0)
k4 <- ifelse(choiceprice$choice == "4", 1, 0)
k5 <- ifelse(choiceprice$choice == "5", 1, 0)
k6 <- ifelse(choiceprice$choice == "6", 1, 0)
k7 <- ifelse(choiceprice$choice == "7", 1, 0)
k8 <- ifelse(choiceprice$choice == "8", 1, 0)
k9 <- ifelse(choiceprice$choice == "9", 1, 0)
k10 <- ifelse(choiceprice$choice == "10", 1, 0)
k1_10 <- cbind(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10)

conlogit <- function(theta, X, Y){
  temp = prop.table(exp(X*theta[1]+sapply(c(0,theta[2:10]),rep, 4470)),1)
  Y = -sum(k1_10*log(temp))
  return(Y)
}

optim(c(0,0,0,0,0,0,0,0,0,0), conlogit, X=prt, method = "BFGS")$par
m1 = as.matrix(optim(c(0,0,0,0,0,0,0,0,0,0), conlogit, X=prt, method = "BFGS")$par)
m1

#Exercise 3
#family income varies among individuals are not the alternatives, use Multinomial logit model
merge1 <- merge(demos[,c(1:2)],choiceprice[,c(1:2)], by="hhid")
income <- matrix(rep(merge1[,2],10),ncol=10)
MultLogit <- function(theta, X, Y){
  alpha = sapply(c(0,theta[10:18]),rep,4470)
  beta = sapply(c(0,theta[1:9]) ,rep,4470)
  temp = prop.table(exp(X*beta + alpha),1)
  Y = -sum(k1_10*log(temp))
  return(Y)
}

optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), MultLogit, X=income, method = "BFGS")$par


#exercise 4
a <- m1[2:10]
a <- sapply(a,rep,4470)
a2 <- matrix(c(0),nrow = 4470,ncol = 1)
a <- cbind(a2,a)

price2 <- choiceprice[,3:12]
c2 <- as.vector(price2[,2]-price2[,1])
c3 <- as.vector(price2[,3]-price2[,1])
c4 <- as.vector(price2[,4]-price2[,1])
c5 <- as.vector(price2[,5]-price2[,1])
c6 <- as.vector(price2[,6]-price2[,1])
c7 <- as.vector(price2[,7]-price2[,1])
c8 <- as.vector(price2[,8]-price2[,1])
c9 <- as.vector(price2[,9]-price2[,1])
c10 <- as.vector(price2[,10]-price2[,1])
c_all <- cbind(c2,c3,c4,c5,c6,c7,c8,c9,c10)
price3 <- matrix(c(0),nrow = 4470,ncol = 1)
price4 <- cbind(price3,c_all)
x = price4
vij <- x*m1[1]+a
total <- exp(vij[,1])+exp(vij[,2])+exp(vij[,3])+exp(vij[,4])+exp(vij[,5])+exp(vij[,6])+exp(vij[,7])+exp(vij[,8])+exp(vij[,9])+exp(vij[,10])
pij <- exp(vij)/total

dataset <- matrix(c(0),ncol=10,nrow=4470)
dataset <- cbind(as.matrix(choiceprice[,2]),dataset)
for(i in 1:4470){
  for(r in 1:10){
    dataset[i,r+1] <- ifelse(dataset[i,1]==r,1,0)
  }
}
dij <- dataset[,2:11]

pij_1 <- as.matrix(cbind(pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1]))
me_1 <- pij_1*(dij-pij)*m1[1,]
me_1 <- apply(me_1,2,mean)
pij_2 <- cbind(pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2])
me_2 <- pij_2*(dij-pij)*m1[1,]
me_2 <- apply(me_2,2,mean)
pij_3 <- cbind(pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3])
me_3 <- pij_3*(dij-pij)*m1[1,]
me_3 <- apply(me_3,2,mean)
pij_4 <- cbind(pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4])
me_4 <- pij_4*(dij-pij)*m1[1,]
me_4 <- apply(me_4,2,mean)
pij_5 <- cbind(pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5])
me_5 <- pij_5*(dij-pij)*m1[1,]
me_5 <- apply(me_5,2,mean)
pij_6 <- cbind(pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6])
me_6 <- pij_6*(dij-pij)*m1[1,]
me_6 <- apply(me_6,2,mean)
pij_7 <- cbind(pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7])
me_7 <- pij_7*(dij-pij)*m1[1,]
me_7 <- apply(me_7,2,mean)
pij_8 <- cbind(pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8])
me_8 <- pij_8*(dij-pij)*m1[1,]
me_8 <- apply(me_8,2,mean)
pij_9 <- cbind(pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9])
me_9 <- pij_9*(dij-pij)*m1[1,]
me_9 <- apply(me_9,2,mean)
pij_10 <- cbind(pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10])
me_10 <- pij_10*(dij-pij)*m1[1,]
me_10 <- apply(me_10,2,mean)
me_all <- rbind(me_1,me_2,me_3,me_4,me_5,me_6,me_7,me_8,me_9,me_10)
me_all
#For example, in me_1 ¨C v1:
#-0.00544 means that one unit increase in the price of product 1 will decrease 0.00544 in the probability to buy the product 1


theta2 <- as.vector(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), MultLogit, X=income, method = "BFGS")$par)
a2 = c(0,theta2[10:18])
alpha2ij = sapply(a2,rep,4470)
beta2 = c(0,theta2[1:9])
beta2ij = sapply(beta2,rep,4470) 
Vij2 = income*beta2ij + alpha2ij
Pij2 = prop.table(exp(Vij2),1)
beta_i_bar <- Pij2[,1]*beta2ij[,1]+Pij2[,2]*beta2ij[,2]+Pij2[,3]*beta2ij[,3]+Pij2[,4]*beta2ij[,4]+Pij2[,5]*beta2ij[,5]+Pij2[,6]*beta2ij[,6]+Pij2[,7]*beta2ij[,7]+Pij2[,8]*beta2ij[,8]+Pij2[,9]*beta2ij[,9]+Pij2[,10]*beta2ij[,10]
ME2 <- Pij2*(beta2ij - beta_i_bar)
as.matrix(colMeans(ME2))
#For example, -0.0010504 means that one unit change increase in family income will decrease 0.0010504 in the probability of choosing first 
#choice compared with other 9 choices

#exercise 5
mixlogit <- function(theta, X1, X2, Y){
  a = sapply(c(0,theta[11:19]),rep,4470)
  b = c(0,theta[2:10]) 
  b = sapply(b,rep,4470)
  Vij = X1*theta[1] + X2*b + a
  Pij = prop.table(exp(Vij),1)
  Y = -sum(dij*log(Pij))
  return(Y)
}
beta_f <- as.vector(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), mixlogit, X1=prt, X2=income, method = "BFGS")$par)
beta_f

#remove choose1
xxx = choiceprice
xxx[xxx==1] = NA
xxx1 = na.omit(xxx)

merge2 <- merge(demos[,c(1:2)],xxx1[,c(1:2)], by="hhid")
inc2 <- matrix(rep(merge2[,2],9),ncol=9)

price1 = as.data.frame(xxx1[,2:12])
price3 <- NULL
for(i in 1:9){
  price2 <- price1[,i+1] - price1[,1]
  price3 <- cbind(price3, price2)
}

pd <- subset(choiceprice[,2:12])
pd[pd==1] = NA
pd1 <- na.omit(pd)
dij2n <- ifelse(pd1$choice == "2", 1, 0)
dij3n <- ifelse(pd1$choice == "3", 1, 0)
dij4n <- ifelse(pd1$choice == "4", 1, 0)
dij5n <- ifelse(pd1$choice == "5", 1, 0)
dij6n <- ifelse(pd1$choice == "6", 1, 0)
dij7n <- ifelse(pd1$choice == "7", 1, 0)
dij8n <- ifelse(pd1$choice == "8", 1, 0)
dij9n <- ifelse(pd1$choice == "9", 1, 0)
dij10n <- ifelse(pd1$choice == "10", 1, 0)
dijnew <- cbind(dij2n,dij3n,dij4n,dij5n,dij6n,dij7n,dij8n,dij9n,dij10n)

mixlogit_movecho <- function(theta, X1, X2, Y){
  a = sapply(c(0,theta[10:17]),rep,2699)
  b = c(0,theta[2:9]) 
  b = sapply(b,rep,2699)
  Vij = X1*theta[1] + X2*b + a
  Pij = prop.table(exp(Vij),1)
  Y = -sum(dijnew*log(Pij))
  return(Y)
}

beta_r <- as.vector(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), mixlogit_movecho, X1=price3, X2=inc2, method = "BFGS")$par)
beta_r

Lr_f <- -mixlogit_movecho(beta_f[c(2:10,12:19)], X1=price3, X2=inc2) 
Lr_r <- -mixlogit_movecho(beta_r, X1=price3, X2=inc2) 
MTT <- -2*(Lr_f-Lr_r) 
MTT

# MTT > the critical value of chi_square, 
# reject the null hypothesis that the two results are the same.
























