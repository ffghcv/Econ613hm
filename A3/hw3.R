#homework 3
library(readr)
library(tidyr)

#Exercise 1
crime_long <- read_csv("D:/one/OneDrive/03MQFE/613-homework/hw/A3/crime_long.csv")
population <- read_csv("D:/one/OneDrive/03MQFE/613-homework/hw/A3/population.csv")
officers <- read_csv("D:/one/OneDrive/03MQFE/613-homework/hw/A3/officers.csv")







#Exercise 2
#2.1 Calculate total crime per month and plot the time series of crime.
crime_month = aggregate(crime_long$crimes, by=list(crime_long$crime_month), FUN=sum)
plot(crime_month[,2]~crime_month[,1],xaxt = "n", type = "l")

#2.2 Merge the two datasets by districts-units and period.
temp = unite(population,'mu',month, district,sep='sss')
ttmp = unite(crime_long,'mu',crime_month, district,sep='sss')
m1 = merge(temp, ttmp, by='mu')
m2 = separate(m1, mu, c('month','unit'), sep='sss',remove = FALSE)
m2[,2] = as.Date(m2[,2], format = '%Y-%m-%d')


#2.3 Construct a panel data of unit over time with the following variables
t1 = aggregate(m2[,c('crimes')], by=list(m2$mu), FUN=sum)
t2 = aggregate(m2[,c('tot_pop','tot_white','tot_black','tot_hisp')], by=list(m2$mu), FUN=max)
t3 = merge(t1,t2,by='Group.1',all = TRUE)

m2_v = m2[m2$crime_type == 'violent',]
t4 = aggregate(m2_v[,c('crimes')], by=list(m2_v$mu), FUN=sum)

m2_p = m2[m2$crime_type == 'property',]
t5 =aggregate(m2_p[,c('crimes')], by=list(m2_p$mu), FUN=sum)

t6 = aggregate(m2[,c('p50_inc')], by=list(m2$mu), FUN=median)

m3 = cbind(t3$Group.1, t3$x/t3$tot_pop, t4$x/t3$tot_pop, t5$x/t3$tot_pop, t6$x, t3[4:6]/t3$tot_pop)
colnames(m3) = c('mu','tc','vc','pc','inc','white','black','hisp')
m3 = separate(m3, mu, c('month','unit'), sep='sss',remove = FALSE)
m3[,2] = as.Date(m3[,2], format = '%Y-%m-%d')




#Exercise 3
#Panel Data: Introduction
#with intercept
officers_u = unite(officers,'mu',month, unit,sep='sss')
m4 = merge(m3,officers_u,by='mu')

X = cbind(1,m4$tenure,m4$tc,m4$inc,m4$black,m4$hisp,m4$white)
beta_hat = solve(t(X)%*%X)%*%t(X)%*%m4$arrest
beta_hat

#check by lm
l1 = lm(arrest ~ tenure + tc + inc + black + hisp + white, data = m4)
summary(l1)

#without intercept
X = cbind(m4$tenure,m4$tc,m4$inc,m4$black,m4$hisp,m4$white)
beta_hat = solve(t(X)%*%X)%*%t(X)%*%m4$arrest
beta_hat

#check by lm
l2 = lm(arrest ~ tenure + tc + inc + black + hisp + white-1, data = m4)
summary(l2)






#Exercise 4
#Panel Data: More controls
val1 = unique(m4[,3])
temp = matrix(data=0, nrow = nrow(m4), ncol = length(val1))
for (i in 1:length(val1)){
  temp[m4[,3]==val1[i],i] = 1
}
colnames(temp) = val1
m5 = cbind(m4, temp)


val2 = unique(m4[,2])
temp = matrix(data=0, nrow = nrow(m4), ncol = length(val2))
for (i in 1:length(val2)){
  temp[m4[,2]==val2[i],i] = 1
}
colnames(temp) = as.character(val2)
m5 = cbind(m5, temp)
xx = c('mu','month','unit','vc','pc','NUID','2007-01-01')
m5 = m5[,!names(m5) %in% xx]
l3 = lm(arrest ~. - 1, data = m5)
summary(l3)


#check
l3 = lm(arrest ~ tenure + tc + inc + black + hisp + white + factor(unit)+factor(month) -1, data = m4)
summary(l3)









#Exercise 5
#between estimator
m4_between1 = aggregate(m4, list(m4$NUID, m4$unit), mean)
m4_between1$unit = m4_between1$Group.2

lm_between = lm(arrest ~ tenure + tc + inc + black + hisp + white + factor(unit), data=m4_between1)
summary(lm_between)

#within estimator
mean_between = m4_between1[,c('NUID','arrest','tenure','tc','inc','black','hisp','white')]
colnames(mean_between) = c('NUID','arrest_mean','tenure_mean','tc_mean','inc_mean','black_mean','hisp_mean','white_mean')
m4_within = merge(m4,mean_between,by='NUID')
m4_within$arrest_within = m4_within$arrest - m4_within$arrest_mean
m4_within$tenure_within = m4_within$tenure - m4_within$tenure_mean
m4_within$tc_within = m4_within$tc - m4_within$tc_mean
m4_within$inc_within = m4_within$inc - m4_within$inc_mean
m4_within$black_within = m4_within$black - m4_within$black_mean
m4_within$hisp_within = m4_within$hisp - m4_within$hisp_mean
m4_within$white_within = m4_within$white - m4_within$white_mean

lm_within = lm(arrest_within ~ tenure_within + tc_within + inc_within + black_within + hisp_within + white_within + factor(unit)+factor(month) - 1 , data=m4_within)
summary(lm_within)

#first diff estimator
m4_fd = m4[order(m4$NUID, m4$month),]
m4_fd1 = as.data.frame(lapply(m4_fd,as.numeric))
m4_fd1$month = m4_fd$month

m4_fd2 = m4_fd1[-1,] - m4_fd1[-nrow(m4_fd1),]
m4_fd2 = rbind(c(0,1,0,0,0), m4_fd2)
m4_fd3 = m4_fd2[m4_fd2$NUID == 0,]
lm_fd = lm(arrest ~ tenure + tc + inc + black + hisp + white + factor(unit), data=m4_fd3)
summary(lm_fd)


#compare three beta
#between:1.064e-05
#within:1.088e-04
#first difference:1.355e-04
#The between estimator is smaller than the other two. 


#GMM in one-step
X = m5[,-7]
Y = m5[,7]

X = as.matrix(X)
beta = solve(t(X) %*% X %*% t(X) %*% X, tol = 1e-100) %*%t(X) %*% X %*% t(X) %*% Y

colnames(m5)[1:6]
b = as.data.frame(beta[1:6])
b = t(b)
colnames(b) = colnames(m5)[1:6]
b
