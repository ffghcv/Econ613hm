#Nancun Li

library(AER)
setwd("D:/one/OneDrive/03MQFE/613-homework/��ҵ/A1")
datstu = read.csv("dat/datstu.csv")
datjss = read.csv("dat/datjss.csv")
datsss = read.csv("dat/datsss.csv")

#exercise 1
#number of student
nrow(datstu)

#number of school
schoolcode = datstu[,5:10]
temp1 = c(schoolcode[,1],schoolcode[,2],schoolcode[,3],schoolcode[,4],schoolcode[,5],schoolcode[,6])
length(unique(as.vector(temp1)))

length(unique(datsss[,3]))

#Number of programs
programcode = datstu[,11:16]
temp2 = cbind(programcode[,1],programcode[,2],programcode[,3],programcode[,4],programcode[,5],programcode[,6])
length(unique(as.vector(temp2)))

#Number of chooses
k = 1
sp = list()
while (k<=6){
  sp[[k]] = paste(schoolcode[,k],programcode[,k])
  k = k+1
}

length(unique(c(sp[[1]],sp[[2]],sp[[3]],sp[[4]],sp[[5]],sp[[6]])))

#Missing test score
sum(is.na(datstu[,2]))

#Apply to same school different program
temp2 = cbind(schoolcode[,1],schoolcode[,2],schoolcode[,3],schoolcode[,4],schoolcode[,5],schoolcode[,6])


find_same_school = function(te){
  m = nrow(te)
  n = ncol(te)
  A = c()
  for (i in 1:n-1){
    for (j in (i+1):n){
      x = te[,i] == te[,j]
      A = cbind(A, x)
    }
  }
  
  num = 0
  for (i in 1:m)(
    if (TRUE %in% A[i,]){
      num = num + 1
    }
  )
  return(num)
}

find_same_school(temp2)

#less than 6 chooses
temp = sp[[6]]
length(temp[schoolcode[,6] == "NA"])
length(temp[programcode[,6] == ""])






# exercise 2
temp = datstu$rankplace
table(temp)

temp1 = c(schoolcode[,1],schoolcode[,2],schoolcode[,3],schoolcode[,4],schoolcode[,5],schoolcode[,6])
temp2 = c(programcode[,1],programcode[,2],programcode[,3],programcode[,4],programcode[,5],programcode[,6])
datschoollevel = data.frame("schoolcode" = temp1, "programcode" = temp2)
datsss1 = unique(datsss[3:6])
temp3 = merge(datschoollevel, datsss1, by.x = "schoolcode", by.y = "schoolcode", all.x = T)
datsss_clean = unique(temp3[complete.cases(temp3[,4]),])
datsss_clean = cbind(datsss_clean, paste(datsss_clean[,1],datsss_clean[,2]))
colnames(datsss_clean) = c("schoolcode","programcode",'sssdistrict','ssslong','ssslat','sp')


no_na_score = datstu[complete.cases(datstu[,2]),]
find_sp_student = function(test1){
  A = data.frame()
  for (i in 1:6){
    a = cbind(test1[test1$rankplace==i,2],test1[test1$rankplace==i,(4+i)],test1[test1$rankplace==i,(10+i)])
    A = rbind(A,a)
  }
  A = cbind(A, paste(A[,2],A[,3]))
  return(A)
}
sp_student = find_sp_student(no_na_score)
names(sp_student) = c("score","s",'p','sp')
sp_student$score = as.numeric(sp_student$score)
library(dplyr)
planes = group_by(sp_student, sp)
sp_clean = data.frame(summarise(planes,cutoff = min(score),quality = mean(score), size = n()))


school_level = merge(datsss_clean, sp_clean, by = 'sp', all.x = T, all.y = T)

print(school_level[1:20,])






#exercise 3

dis_s = unique(datsss_clean[,3:5])
dis_j = unique(datjss[,2:4])


dis_js = c()
dis_js = cbind(dis_js, dis_s[,1])

for(i in 1:nrow(dis_j)){
  jsslong = dis_j[i,2]
  jsslat = dis_j[i,3]
  
  A = (69.172*(dis_s[,2]-jsslong)*cos(jsslat/57.3))^2
  B = (69.172*(dis_s[,3]-jsslat))^2
  S = sqrt(A+B)
  
  dis_js = cbind(dis_js, S)
  colnames(dis_js)[colnames(dis_js) == 'S'] = dis_j[i,1]
}

rownames(dis_js) = dis_js[,1]
dis_js = dis_js[,-1]

library(reshape2)
dis_js_long = melt(dis_js, id.vars = "V1")
colnames(dis_js_long) = c('sssdis','jssdis','distance')

print(dis_js_long[1:20,])


#exercise 4
get_rank_choice = function(i){
  te = cbind(no_na_score$score, paste(no_na_score[,(4+i)],no_na_score[,(10+i)]),no_na_score[,17])
  colnames(te) = c('score', 'sp', 'jssdis')
  te1 = merge(te, school_level, by.x = 'sp', by.y = 'sp', all.x=TRUE)
  te1 = cbind(te1, paste(te1$sssdistrict, te1$jssdis))
  colnames(te1)[12] = 'sj'
  
  te2 = cbind(dis_js_long, paste(dis_js_long$sssdis, dis_js_long$jssdis))
  colnames(te2)[4] = 'sj'
  
  te3 = merge(te1, te2, by.x = 'sj', by.y = 'sj', all.x = TRUE)
  te3$score = as.numeric(te3$score)
  te3$cutoff = as.numeric(te3$cutoff)
  te3$quality = as.numeric(te3$quality)
  te3$distance = as.numeric(te3$distance)
  return(te3)
}

report_1 = function(te3){
  a = c(mean(te3$cutoff,na.rm = TRUE), sd(te3$cutoff, na.rm = TRUE),mean(te3$quality, na.rm = TRUE),sd(te3$quality, na.rm = TRUE),mean(te3$distance, na.rm = TRUE),sd(te3$distance,na.rm = TRUE))
  return(a)
}

change_name = function(table1){
  rownames(table1) = c('rank1','rank2','rank3','rank4','rank5','rank6')
  colnames(table1) = c('avg_cutoff','sd_cutoff','avg_quality','sd_quality','avg_distance','sd_distance')
  return(table1)
}

table1 = c()
table_25 = c()
table_50 = c()
table_75 = c()
table_100 = c()

for (i in 1:6){
  te = get_rank_choice(i)
  rank = report_1(te)
  table1 = rbind(table1, rank)
  
  t_25 = te[te$score <= quantile(te$score, 0.25),]
  rank_25 = report_1(t_25)
  table_25 = rbind(table_25, rank_25)
  
  
  t_50 = te[te$score > quantile(te$score, 0.25) & te$score <= quantile(te$score, 0.5),]
  table_50 = rbind(table_50, report_1(t_50))
  
  t_75 = te[te$score > quantile(te$score, 0.5) & te$score <= quantile(te$score, 0.75),]
  table_75 = rbind(table_75, report_1(t_75))
  
  t_100 = te[te$score > quantile(te$score, 0.75),]
  table_100 = rbind(table_100, report_1(t_100))
}


table1 = change_name(table1)
print(table1)

table_25 = change_name(table_25)
print(table_25)

table_50 = change_name(table_50)
print(table_50)

table_75 = change_name(table_75)
print(table_75)

table_100 = change_name(table_100)
print(table_100)






# exercise 5
set.seed(121)
X1 = runif(10000,min=1,max=3)
X2 = rgamma(10000, shape = 3, scale = 2)
X3 = rbinom(10000,size = 1,prob = 0.3)
epi = rnorm(n=10000,mean=2,sd=1)
Y = 0.5 + 1.2*X1 - 0.9*X2 + 0.1*X3 + epi
ydum = rep(0,10000)
ydum[Y>mean(Y)] = 1


print(X1[1:20])
print(X2[1:20])
print(X3[1:20])
print(epi[1:20])
print(Y[1:20])
print(ydum[1:20])


#exercise 6
correlation = cor(Y,X1)
print(correlation)
#0.2 is quite different with 1.2

X = cbind(rep(1,10000), X1,X2,X3)
coeff = solve(t(X) %*% X) %*% t(X) %*% Y
print(coeff)


ee = Y - X %*% coeff
S_2 = 1/(10000-4)*sum(ee^2)
SE = sqrt(S_2 * solve(t(X) %*% X))
print(diag(SE))







#Exercise 7
est_linear = lm(ydum ~ X1 + X2+ X3)
summary(est_linear)

est_probit = glm(ydum ~ X1 + X2+ X3, family = binomial(link = "probit"))
summary(est_probit)

est_logit = glm(ydum ~ X1 + X2+ X3, family = binomial)
summary(est_logit)              


# Interpret and compare the estimated coeffcients:
# linear model: the coeff of X1 X2 X3 is 0.14, -0.1, 0.01�� for X1 X2 is significant, 
# For X3 is not significant. 0.14 means that one unit change in X1 lead to 0.14 unit
# change in Ydum
# 
# 
# probit model: the coeff of X1 X2 X3 is 1.2, -0.8, 0.09�� for X1 X2 is significant, 
# For X3 is not significant. 1.2 means that the effect of X1 makes it more likely
# to increase Ydum
# 
# 
# logit model: the coeff of X1 X2 X3 is 2.18, -1.59, 0.17�� for X1 X2 is significant, 
# For X3 is significant(in 95%). 2.18 means that the effect of X1 makes it more likely
# to increase Ydum
# 


#exercise8
data = data.frame(ydum, X)
est_probit = glm(ydum ~ X1 + X2+ X3, family = binomial(link = "probit"))
est_logit = glm(ydum ~ X1 + X2+ X3, family = binomial)
pdf1 = mean(dnorm(predict(est_probit, type = "link")))
pdf2 = mean(dlogis(predict(est_logit, type = "link")))

marg1 = pdf1*coef(est_probit)
marg2 = pdf2*coef(est_logit)

boot = 500
botvar = matrix(rep(NA,boot*length(coef(est_probit))), nrow=boot)
set.seed(121)
for(i in 1:boot){
  samp1 <- data[sample(1:dim(data)[1],replace=T,dim(data)[1]),]
  est_probit1 = glm(ydum ~ X1 + X2+ X3, family = binomial(link = "probit"), data = samp1)
  pdf1_1 = mean(dnorm(predict(est_probit1, type = "link")))
  botvar[i,] <- pdf1_1*coef(est_probit1)
}
res1 = cbind(marg1,apply(botvar,2,sd))
print(res1)


boot = 500
botvar = matrix(rep(NA,boot*length(coef(est_logit))), nrow=boot)
set.seed(121)
for(i in 1:boot){
  samp1 <- data[sample(1:dim(data)[1],replace=T,dim(data)[1]),]
  est_logit1 = glm(ydum ~ X1 + X2+ X3, family = binomial(link = "logit"), data = samp1)
  pdf1_1 = mean(dnorm(predict(est_logit1, type = "link")))
  botvar[i,] <- pdf1_1*coef(est_logit1)
}
res2 = cbind(marg2,apply(botvar,2,sd))
print(res2)

