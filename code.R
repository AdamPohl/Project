# setwd("C:/Users/Timothy/Documents/GitHub/Project")

names =  c("id", "ccf", "age", "sex", "painloc", "painexer", "relrest", "pncaden", "cp", "trestbps", "htn", "chol", "smoke", "cigs", "years", "fbs", "dm", "famhist", "restecg", "ekgmo", "ekgday", "ekgyr", "dig", "prop", "nitr", "pro", "diuretic", "proto", "thaldur", "thaltime", "met", "thalach", "thalrest", "tpeakbps", "tpeakbpd", "dummy", "trestbpd", "exang", "xhypo", "oldpeak", "slope", "rldv5", "rldv5e", "ca", "restckm", "exerckm", "restef", "restwm", "exeref", "exerwm", "thal", "thalsev", "thalpul", "earlobe", "cmo", "cday", "cyr", "num", "lmt", "ladprox", "laddist", "diag", "cxmain", "ramus", "om1", "om2", "rcaprox", "rcadist", "lvx1", "lvx2", "lvx3", "lvx4", "lvf", "cathef", "junk", "place")

data1 <- scan(file="project_heart_cleveland.txt")
mat1 <- matrix(data1, ncol=75, byrow=TRUE)
datb1 <- data.frame(mat1)
datb1 <- cbind(datb1, "cleveland")
colnames(datb1) <- names

data2 <- scan(file="project_heart_hungarian.txt")
mat2 <- matrix(data2, ncol=75, byrow=TRUE)
datb2 <- data.frame(mat2)
datb2 <- cbind(datb2, "hungarian")
colnames(datb2) <- names

data3 <- scan(file="project_heart_longbeachva.txt")
mat3 <- matrix(data3, ncol=75, byrow=TRUE)
datb3 <- data.frame(mat3)
datb3 <- cbind(datb3, "longbeachva")
colnames(datb3) <- names

data4 <- scan(file="project_heart_switzerland.txt")
mat4 <- matrix(data4, ncol=75, byrow=TRUE)
datb4 <- data.frame(mat4)
datb4 <- cbind(datb4, "switzerland")
colnames(datb4) <- names

datall = rbind(datb1, datb2, datb3, datb4)


#### Question 1 ####



#### Question 2 ####
library(car)
model2 = lm(cbind(chol, thaldur, thaltime, met, thalach, thalrest, tpeakbps, tpeakbpd, trestbpd, oldpeak, rldv5, rldv5e) ~ proto + restecg + dig + prop + nitr + pro + diuretic, data=datall)
model2

#### Question 3 ####



#### Question 4 ####
