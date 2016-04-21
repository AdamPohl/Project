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


# Function to find variabes that are missing at least the percent data given
navariables <- function(dataset, percent) {
  emptryvariables <- vector()
  for(i in 1:ncol(dataset)) {
    currentnacount <- 0
    for(j in 1:nrow(dataset)) {
      if(is.na(dataset[j, i])) {
        currentnacount <- currentnacount + 1
      }
    }
    if(currentnacount >= (nrow(dataset) * percent)) {
      emptryvariables <- c(emptryvariables, colnames(dataset)[i])
    } 
  }
  return(emptryvariables)
}

# Find variables that are constant
constvar <- function(dataset) {
  lst <- lapply(dataset, function(x)length(unique(x)))
  return(colnames(dataset)[which(!lst > 1)])
}

# This function removes empty variables and constant columns
removenaandconstvar <- function(dataset, x) {
  # dummy variables
  dummyvar <- c("id", "ccf", "dummy", "restckm", "exerckm", "thalsev", 
                "thapul", "earlobe", "lvx1", "lvx2", "lvx3", "lvx4", "lvf", 
                "cathef", "junk", "place")
  
  # Finding variables with a least x missing data
  missingvarables <- navariables(dataset, x)
  
  # Constant variables
  constvarables <- constvar(dataset)
  
  newdataset <- dataset[, !colnames(dataset) %in% unique(c(dummyvar, missingvarables, constvarables))]
  newdataset <- na.omit(newdataset)
  
  return(newdataset)
}


#### Question 1 ####



#### Question 2 ####
library(car)
model2 = lm(cbind(chol, thaldur, thaltime, met, thalach, thalrest, tpeakbps, tpeakbpd, trestbpd, oldpeak, rldv5, rldv5e) ~ proto + restecg + dig + prop + nitr + pro + diuretic, data=datall)
model2

#### Question 3 ####

# Cleveland data
ex3date <- datb1

# Finding variables with a least 80% missing data
navariables(ex3date, .80)

## They are painloc, painexer, relrest, pncaden, smoke, rldv5, restckm, 
## exerckm, restef, restwm, exeref, exerwm, thalsev, thalpul, earlobe, 
## diag, ramus, om2, cathef, junk

constvar(ex3date)
# proto is a constant variable


ex3datf <- ex3date[,c("num", "age", "sex", "cp", "trestbps", "htn", "chol", "cigs", 
                      "years", "fbs", "famhist", "restecg", "ekgmo", 
                      "ekgday", "ekgyr", "dig", "prop", "nitr", "pro", 
                      "diuretic", "thaldur", "thaltime", "met", 
                      "thalach", "thalrest", "tpeakbps", "tpeakbpd", 
                      "trestbpd", "exang", "xhypo", "oldpeak", "slope", 
                      "rldv5e", "ca", "thal", "cmo", "cday", "cyr", 
                      "lmt", "ladprox", "laddist", "cxmain", "om1", "rcaprox",
                      "rcadist")]

ex3datf <- na.omit(ex3datf)

ex3modcle <- lm(num ~ ., data = ex3datf)

ex3pca1 <- princomp(ex3datf[,2:ncol(ex3datf)], cor=TRUE)
ex3pca1

#### Question 4 ####
