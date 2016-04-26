# setwd("C:/Users/Timothy/Documents/GitHub/Project")

library(car)

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
    contvar <- vector()
    for(i in 1:ncol(dataset)) {
        obs <- dataset[,i]
        obs <- na.omit(obs)
        if(length(unique(obs)) == 1) {
            contvar <- c(contvar, colnames(dataset)[i])
        }
    }
    return(contvar)
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

    # Constant variables again
    constvarables <- constvar(newdataset)

    newdataset2 <- newdataset[, !colnames(newdataset) %in% unique(constvarables)]

    return(newdataset2)
}


#### Question 1 ####



#### Question 2 ####
library(car)
model2 = lm(cbind(chol, thaldur, thaltime, met, thalach, thalrest, tpeakbps, tpeakbpd, trestbpd, oldpeak, rldv5, rldv5e) ~ proto + restecg + dig + prop + nitr + pro + diuretic, data=datall)
model2

#### Question 3 ####

## Cleveland data
ex3datcle <- removenaandconstvar(datb1, .9)

ex3modcle <- lm(num ~ ., data = ex3datcle)

vif(ex3modcle)

ex3pcacle <- princomp(ex3datcle[,!colnames(ex3datcle) %in% "num"], cor=TRUE)
summary(ex3pcacle, loadings = TRUE)

plot(ex3pcacle$sd, type="l", xlab = "Component Number",
     ylab = "Eigenvalues", main = "Scree plot - Cleveland")

# We need 21 principle components to maintain 80% of the variance
ex3moecle <- lm(ex3datcle[,"num"] ~ ex3pcacle$scores[,1:21])
summary(ex3moecle)

ex3mofcle <- lm(ex3datcle[,"num"] ~ ex3pcacle$scores)
summary(ex3mofcle)

## Hungarian data
ex3dathun <- removenaandconstvar(datb2, .79)

ex3modhun <- lm(num ~ ., data = ex3dathun)
summary(ex3modhun)

vif(ex3modhun)

ex3pcahun <- princomp(ex3dathun[,!colnames(ex3dathun) %in% "num"], cor=TRUE)
summary(ex3pcahun, loadings = TRUE)

plot(ex3pcahun$sd, type="l", xlab = "Component Number",
     ylab = "Eigenvalues", main = "Scree plot - Hungary")

# We need 13 principle components to maintain 80% of the variance
ex3moehun <- lm(ex3dathun[,"num"] ~ ex3pcahun$score[,1:13])

ex3mofhun <- lm(ex3dathun[,"num"] ~ ex3pcahun$scores)

## Longbeachva
ex3datlon <- removenaandconstvar(datb3, .50)

ex3modlon <- lm(num ~ ., data = ex3datlon)

vif(ex3modlon)

ex3pcalon <- princomp(ex3datlon[,!colnames(ex3datlon) %in% "num"], cor=TRUE)
summary(ex3pcalon, loadings = TRUE)

plot(ex3pcalon$sd, type="l", xlab = "Component Number",
     ylab = "Eigenvalues", main = "Scree plot - Longbeach")

# We need 21 principle components to maintain 80% of the variance
ex3moelon <- lm(ex3datlon[,"num"] ~ ex3pcalon$score[,1:21])

ex3moflon <- lm(ex3datlon[,"num"] ~ ex3pcalon$scores)

## Switzerland data
ex3datswi <- removenaandconstvar(datb4, .13)

ex3modswi <- lm(num ~ ., data = ex3datswi)

vif(ex3modswi)

ex3pcaswi <- princomp(ex3datswi[,!colnames(ex3datswi) %in% "num"], cor=TRUE)
summary(ex3pcaswi, loadings = TRUE)

plot(ex3pcaswi$sd, type="l", xlab = "Component Number",
     ylab = "Eigenvalues", main = "Scree plot - Switzerland")

# We need 16 principle components to maintain 80% of the variance
ex3moeswi <- lm(ex3datswi[,"num"] ~ ex3pcaswi$score[,1:16])

ex3mofswi <- lm(ex3datswi[,"num"] ~ ex3pcaswi$scores)

#### Question 4 ####
