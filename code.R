## MA3505 - Multivariate Statistics Project 1

## Importing Librays
library(asbio)
library(car)
library(MASS)


## Names of each variable
names =  c("id", "ccf", "age", "sex", "painloc", "painexer", "relrest", "pncaden", "cp", "trestbps", "htn", "chol", "smoke", "cigs", "years", "fbs", "dm", "famhist", "restecg", "ekgmo", "ekgday", "ekgyr", "dig", "prop", "nitr", "pro", "diuretic", "proto", "thaldur", "thaltime", "met", "thalach", "thalrest", "tpeakbps", "tpeakbpd", "dummy", "trestbpd", "exang", "xhypo", "oldpeak", "slope", "rldv5", "rldv5e", "ca", "restckm", "exerckm", "restef", "restwm", "exeref", "exerwm", "thal", "thalsev", "thalpul", "earlobe", "cmo", "cday", "cyr", "num", "lmt", "ladprox", "laddist", "diag", "cxmain", "ramus", "om1", "om2", "rcaprox", "rcadist", "lvx1", "lvx2", "lvx3", "lvx4", "lvf", "cathef", "junk", "place")

## Importing the datasets
data1 <- scan(file="project_heart_cleveland.txt")
mat1 <- matrix(data1, ncol=75, byrow=TRUE)
datb1 <- data.frame(mat1)
datb1 <- cbind(datb1, "cleveland")
colnames(datb1) <- names
cleveland.df <- datb1

data2 <- scan(file="project_heart_hungarian.txt")
mat2 <- matrix(data2, ncol=75, byrow=TRUE)
datb2 <- data.frame(mat2)
datb2 <- cbind(datb2, "hungarian")
colnames(datb2) <- names
hungary.df <- datb2

data3 <- scan(file="project_heart_longbeachva.txt")
mat3 <- matrix(data3, ncol=75, byrow=TRUE)
datb3 <- data.frame(mat3)
datb3 <- cbind(datb3, "longbeachva")
colnames(datb3) <- names
longbeach.df <- datb3

data4 <- scan(file="project_heart_switzerland.txt")
mat4 <- matrix(data4, ncol=75, byrow=TRUE)
datb4 <- data.frame(mat4)
datb4 <- cbind(datb4, "switzerland")
colnames(datb4) <- names
switzerland.df <-datb4

datall = rbind(datb1, datb2, datb3, datb4)
all.df <- datall

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


#### Exploratory Data Analysis ####


dim(cleveland.df)
## dim 282 76
dim(hungary.df)
#dim 294 76
dim(switzerland.df)
## 123 76
dim(longbeach.df)
## 200 76
dim(all.df)
## 899 76


str(all.df)
#### Read the summaries to get any useful information
summary (all.df)
summary (cleveland.df)
summary (hungary.df)
summary (switzerland.df)
summary (longbeach.df)

### Some different variables
hungaryobs = subset(hungary.df, selec=c(age, sex,cp,trestbps, chol, fbs,
                                        restecg, thalach, exang, oldpeak, num))
swissobs = subset(switzerland.df, selec=c(age, sex,cp,trestbps, chol, fbs,
                                          restecg, thalach, exang, oldpeak, num))
longbeachobs = subset(longbeach.df, selec=c(age, sex,cp,trestbps, chol, fbs,
                                            restecg, thalach, exang, oldpeak, num))
clevelandobs = subset(cleveland.df, selec=c(age, sex,cp,trestbps, chol, fbs,
                                            restecg, thalach, exang, oldpeak, num))
allobs = subset(all.df, selec=c(age, sex,cp,trestbps, chol, fbs,
                                restecg, thalach, exang, oldpeak, num))

hungaryobscc=hungaryobs[complete.cases(hungaryobs),]
longbeachobscc=longbeachobs[complete.cases(longbeachobs),]
swissobscc=swissobs[complete.cases(swissobs),]
clevelandobscc=clevelandobs[complete.cases(clevelandobs),]
allobscc=allobs[complete.cases(allobs),]


## correlation matrix
sink("intro files/Intcormatrix.txt")
cor(allobscc)
sink()

###Removing 0,1 variables
allobs2 = subset(all.df, selec=c(age,cp,trestbps, chol,
                                 thalach, oldpeak, num))
allobs2cc=allobs2[complete.cases(allobs2),]
hungaryobs2 = subset(hungary.df, selec=c(age,cp,trestbps, chol, thalach, oldpeak))
swissobs2 = subset(switzerland.df, selec=c(age,cp,trestbps, chol, thalach, oldpeak))
longbeachobs2 = subset(longbeach.df, selec=c(age,cp,trestbps, chol, thalach, oldpeak))
clevelandobs2 = subset(cleveland.df, selec=c(age,cp,trestbps, chol, thalach, oldpeak))

hungaryobs2cc=hungaryobs2[complete.cases(hungaryobs2),]
longbeachobs2cc=longbeachobs2[complete.cases(longbeachobs2),]
swissobs2cc=swissobs2[complete.cases(swissobs2),]
clevelandobs2cc=clevelandobs2[complete.cases(clevelandobs2),]



###large scattergraph
pairs(allobs2cc[, 1:6])

###Bivariate boxplots

#all
age <- as.vector(allobs2cc[,1])
chol <- as.vector(allobs2cc[,4])
thalech <- as.vector(allobs2cc[,5])
oldpeak <- as.vector(allobs2cc[,6])

png(filename="intro files/intplot3.png")
bv.boxplot(age, chol, ID.out = TRUE, bg.out = "red", xlab="Age", ylab="chol")
dev.off()
png(filename="intro files/intplot4.png")
bv.boxplot(age, thalech, ID.out = TRUE, bg.out = "red", xlab="Age", ylab="thalech")
dev.off()
png(filename="intro files/intplot5.png")
bv.boxplot(age,oldpeak, ID.out = TRUE, bg.out = "red", xlab="Age", ylab="oldpeak")
dev.off()


#### Question 1 ####



#### Question 2 ####
model2 = lm(cbind(chol, thaldur, thaltime, met, thalach, thalrest, tpeakbps, tpeakbpd, trestbpd, oldpeak, rldv5, rldv5e) ~ proto + restecg + dig + prop + nitr + pro + diuretic, data=datall)
# summary(model2)
# model2

#### Question 3 ####

## Cleveland data
ex3datcle <- removenaandconstvar(datb1, .9)

ex3modcle <- lm(num ~ ., data = ex3datcle)

# vif(ex3modcle)

ex3pcacle <- princomp(ex3datcle[,!colnames(ex3datcle) %in% "num"], cor=TRUE)

# summary(ex3pcacle, loadings = TRUE)

# postscript("question3output/clescreeplot.eps", width = 6, height = 4,
#            horizontal = FALSE, onefile = FALSE, paper = "special")
# plot(ex3pcacle$sd, type="l", xlab = "Component Number",
#      ylab = "Eigenvalues", main = "Scree plot - Cleveland")
# abline(v = 6, col = "purple", lty = 2)
# dev.off()


## Hungarian data
ex3dathun <- removenaandconstvar(datb2, .79)

ex3modhun <- lm(num ~ ., data = ex3dathun)

# vif(ex3modhun)

ex3pcahun <- princomp(ex3dathun[,!colnames(ex3dathun) %in% "num"], cor=TRUE)

# summary(ex3pcahun, loadings = TRUE)

# postscript("question3output/hunscreeplot.eps", width = 6, height = 4,
#            horizontal = FALSE, onefile = FALSE, paper = "special")
# plot(ex3pcahun$sd, type="l", xlab = "Component Number",
#      ylab = "Eigenvalues", main = "Scree plot - Hungary")
# abline(v = 6, col = "purple", lty = 2)
# dev.off()


## Longbeachva
ex3datlon <- removenaandconstvar(datb3, .50)

ex3modlon <- lm(num ~ ., data = ex3datlon)

# vif(ex3modlon)

ex3pcalon <- princomp(ex3datlon[,!colnames(ex3datlon) %in% "num"], cor=TRUE)

# summary(ex3pcalon, loadings = TRUE)

# postscript("question3output/lonscreeplot.eps", width = 6, height = 4,
#            horizontal = FALSE, onefile = FALSE, paper = "special")
# plot(ex3pcalon$sd, type="l", xlab = "Component Number",
#      ylab = "Eigenvalues", main = "Scree plot - Longbeach")
# abline(v = 11, col = "purple", lty = 2)
# dev.off()

## Switzerland data
ex3datswi <- removenaandconstvar(datb4, .13)

ex3modswi <- lm(num ~ ., data = ex3datswi)

# vif(ex3modswi)

ex3pcaswi <- princomp(ex3datswi[,!colnames(ex3datswi) %in% "num"], cor=TRUE)

# summary(ex3pcaswi, loadings = TRUE)

# postscript("question3output/swiscreeplot.eps", width = 6, height = 4,
#            horizontal = FALSE, onefile = FALSE, paper = "special")
# plot(ex3pcaswi$sd, type="l", xlab = "Component Number",
#      ylab = "Eigenvalues", main = "Scree plot - Switzerland")
# abline(v = 5, col = "purple", lty = 2)
# dev.off()

#### Question 4 ####

## Clevland model ##
clevdata <- subset(ex3datcle, select = c(-lmt))
clevmod <- lda(num ~ ., data = clevdata)
# clevmod
clevmod2 <- predict(clevmod)
ldahist(data=clevmod2$x[,1], g=clevdata$num)
plot(clevmod2$x[, 1], clevmod2$x[, 2], col = clevdata$num, main = "scatterplot for Clevland model")


## Hungary model ##
hungmod <- lda(num ~ ., data = ex3dathun)
# hungmod
hungmod2 <- predict(clevmod)
ldahist(data=hungmod2$x[,1], g=ex3dathun$num)
plot(hungmod2$x[, 1], hungmod2$x[, 2], col = ex3dathun$num, main = "scatterplot for Hungary model")

## Longbeach model ##
longmod <- lda(num ~ ., data = ex3datlon)
# longmod
longmod2 <- predict(clevmod)
ldahist(data=longmod2$x[,1], g=ex3datlon$num)
plot(longmod2$x[, 1], longmod2$x[, 2], col = ex3datlon$num, main = "scatterplot for Longbeach model")

## Switzerland model ##
swidata <- subset(ex3datswi, select= c(-lmt))
switmod <- lda(num ~ ., data = swidata)
# switmod
switmod2 <- predict(switmod)
ldahist(data=switmod2$x[,1], g=swidata$num)
plot(switmod2$x[, 1], switmod2$x[, 2], col = swidata$num, main = "scatterplot for Switzerland model")
