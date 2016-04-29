### Exploratory Data Analysis


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
sink("Intcormatrix.txt")
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
pairs(allobs2cc[, 1:6

###Bivariate boxplots

#all
age <- as.vector(allobs2cc[,1])
chol <- as.vector(allobs2cc[,4])
thalech <- as.vector(allobs2cc[,5])
oldpeak <- as.vector(allobs2cc[,6])

png(filename="intplot3.png")
bv.boxplot(age, chol, ID.out = TRUE, bg.out = "red", xlab="Age", ylab="chol")
dev.off()
png(filename="intplot4.png")
bv.boxplot(age, thalech, ID.out = TRUE, bg.out = "red", xlab="Age", ylab="thalech")
dev.off()
png(filename="intplot5.png")
bv.boxplot(age,oldpeak, ID.out = TRUE, bg.out = "red", xlab="Age", ylab="oldpeak")
dev.off()


