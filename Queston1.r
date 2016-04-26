
### Question 1
###Creating matrix and data frame for cleveland
cleve = scan("C:/Users/Chloe/Documents/Multivariate/project_heart_cleveland.txt")
clevel = matrix(cleve, ncol=75, byrow=TRUE)
#Indicator Variable
clevelandind = rep(1,282)
clevela = t(clevelandind)
one = t(clevela)
cleveland = cbind(clevel, one)
cleveland.df <- data.frame(cleveland)


###Creating matrix and data frame for Hungary
hun = scan("C:/Users/Chloe/Documents/Multivariate/project_heart_hungarian.txt")
hung = matrix(hun, ncol=75, byrow=TRUE)
#Indicator Variable
hungaryind = rep(2,294)
hunga = t(hungaryind)
two = t(hunga)
hungary = cbind(hung, two)
hungary.df <- data.frame(hungary)	



###Creating matrix and data frame for Long Beach
long = scan("C:/Users/Chloe/Documents/Multivariate/project_heart_longbeachva.txt")
longb = matrix(long, ncol=75, byrow=TRUE)
#Indicator Variable
longind = rep(3,200)
longbe = t(longind)
three = t(longbe)
longbeach = cbind(longb, three)
longbeach.df <- data.frame(longbeach)



###Creating matrix and data frame for Switzerland
swiss = scan("C:/Users/Chloe/Documents/Multivariate/project_heart_switzerland.txt")
switz = matrix(swiss, ncol=75, byrow=TRUE)
#Indicator Variable
swissind = rep(4,123)
switze = t(swissind)
four = t(switze)
switzerland = cbind(switz, four)
switzerland.df <- data.frame(switzerland)	



###Creating matrix and data frame for Joint
all = rbind(cleveland, hungary, longbeach, switzerland)
all.df <- data.frame(all)


###Names for Data Frames
 data <- read.table("C:/Users/Chloe/Documents/Multivariate/names.txt")
dattest = scan("C:/Users/Chloe/Documents/Multivariate/names.txt", what="")

#Putting names on all data frames
names(cleveland.df) <- dattest
names(hungary.df) <- dattest
names(longbeach.df) <- dattest
names(switzerland.df) <- dattest
names(all.df) <- dattest


### Changing Values for Hungary 
hungary.df$proto[hungary.df$proto==150] <- 7
hungary.df$proto[hungary.df$proto==100] <- 9
hungary.df$proto[hungary.df$proto==50] <- 11
###Nearest to 50
hungary.df$proto[hungary.df$proto==25] <- 11
###Nearest to 150
hungary.df$proto[hungary.df$proto==175] <- 7
hungary.df$proto[hungary.df$proto==125] <- 8
hungary.df$proto[hungary.df$proto==75] <- 10
###Nearest to 125
hungary.df$proto[hungary.df$proto==130] <- 8
###Over 25 away from anything there NA
hungary.df$proto[hungary.df$proto==200] <- NA


###Same as above 
all.df$proto[all.df$proto==150] <- 7
all.df$proto[all.df$proto==100] <- 9
all.df$proto[all.df$proto==50] <- 11
all.df$proto[all.df$proto==25] <- 11
all.df$proto[all.df$proto==175] <- 7
all.df$proto[all.df$proto==125] <- 8
all.df$proto[all.df$proto==75] <- 10
all.df$proto[all.df$proto==130] <- 8
all.df$proto[all.df$proto==200] <- NA

###Creating a subset with just the required protocols , the variables rldv5, rldv5e and met were removed.
allproto1 = subset(all.df, select=c(proto,chol,
thaldur, thaltime, thalach, thalrest, tpeakbps, tpeakbpd, trestbpd,
oldpeak))

###sufficient observations for each proto.
allproto1[allproto1[,1]==7,]

allproto1$proto[allproto1$proto==4] <- NA
allproto1$proto[allproto1$proto==12] <- NA
allproto1$proto[allproto1$proto==6] <- NA
allproto1$proto[allproto1$proto==7] <- NA

### Assigning new class names
allproto1$proto[allproto1$proto==8] <- 2
allproto1$proto[allproto1$proto==9] <- 3
allproto1$proto[allproto1$proto==10] <- 4
allproto1$proto[allproto1$proto==11] <- 5

### Include only complete cases
allproto1cc=allproto1[complete.cases(allproto1),]

### Creating vectors to run tests
allproto1cc1 = allproto1cc[,1] == 1
allproto1cc2 = allproto1cc[,1] == 2
allproto1cc3 = allproto1cc[,1]  == 3
allproto1cc4 = allproto1cc[,1]  == 4
allproto1cc5 = allproto1cc[,1]  == 5

boxM(allproto1cc[,2:9], allproto1cc1)
boxM(allproto1cc[,2:9], allproto1cc2)
boxM(allproto1cc[,2:9], allproto1cc3)
boxM(allproto1cc[,2:9], allproto1cc4)
boxM(allproto1cc[,2:9], allproto1cc5)

## Next to run hypothesis tests for the mean vector first using the multivariate james test
 maovjames(allproto1cc[,2:9], allproto1cc1[,1])
 

### subsetting the protos
proto1 = subset(allproto1cc, proto == 1)
proto2 = subset(allproto1cc, proto == 2)
proto3 = subset(allproto1cc, proto == 3)
proto4 = subset(allproto1cc, proto == 4)
proto5 = subset(allproto1cc, proto == 5)

### Conducting James test
### R=1
james(proto1[, 2:9], proto2[, 2:9], R=1)
james(proto1[, 2:9], proto3[, 2:9], R=1)
james(proto1[, 2:9], proto4[, 2:9], R=1)
james(proto1[, 2:9], proto5[, 2:9], R=1)

james(proto2[, 2:9], proto3[, 2:9], R=1)
james(proto2[, 2:9], proto4[, 2:9], R=1)
james(proto2[, 2:9], proto5[, 2:9], R=1)

james(proto3[, 2:9], proto4[, 2:9], R=1)
james(proto3[, 2:9], proto5[, 2:9], R=1)

james(proto4[, 2:9], proto5[, 2:9], R=1)

### R=2
james(proto1[, 2:9], proto2[, 2:9], R=2)
james(proto1[, 2:9], proto3[, 2:9], R=2)
james(proto1[, 2:9], proto4[, 2:9], R=2)
james(proto1[, 2:9], proto5[, 2:9], R=2)

james(proto2[, 2:9], proto3[, 2:9], R=2)
james(proto2[, 2:9], proto4[, 2:9], R=2)
james(proto2[, 2:9], proto5[, 2:9], R=2)

james(proto3[, 2:9], proto4[, 2:9], R=2)
james(proto3[, 2:9], proto5[, 2:9], R=2)

james(proto4[, 2:9], proto5[, 2:9], R=2)


### R=3
par(mfrow=c(3, 3))
james(proto1[, 2:9], proto2[, 2:9], R=3)
james(proto1[, 2:9], proto3[, 2:9], R=3)
james(proto1[, 2:9], proto4[, 2:9], R=3)
james(proto1[, 2:9], proto5[, 2:9], R=3)

james(proto2[, 2:9], proto3[, 2:9], R=3)
dev.new()
par(mfrow=c(3, 3))

james(proto2[, 2:9], proto4[, 2:9], R=3)
james(proto2[, 2:9], proto5[, 2:9], R=3)

james(proto3[, 2:9], proto4[, 2:9], R=3)
james(proto3[, 2:9], proto5[, 2:9], R=3)

james(proto4[, 2:9], proto5[, 2:9], R=3)

