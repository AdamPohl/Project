data1 <- scan(file="project_heart_cleveland.txt")
mat1 <- matrix(data1, ncol=75, byrow=TRUE)
data2 <- scan(file="project_heart_hungarian.txt")
mat2 <- matrix(data2, ncol=75, byrow=TRUE)
data3 <- scan(file="project_heart_longbeachva.txt")
mat3 <- matrix(data3, ncol=75, byrow=TRUE)
data4 <- scan(file="project_heart_switzerland.txt")
mat4 <- matrix(data4, ncol=75, byrow=TRUE)

vec1 <- vector()
vec2 <- vector()
vec3 <- vector()
vec4 <- vector()

for (i in 1:282){
  vec1[i] <- "Cleveland"
}
mat1 <- cbind(mat1, vec1)
for (i in 1:294){
  vec2[i] <- "Hungary"
}
mat2 <- cbind(mat2, vec2)
for (i in 1:200){
  vec3[i] <- "Longbeach"
}
mat3 <- cbind(mat3, vec3)
for (i in 1:123){
  vec4[i] <- "Switzerland"
}
mat4 <- cbind(mat4, vec4)

mat = rbind(mat1, mat2, mat3, mat4)

names =  c("id", "ccf", "age", "sex", "painloc", "painexer", "relrest", "pncaden", "cp", "trestbps", "htn", "chol", "smoke", "cigs", "years", "fbs", "dm", "famhist", "restecg", "ekgmo", "ekgday", "ekgyr", "dig", "prop", "nitr", "pro", "diuretic", "proto", "thaldur", "thaltime", "met", "thalach", "thalrest", "tpeakbps", "tpeakbpd", "dummy", "trestbpd", "exang", "xhypo", "oldpeak", "slope", "rldv5", "rldv5e", "ca", "restckm", "exerckm", "restef", "restwm", "exeref", "exerwm", "thal", "thalsev", "thalpul", "earlobe", "cmo", "cday", "cyr", "num", "lmt", "ladprox", "laddist", "diag", "cxmain", "ramus", "om1", "om2", "rcaprox", "rcadist", "lvx1", "lvx2", "lvx3", "lvx4", "lvf", "cathef", "junk", "place")

colnames(mat) = names
