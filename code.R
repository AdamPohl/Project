data1 <- scan(file="/home/huaraz2/Desktop/multivarite/Project/project_heart_cleveland.txt")
mat1 <- matrix(data1, ncol=75, byrow=TRUE)
data2 <- scan(file="/home/huaraz2/Desktop/multivarite/Project/project_heart_hungarian.txt")
mat2 <- matrix(data2, ncol=75, byrow=TRUE)
data3 <- scan(file="/home/huaraz2/Desktop/multivarite/Project/project_heart_longbeachva.txt")
mat3 <- matrix(data3, ncol=75, byrow=TRUE)
data4 <- scan(file="/home/huaraz2/Desktop/multivarite/Project/project_heart_switzerland.txt")
mat4 <- matrix(data4, ncol=75, byrow=TRUE)

vec1 <- vector()
vec2 <- vector()
vec3 <- vector()
vec4 <- vector()

for (i in 1:282){
  vec1[i] <- "one"
}
mat1 <- cbind(mat1, vec1)
for (i in 1:294){
  vec2[i] <- "two"
}
mat2 <- cbind(mat2, vec2)
for (i in 1:200){
  vec3[i] <- "three"
}
mat3 <- cbind(mat3, vec3)
for (i in 1:123){
  vec4[i] <- "four"
}
mat4 <- cbind(mat4, vec4)
