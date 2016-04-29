##########################################################
# Two sample means with no assumption of equal variances #
##########################################################

james=function(y1,y2,a=0.05,R=999) {
## y1 and y2 are the two samples
## a is the significance level, set by default to 0.05
## if R==1 the James test is performed
## if R==2 the Nel and van der Merwe test is performed
## if R>2 bootstrap calculation of the p-value is performed
## 999 bootstrap re-samples are set by default
y1=as.matrix(y1)
y2=as.matrix(y2)
p=ncol(y1) ## dimensionality of the data
n1=nrow(y1) ## size of the first sample
n2=nrow(y2) ## size of the second sample
n=n1+n2 ## the toal sample size
ybar1=apply(y1,2,mean) ## sample mean vector of the first sample
ybar2=apply(y2,2,mean) ## sample mean vector of the second sample
dbar=ybar2-ybar1 ## difference of the two mean vectors
A1=cov(y1)/n1 ; A2=cov(y2)/n2
V=A1+A2 ## covariance matrix of the difference
test=as.numeric(dbar%*%solve(V)%*%dbar)
b1=solve(V)%*%A1
b2=solve(V)%*%A2
if (R==1) { ## James test
A=1+(1/(2*p))*( (sum(diag(b1)))^2/(n1-1)+(sum(diag(b2)))^2/(n2-1) )
B=(1/(p*(p+2)))*( sum(diag(b1%*%b1))/(n1-1)+sum(diag(b2%*%b2))/(n2-1)+
0.5*((sum(diag(b1)))^2/(n1-1)+(sum(diag(b2)))^2/(n2-1)) )
x2=qchisq(1-a,p)
delta=(A+B*x2)
twoha=x2*delta ## corrected critical value of the chi-square distribution
pvalue=1-pchisq(test/delta,p) ## p-value of the test statistic
result=list(test=test,correction=delta,corrected.critical.value=twoha,p.value=pvalue) }
if (R==2) { ## MNV test
low=( sum(diag(b1%*%b1))+sum(diag(b1))^2 )/n1+( sum(diag(b2%*%b2))+sum(diag(b2))^2 )/n2
v=(p+p^2)/low
test=as.numeric( ( (v-p+1)/(v*p) )*test ) ## test statistic
crit=qf(1-a,p,v-p+1) ## critical value of the F distribution
pvalue=1-pf(test,p,v-p+1) ## p-value of the test statistic
result=list(test=test,critical=crit,df1=p,df2=v-p-1,p.value=pvalue) }
if (R>2) { ## bootstrap calibration
z=rbind(y1,y2) ## the two samples combined in one
mc=matrix(rep(colMeans(z),n),nrow=n,byrow=TRUE) ## the combined sample mean vector
m1=matrix(rep(colMeans(y1),n1),nrow=n1,byrow=TRUE) ## first mean vector
m2=matrix(rep(colMeans(y2),n2),nrow=n2,byrow=TRUE) ## second mean vector
## the next two rows bring the mean vectors of the two sample equal to the
## combined mean and thus equal under the null hypothesis
x1=y1-m1+mc[1:n1,]
x2=y2-m2+mc[1:n2,]
t=rep(0,R)
for (i in 1:R) {
b1=sample(1:n1,n1,replace=TRUE)
b2=sample(1:n2,n2,replace=TRUE)
xbar1=apply(x1[b1,],2,mean) ## sample mean vector of the first sample
xbar2=apply(x2[b2,],2,mean) ## sample mean vector of the second sample
db=xbar2-xbar1 ## difference of the two mean vectors
A1=cov(x1[b1,])/n1 ; A2=cov(x2[b2,])/n2
V=A1+A2 ## covariance matrix of the difference
t[i]=as.numeric(db%*%solve(V)%*%db) }
pvalue=( sum(t>test)+1 )/ (R+1)
hist(t,xlab="bootstrapped test statistic",main=" ")
abline(v=test,lty=2,lwd=2) ## The dotted vertical line is the test statistic value
result=list(p.value=pvalue) }
result }



##################################################
# MANOVA with no assumption of equal covariances #
##################################################

maovjames=function(x,ina,a=0.05){
## x contains all the groups together
x=as.matrix(x) ## makes sure x is a matrix
ina=as.numeric(ina) ## the group indicator variable
ni=as.vector(table(ina)) ## the group sample sizes
k=max(ina) ## the number of groups
p=ncol(x) ## the dimensionality
n=nrow(x) ## the total sample size
## the objects below will be used later
me=mi=W=matrix(nrow=k,ncol=p)
t=rep(0,k)
wi=array(dim=c(p,p,k))
## the next for function calculates the
## mean vector and covariance matrix of each group
for (i in 1:k) {
mi[i,]=colMeans(x[ina==i,])
wi[,,i]=solve( var(x[ina==i,])/ni[i] )
me[i,]=mi[i,]%*%wi[,,i] }
W=apply(wi,1:2,sum)
ma=apply(me,2,sum)
mesi=ma%*%solve(W) ## common mean vector
for (i in 1:k) t[i]=(mi[i,]-mesi)%*%wi[,,i]%*%t(mi[i,]-mesi)
test=sum(t) ## the test statistic
r=p*(k-1)
t1=t2=numeric(k)
for (i in 1:k){
exa1=diag(p)-solve(W)%*%wi[,,i]
7
exa2=exa1%*%exa1
t1[i]=sum(diag(exa1))
t2[i]=sum(diag(exa2)) }
A=1+1/(2*r)*sum(t1^2/(ni-1))
B=1/(r*(r+2))*sum( t2/(ni-1)+t1^2/(2*(ni-1)) )
x2=qchisq(1-a,r)
delta=(A+B*x2)
twoha=x2*delta ## corrected critical value of the chi-square distribution
pvalue=1-pchisq(test/delta,r) ## p-value of the test statistic
result=list(test=test,correction=delta,corrected.critical.value=twoha,p.value=pvalue)
result }

############################
# Test for equal variances #
############################

cov.equal=function(x,Sigma,a=0.05) {
## x is the data set
## Sigma is the assumed covariance matrix
## a is the significance level set by default to 0.05
x=as.matrix(x)
Sigma=as.matrix(Sigma)
p=ncol(x) ## dimensionality of the data
n=nrow(x) ## total sample size
S=cov(x) ## sample covariance matrix
## the next 2 lines construct the test statistic
mesa=solve(Sigma)%*%S
test=n*sum(diag(mesa))-n*log(det(mesa))-n*p
df=0.5*p*(p+1) ## the degrees of freedom of the chi-square distribution
pvalue=1-pchisq(test,df) ## p-value of the test statistic
crit=qchisq(1-a,df) ## critical value of the chi-square distribution
list(test=test,degres=df,p.value=pvalue,critical=crit) }


