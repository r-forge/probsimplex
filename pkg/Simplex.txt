Simfunc<-function(a,b){
x<- -b/2 + 1-a
y<-b*sqrt(3)/2
return(c(x,y))
}

Sim<-function(x,y,z,aa){
if (min(x)<0){stop("Probabilities cannot be negative")}
if (min(y)<0){stop("Probabilities cannot be negative")}
a<-length(x)
vec<-rep(0,a)
for (i in 1:a){
vec[i]<-x[i]+y[i]                 
              }
if (max(vec)>1){stop("Distributions sum to more than one")}
x1<-c(0,1)
y1<-c(0,0)
plot(x1,y1,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
x2<-c(0,0.5)
y2<-c(0,sqrt(3/4))
par(new=T)
plot(x2,y2,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
x3<-c(0.5,1)
y3<-c(sqrt(3/4),0)
par(new=T)
plot(x3,y3,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
for (i in 1:a){
              par(new=T)
              plot(Simfunc(x[i],y[i])[1],Simfunc(x[i],y[i])[2],xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",main="Simplex",axes=FALSE,pch=aa,col=z)
              }
text(-0.05,-0.05,"(1,0,0)")              
text(1.05,-0.05,"(0,0,1)")
text(0.5,0.92,"(0,1,0)")
}


Simline<-function(x,y,z){
if (min(x)<0){stop("Probabilities cannot be negative")}
if (min(y)<0){stop("Probabilities cannot be negative")}
a<-length(x)
vec<-rep(0,a)
for (i in 1:a){
vec[i]<-x[i]+y[i]                 
              }
if (max(vec)>1){stop("Distributions sum to more than one")}
x1<-c(0,1)
y1<-c(0,0)
plot(x1,y1,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
x2<-c(0,0.5)
y2<-c(0,sqrt(3/4))
par(new=T)
plot(x2,y2,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
x3<-c(0.5,1)
y3<-c(sqrt(3/4),0)
par(new=T)
plot(x3,y3,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
par(new=T)
a<-length(x)
plot(Simfunc(x,y)[1:a],Simfunc(x,y)[(a+1):(2*a)],type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",main="Simplex",axes=FALSE,col=z)            
text(-0.05,-0.05,"(1,0,0)")              
text(1.05,-0.05,"(0,0,1)")
text(0.5,0.92,"(0,1,0)")
}