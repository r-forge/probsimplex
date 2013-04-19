
<!-- This is the project specific website template -->
<!-- It is about to be changed -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> 


<strong>CODE FOR SIMPLEX TRIANGLE</strong>

<p>Simfunc<-function(a,b){
<p>x<- -b/2 + 1-a
<p>y<-b*sqrt(3)/2
<p>return(c(x,y))
<p>}
<p>Sim<-function(x,y,z,aa){
<p>if (min(x)<0){stop("Probabilities cannot be negative")}
<p>if (min(y)<0){stop("Probabilities cannot be negative")}
<p>a<-length(x)
<p>vec<-rep(0,a)
<p>for (i in 1:a){
<p>vec[i]<-x[i]+y[i]                 
<p>              }
<p>if (max(vec)>1){stop("Distributions sum to more than one")}
<p>x1<-c(0,1)
<p>y1<-c(0,0)
<p>plot(x1,y1,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
<p>x2<-c(0,0.5)
<p>y2<-c(0,sqrt(3/4))
<p>par(new=T)
<p>plot(x2,y2,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
<p>x3<-c(0.5,1)
<p>y3<-c(sqrt(3/4),0)
<p>par(new=T)
<p>plot(x3,y3,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
<p>for (i in 1:a){
<p>              par(new=T)
<p>              plot(Simfunc(x[i],y[i])[1],Simfunc(x[i],y[i])[2],xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",main="Simplex",axes=FALSE,pch=aa,col=z)
<p>              }
<p>text(-0.05,-0.05,"(1,0,0)")              
<p>text(1.05,-0.05,"(0,0,1)")
<p>text(0.5,0.92,"(0,1,0)")
<p>}
<p>Simline<-function(x,y,z){
<p>if (min(x)<0){stop("Probabilities cannot be negative")}
<p>if (min(y)<0){stop("Probabilities cannot be negative")}
<p>a<-length(x)
<p>vec<-rep(0,a)
<p>for (i in 1:a){
<p>vec[i]<-x[i]+y[i]                 
<p>              }
<p>if (max(vec)>1){stop("Distributions sum to more than one")}
<p>x1<-c(0,1)
<p>y1<-c(0,0)
<p>plot(x1,y1,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
<p>x2<-c(0,0.5)
<p>y2<-c(0,sqrt(3/4))
<p>par(new=T)
<p>plot(x2,y2,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
<p>x3<-c(0.5,1)
<p>y3<-c(sqrt(3/4),0)
<p>par(new=T)
<p>plot(x3,y3,type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",axes=FALSE)
<p>par(new=T)
<p>a<-length(x)
<p>plot(Simfunc(x,y)[1:a],Simfunc(x,y)[(a+1):(2*a)],type="l",xlim=c(-0.1,1.1),ylim=c(-0.1,sqrt(3/4)+0.1),xlab="", ylab="",main="Simplex",axes=FALSE,col=z)            
<p>text(-0.05,-0.05,"(1,0,0)")              
<p>text(1.05,-0.05,"(0,0,1)")
<p>text(0.5,0.92,"(0,1,0)")
<p>}

<p>
<p>

<strong>CODE FOR SIMPLEX TETRAHEDRON</strong>

<p>#requires package "scatterplot3d"</p>

<p>#2d simplex</p>

<p>simplex<-function(x,y,z){
<p>f1<-1-x-y/2
<p>f2<-(sqrt(3)/2)*y
<p>return(c(f1,f2))
<p>}

<p>#generation of tetrahedron 

<p>library(scatterplot3d)
<p>x1<-c(0.0,0.5,1.0)
<p>y1<-c(0,sqrt(3)/2,0)
<p>z1<-c(0,0,0)
<p>scatterplot3d(x1,y1,z1,xlim=c(-0.1,1),ylim=c(-0.1,1),zlim=c(-0.1,1),type="l",col.axis="blue",xlab="",ylab="",zlab="",grid=F)
<p>x2<-c(0,0.5,1)
<p>y2<-c(0,tan(pi/6)/2,0)
<p>z2<-c(0,sqrt(2/3),0)
<p>par(new=T)
<p>scatterplot3d(x2,y2,z2,xlim=c(-0.1,1),ylim=c(-0.1,1),zlim=c(-0.1,1),type="l",col.axis="blue",xlab="",ylab="",zlab="",grid=F)
<p>x3<-c(0.5,0.5)
<p>y3<-c(tan(pi/6)/2,sqrt(3)/2)
<p>z3<-c(sqrt(2/3),0)
<p>par(new=T)
<p>scatterplot3d(x3,y3,z3,xlim=c(-0.1,1),ylim=c(-0.1,1),zlim=c(-0.1,1),type="l",col.axis="blue",xlab="",ylab="",zlab="",grid=F)
<p>x4<-c(0,1)
<p>y4<-c(0,0)
<p>z4<-c(0,0)
<p>par(new=T)
<p>scatterplot3d(x4,y4,z4,xlim=c(-0.1,1),ylim=c(-0.1,1),zlim=c(-0.1,1),type="l",col.axis="blue",xlab="",ylab="",zlab="",grid=F)
<p>#convert length-four probability distribution into 3D point
<p>ThreeD<-function(a,b,c,d){
<p>if(d<1){
<p>if(d>0){
<p>a1<-simplex(a/(1-d),b/(1-d),c(1-d))[1]*(1-d)
<p>a2<-simplex(a/(1-d),b/(1-d),c(1-d))[2]*(1-d) 
<p>x1<-(1-d)/2
<p>y1<-(1-d)/(2*sqrt(3))
<p>c1<-1/2-x1
<p>c2<-1/(2*sqrt(3))-y1
<p>e1<-simplex(a/(1-d),b/(1-d),c(1-d))[1]*(1-d)+c1
<p>e2<-simplex(a/(1-d),b/(1-d),c(1-d))[2]*(1-d)+c2
<p>e3<-sqrt(2/3)*d}
<p>else{
<p>e1<-simplex(a,b,c)[1]
<p>e2<-simplex(a,b,c)[2]
<p>e3<-0
<p>}
<p>}
<p>else{
<p>e1<-simplex(1/3,1/3,1/3)[1]
<p>e2<-simplex(1/3,1/3,1/3)[2]
<p>e3<-sqrt(2/3)
<p>return(c(e1,e2,e3))
<p>#plot (a,b,c,d)</p>        
<p>par(new=T)</p>
<p>scatterplot3d(ThreeD(a,b,c,d)[1],ThreeD(a,b,c,d)[2],ThreeD(a,b,c,d)[3],xlim=c(-0.1,1),ylim=c(-0.1,1),zlim=c(-0.1,1),col.axis="blue",xlab="",ylab="",zlab="",grid=F)</p>


 </p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
