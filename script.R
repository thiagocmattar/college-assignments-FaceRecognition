rm(list=ls())
library(bmp)
library(rgl)

setwd("~/Reconhecimento de Padrões/Ex2/imagens")
f1<-c('f1','f2','f3','f4','f5')
f2<-c('F1','F2','F3','F4','F5')

y<-matrix(0,nrow=50,ncol=1)
k<-1
aux<-1
x<-matrix(0,nrow=50,ncol=2576)

for(i in 1:5)
{
  for(j in 1:5)
  {
    imglb1<-c(f1[i],'teste',j,'.bmp')
    imglb2<-c(f2[i],'TESTE',j,'R.bmp')
    imglb1<-paste(imglb1,sep="",collapse = "")
    imglb2<-paste(imglb2,sep="",collapse = "")
    x[aux,]<-as.vector(read.bmp(imglb1))
    y[k,1]<-j
    y[k+1,1]<-j
    x[aux+1,]<-as.vector(read.bmp(imglb2))
    k<-k+2
    aux<-aux+2
  }
}

mx<-rowMeans(x)
sx<-apply(x,1,sd)

plot(mx[y=='1'],sx[y=='1'],col='red',xlim=c(80,150),ylim=c(30,65),xlab='',ylab='')
par(new=T)
plot(mx[y=='2'],sx[y=='2'],col='blue',xlim=c(80,150),ylim=c(30,65),xlab='',ylab='')
par(new=T)
plot(mx[y=='3'],sx[y=='3'],col='green',xlim=c(80,150),ylim=c(30,65),xlab='',ylab='')
par(new=T)
plot(mx[y=='4'],sx[y=='4'],col='magenta',xlim=c(80,150),ylim=c(30,65),xlab='',ylab='')
par(new=T)
plot(mx[y=='5'],sx[y=='5'],col='black',xlim=c(80,150),ylim=c(30,65),
     xlab='mean(x)',ylab='sd(x)',main='Média e Desvio Padrão das Luminâncias')


#Cálculo das probabilidades de cada classe
pc1<-sum(1*(y=='1'))/nrow(x)
pc2<-sum(1*(y=='2'))/nrow(x)
pc3<-sum(1*(y=='3'))/nrow(x)
pc4<-sum(1*(y=='4'))/nrow(x)
pc5<-sum(1*(y=='5'))/nrow(x)

pdfnvar<- function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))

K1<-cov(cbind(mx[y=='1'],sx[y=='1']))
K2<-cov(cbind(mx[y=='2'],sx[y=='2']))
K3<-cov(cbind(mx[y=='3'],sx[y=='3']))
K4<-cov(cbind(mx[y=='4'],sx[y=='4']))
K5<-cov(cbind(mx[y=='5'],sx[y=='5']))

m1<-c(mean(mx[y=='1']),mean(sx[y=='1']))
m2<-c(mean(mx[y=='2']),mean(sx[y=='2']))
m3<-c(mean(mx[y=='3']),mean(sx[y=='3']))
m4<-c(mean(mx[y=='4']),mean(sx[y=='4']))
m5<-c(mean(mx[y=='5']),mean(sx[y=='5']))

range1<-seq(min(mx)-10,max(mx)+10,0.1)
range2<-seq(min(sx)-10,max(sx)+10,0.1)

pxc1<-matrix(0,nrow=length(range1),ncol=length(range2))
pxc2<-matrix(0,nrow=length(range1),ncol=length(range2))
pxc3<-matrix(0,nrow=length(range1),ncol=length(range2))
pxc4<-matrix(0,nrow=length(range1),ncol=length(range2))
pxc5<-matrix(0,nrow=length(range1),ncol=length(range2))

for(i in 1:length(range1))
{
  for(j in 1:length(range2))
  {
    pxc1[i,j]<-pdfnvar(c(range1[i],range2[j]),m1,K1,2)*pc1
    pxc2[i,j]<-pdfnvar(c(range1[i],range2[j]),m2,K2,2)*pc2
    pxc3[i,j]<-pdfnvar(c(range1[i],range2[j]),m3,K3,2)*pc3
    pxc4[i,j]<-pdfnvar(c(range1[i],range2[j]),m4,K4,2)*pc4
    pxc5[i,j]<-pdfnvar(c(range1[i],range2[j]),m5,K5,2)*pc5
  }
}

persp3d(range1,range2,pxc1,col='blue',zlim=c(0,0.025),zlab='P(x|C)',xlab='mean(x)',ylab='sd(x)')
persp3d(range1,range2,pxc2,col='red',zlim=c(0,0.025),add=T)
persp3d(range1,range2,pxc3,col='green',zlim=c(0,0.025),add=T)
persp3d(range1,range2,pxc4,col='magenta',zlim=c(0,0.025),add=T)
persp3d(range1,range2,pxc5,col='yellow',zlim=c(0,0.025),add=T)

M1<-1*(pxc1>pxc2 & pxc1>pxc3 & pxc1>pxc4 & pxc1>pxc5)
M2<-2*(pxc2>pxc1 & pxc2>pxc3 & pxc2>pxc4 & pxc2>pxc5)
M3<-3*(pxc3>pxc1 & pxc3>pxc2 & pxc3>pxc4 & pxc3>pxc5)
M4<-4*(pxc4>pxc1 & pxc4>pxc2 & pxc4>pxc3 & pxc4>pxc5)
M5<-5*(pxc5>pxc1 & pxc5>pxc2 & pxc5>pxc3 & pxc5>pxc4)

plot(mx[y=='1'],sx[y=='1'],col='blue',xlim=c(80,150),ylim=c(30,70),xlab='',ylab='')
par(new=T)
plot(mx[y=='2'],sx[y=='2'],col='red',xlim=c(80,150),ylim=c(30,70),xlab='',ylab='')
par(new=T)
plot(mx[y=='3'],sx[y=='3'],col='green',xlim=c(80,150),ylim=c(30,70),xlab='',ylab='')
par(new=T)
plot(mx[y=='4'],sx[y=='4'],col='magenta',xlim=c(80,150),ylim=c(30,70),xlab='',ylab='')
par(new=T)
plot(mx[y=='5'],sx[y=='5'],col='yellow',xlim=c(80,150),ylim=c(30,70),xlab='',ylab='')
par(new=T)
contour(range1,range2,M1,xlim=c(80,150),ylim=c(30,70),
        xlab='mean(x)',ylab='sd(x)',nlevels=1,drawlabels = FALSE)

par(new=T)
contour(range1,range2,pxc1,col='blue',xlim=c(80,150),ylim=c(30,70),xlab='mean(x)',ylab='sd(x)')
par(new=T)
contour(range1,range2,pxc2,xlim=c(80,150),ylim=c(30,70),col='red')
par(new=T)
contour(range1,range2,pxc3,xlim=c(80,150),ylim=c(30,70),col='green')
par(new=T)
contour(range1,range2,pxc4,xlim=c(80,150),ylim=c(30,70),col='magenta')
par(new=T)
contour(range1,range2,pxc5,xlim=c(80,150),ylim=c(30,70),col='yellow',
        main='Curvas de nível de P(x|Ci)')

par(new=T)
contour(range1,range2,M2,xlim=c(80,150),ylim=c(30,70),
        xlab='mean(x)',ylab='sd(x)',nlevels=1,drawlabels = FALSE)
par(new=T)
contour(range1,range2,M3,xlim=c(80,150),ylim=c(30,70),
        xlab='mean(x)',ylab='sd(x)',nlevels=1,drawlabels = FALSE)
par(new=T)
contour(range1,range2,M4,xlim=c(80,150),ylim=c(30,70),
        xlab='mean(x)',ylab='sd(x)',nlevels=1,drawlabels = FALSE)
par(new=T)
contour(range1,range2,M5,xlim=c(80,150),ylim=c(30,70),
        xlab='mean(x)',ylab='sd(x)',nlevels=1,drawlabels = FALSE)


