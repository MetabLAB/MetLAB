  #--------------------------------
  # func name: PLS-DA, VIP and scoreplot
  # source("D:/快盘/Func/R/plsda.R")
  #pls3(X=,Y=,label4=)
###########################################################
library(plsdepot)
library(scatterplot3d)
## 两分类情况13
pls2 <- function(X=modelX,Y=modelY,
	col2=c("red", "blue"),
	pch2=c(19,2),
	label2=c("EOC","Normal"),
	filename="PLS-scoreplot3"
) { ## start
	plsr2 <- plsreg1(X,as.numeric(Y),comps =10) #Y must be num
	plsr2$Q2
	## R2+Q2最大
	flag <- sort(plsr2$R2+plsr2$Q2[,3],decreasing=T,index=T)$ix[1:3]
	flag <- sort(flag)
	#### scatterplot3d(time,ages,sex,pch=16)
	pdf(file=paste(filename,".pdf",sep=""))	## 高清图
	par(cex=1.2,font=1.5,col=1)
	scatterplot3d(plsr2$x.scores[,flag[1]],plsr2$x.scores[,flag[2]],plsr2$x.scores[,flag[3]],
		angle=30,scale.y=0.6,box=FALSE,
		xlab=paste("t[",flag[1],"]",sep=""),
		ylab=paste("t[",flag[2],"]",sep=""),
		zlab=paste("t[",flag[3],"]",sep=""),
		color=col2[as.numeric(Y)],
		pch=pch2[as.numeric(Y)])
	#abline(h=0,v=0,col=8)
	legend("topright", legend=label2, 
		col=col2,pch=pch2,lwd=2)
	dev.off() ##
	return(plsr2)
} # end
  

  
## VIP ## VIP由plsreg2得到，而且选用mean
vip <- function(X=modelX,Y=modelY) {
	dummy13 <- y2dummy(Y)
	colnames(dummy13) <- paste("dummy",1:ncol(dummy13),sep="")
	plsr13 <- plsreg2(X,dummy13,comps =10)	#判断最优成分个数
	#nc <- which.max(plsr13$Q2cum[,ncol(dummy13)+1])
	#vip <- apply(plsr13$VIP[,1:nc],1,mean)
	# R plsreg2做 t5跟simca一样,代表t5是前5个PC的累积
	vip <- apply(plsr13$VIP,1,mean)
	#names(vip) <- colnames(X)
	return(round(vip,2))
}

###################################################
## PLSREG2 三分类
pls3 <- function(X=modelX,Y=modelY,
	col4=c("red","blue", "green","black"),
	pch4=c(19,2,12,18),
	label4=c("EOC","BOT","Normal","post-operation"),
	filename="PLS3-scoreplot3"
) {
	dummy <- y2dummy(Y)
	colnames(dummy) <- paste("dummy",1:ncol(dummy),sep="")
	#dummy1 <- dummy[sample(1:nrow(dummy)),]
	plsr3 <- plsreg2(X,dummy,comps =10)
	plsr3$Q2;plsr3$Q2cum # plot(plsr2)
	## 多分类scoreplot使用plsreg2
	pdf(file=paste(filename,".pdf",sep=""))	## 高清图
	par(cex=1.1,font=1.5,col=1)
	scatterplot3d(plsr3$x.scores[,1],plsr3$x.scores[,2],plsr3$x.scores[,4],
		angle=30,scale.y=0.6,box=FALSE,
		xlab="t[1]",ylab="t[2]",zlab="t[3]",
		color=col4[as.numeric(Y)],
		pch=pch4[as.numeric(Y)])
	#abline(h=0,v=0,col=8)
	legend("topright", legend=label4, 
		col=col4,pch=pch4, lwd=2)
	dev.off() ##
	return(plsr3)
}