  #-------------------------------------------------------
  # func name: PCA analysis and 2d/3d Scoreplot
  # param: X, Y 
  # sytax:
  # source("D:/快盘/Func/R/pca.R")
  # pca(X=mX,Y=mY,label4=c("SZ","CT","QC"),d3=2)
  #-------------------------------------------------------

pca <- function(X=modelX,Y=modelY,
	col4=c("red","green", "blue","black"),
	pch4=c(19,12,2,18),
	label4=c("EOC","BOT","Normal","post-operation"),
	d3=3
) {
require(scatterplot3d)
	pc1 <- prcomp(data.frame(X),retx=TRUE,center=TRUE,scale = TRUE)
	# pc1$x; pc1$scores
	if (d3==2) {
	pdf(file="PCAscoreplot2.pdf",width=12,height=9)	## 高清图
	par(cex=1.5,font=1.5,col=1)
	plot(pc1$x[,1:2],col=col4[as.integer(Y)], xlim=c(-30,30),
		xlab="1st Principal Component",ylab="2nd Principal Component",
		pch=pch4[as.numeric(Y)])
	legend("bottomleft", legend=label4, 
		col=col4,pch=pch4, lwd=2)
  #加上椭圆
  abline(h=0,v=0,lty=2,lwd=1.5)
  require(ellipse)
	lines(ellipse(0,scale = c(sd(pc1$x[,1]), sd(pc1$x[,2])),
                centre = c(mean(pc1$x[,1]), mean(pc1$x[,2]))), lty=2,lwd=1.5)
	dev.off() ##
	}
	if (d3==3) { ## 3d plot
	pdf(file="PCAscoreplot3.pdf")	## 高清图
	par(cex=1.2,font=1.5,col=1)
	scatterplot3d(pc1$x[,1:3],
		angle=30,scale.y=0.6,box=FALSE,
		xlab="PC[1]",ylab="PC[2]",zlab="PC[3]",
		color=col4[as.numeric(Y)],
		pch=pch4[as.numeric(Y)])
	#abline(h=0,v=0,col=8)
	legend("topright", legend=label4, 
		col=col4,pch=pch4, lwd=2)
	dev.off() ##
	}
	return(pc1)
}
#pca(X=,Y=,label4=c(""),d3=2)
