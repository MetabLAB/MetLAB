####################################################################
## ktest function 完全随机两组
ktest <- function(X=modelX,Y=modelY) {
	ktest123 <- numeric(ncol(X))
	for (i in 1:ncol(X)) {
		ktest123[i] <- kruskal.test(X[,i],Y)$p.value
	}
	ktest123 <- as.numeric(format(ktest123,digits=3,nsmall=3))
	names(ktest123) <- colnames(X)	
	return(ktest123)
}
## 配对秩和检验
wtest <- function(X=modelX,Y=modelY) {
	wtest123 <- numeric(ncol(X))
	for (i in 1:ncol(X)) {
		wtest123[i] <- wilcox.test(modelX14[,i]~modelY14,paired=T)$p.value
	}
	wtest123 <- as.numeric(format(wtest123,digits=3,nsmall=3))
	names(wtest123) <- colnames(X)	
	return(wtest123)
}
##############################################################
## uAUC function
uAUC <- function(X=modelX,Y=modelY) {
	library(pROC)
	uAUC13 <- numeric(ncol(X))
	system.time(
	for (i in 1:ncol(X)) {
		uAUC13[i] <- auc(factor(Y),X[,i])
	} )
	uAUC13[uAUC13<=0.5] <- 1-uAUC13[uAUC13<=0.5]	## range 0.5~1
	names(uAUC13) <- colnames(X)
	return(round(uAUC13,2))
}
#########################################################

## fold change
fc <- function(X=modelX,Y=modelY) {
	X[X==0] <- 1e-4
	#X <- log(X,base=2)
	fc1 <- numeric(ncol(X))
	for (i in 1:ncol(X)) {
		mean12 <- tapply(X[,i],Y,mean)
		fc1[i] <- mean12[1]-mean12[2]
	}
	names(fc1) <- colnames(X)
	return(round(fc1,2))
}