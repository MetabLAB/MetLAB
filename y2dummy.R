## multiclass Y to dummy variables
## Y可以是字符和数值
y2dummy <- function(Y) {
	dummy <- matrix(0,nrow=length(Y),ncol=length(table(Y)))
	for (i in 1:length(Y)) {
		for (j in 1:ncol(dummy)) {
			if (Y[i]==names(table(Y))[j]) dummy[i,j]=1
		}
	}
	colnames(dummy) <- paste("dummy",1:ncol(dummy),sep="")
	return(dummy)
}
## try
# Y <- sample(c("A","B","C"),5,rep=T)
# dummy <- y2dummy(Y)