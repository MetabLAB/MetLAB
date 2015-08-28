#R packages and functions
###############################################################
####K-nearest Neighbor Classification: ##
#The class package
#Function: knn
#Parameters: k=1 and k=3 tested
#Linear Discriminant Analysis:
#The MASS package
#Functions: lda, predict.lda
## Maximum Likelihood: ##
#Implemented in the R language (see below)
## Nearest Centroid:
#Implemented in the R language (see below)
##Principal Component Analysis:
#The mva package
#Function: princomp, prcomp, 
#Variations: fast.prcomp and fast.svd (see below)
#Parameters: number of first PC to include as optional input for classifiers: Z={2,4,6,8,10,12}
###Support Vector Machines:
#The e1071 package
#Functions: svm, predict.svm
#Parameters: scale=TRUE, type=C-classification, kernel = radial (RBF kernel function), Shrinking=TRUE, cost= ¡®estimated values: C=2^(c*2-4), where c={3, 5, 7, 9}¡¯
##T-test:
#The ctest package
#Function: t.test
##############################################################
#Scripts not in packages:
##Principal Component Analysis (PCA)
#When performing PCA of all genes, an alternative implementations of principal component analysis was used which involve the two functions: fast.prcomp and fast.svd.

fast.prcomp <- function (x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL) {
x <- as.matrix(x)
x <- scale(x, center = center, scale = scale.)
s <- fast.svd(x, nu = 0)
if (!is.null(tol)) {
rank <- sum(s$d > (s$d[1] * tol))
if (rank < ncol(x)) 
s$v <- s$v[, 1:rank, drop = FALSE]
}
s$d <- s$d/sqrt(max(1, nrow(x) - 1))
dimnames(s$v) <- list(colnames(x), paste("PC", seq(len = ncol(s$v)), 
sep = ""))
r <- list(sdev = s$d, rotation = s$v)
if (retx) 
r$x <- x %*% s$v
class(r) <- "prcomp"
r
}


fast.svd <- function( x, nu = min(n, p), nv = min(n, p)) {
x <- as.matrix(x)
dx <- dim(x)
n <- dx[1]
p <- dx[2]
if( p <= n )
return( svd( x, nu, nv ) )
else
{
s <- svd( t(x), nu=nv, nv=nu)
retval <- list()
retval$d <- s$d
retval$u <- s$v
retval$v <- s$u
return(retval)
}
}

##############################################################
### Nearest centroid classification
# matrix: matrix with experiments in columns and genes in rows
# class: vector with classes of experiments
# x: the experiment number to test
# y: number of genes

nearest.centroid <- function(matrix,class,x,y) { 
matrix.train <- matrix[-x,]
class.train <- factor(class[-x])
index <- 1:length(class.train)
class1 <- index[class.train==levels(class.train)[1]] # Category 1
class2 <- index[class.train==levels(class.train)[2]] # Category 2
# class 1
for (i in 1:y) {
avg = sum(matrix.train[class1,i])/length(matrix.train[class1,i])
if (i == '1') {
dist1 = (avg-matrix[x,i])^2
}
if (i > '1') { 
dist1 = dist1 + (avg-matrix[x,i])^2
}
}
# class 2
for (i in 1:y) {
avg = sum(matrix.train[class2,i])/length(matrix.train[class2,i])
if (i == '1') { 
dist2 = (avg-matrix[x,i])*(avg-matrix[x,i])
}
if (i > '1') {
dist2 = dist2 + (avg-matrix[x,i])^2
}
}
if (dist2 > dist1) {
return(levels(class.train)[1])
}
if (dist2 < dist1) {
return(levels(class.train)[2])
}
}
# nearest.centroid(matrix,class,x,y)
##############################################################
## Maximum Likelihood Classifier
# matrix: matrix with experiments in columns and genes in rows
# class: vector with classes of experiments
# x: the experiment number to test
# y: number of genes

maximum.likelihood <- function(matrix,class,x,y) {
matrix.train <- matrix[-x,]
class.train <- factor(class[-x])
index <- 1:length(class.train)
class1 <- index[class.train==levels(class.train)[1]] # Category 1
class2 <- index[class.train==levels(class.train)[2]] # Category 2
# class 1
for (i in 1:y) {
# Central estimate of pooled within-class variation
variance <- 1/(length(class.train)-2) * ( (length(class2)-1) * (sd(matrix.train[class2,i]))^2 + (length(class1)-1) * (sd(matrix.train[class1,i]))^2 )
mean <- sum(matrix.train[class1,i])/length(matrix.train[class1,i])
if (i == '1') {
dist1 = (mean-matrix[x,i])^2/variance }
if (i > '1') {
dist1 = dist1 + (mean-matrix[x,i])^2/variance } 
}
# class 2
for (i in 1:y) {
variance <- 1/(length(class.train)-2) * ( (length(class2)-1) * (sd(matrix.train[class2,i]))^2 + (length(class1)-1) * (sd(matrix.train[class1,i]))^2 )
mean <- sum(matrix.train[class2,i])/length(matrix.train[class2,i])
if (i == '1') {
dist2 = (mean-matrix[x,i])^2/variance }
if (i > '1') { 
dist2 = dist2 + (mean-matrix[x,i])^2/variance }
}
if (dist1 > dist2) {
return(levels(class.train)[2]) }
if (dist1 < dist2) {
return(levels(class.train)[1]) }
}


