##### 1) calculate self correlation of a matrix with no NA element#####
x <- c(-2, -1, 0, 1, 2)
y <- c(4,   1, 0, 1, 4)
z <- c(1,   2, 3, 4, 9)
v <- c(1,   2, 3, 4, 5)
cbind(x,y,z,v) #5(sample/observation) by 4 features

result <- rcorr(cbind(x,y,z,v))
result$r #matrix of correlation with 4 by 4
result$n #number of observations used in analyzing each pair of variables
result$p

cor(x,z) #this number is equal to the result$r[1,2]
cor(y,z) #this number is equal to the result$r[2,3]
#So when the y vector or matrix is omitted, the rcorr calculate the paired correlation between columns


########2) calculate self correlation of a matrix with with NA element#####
x <- c(-2, -1, 0, 1, 2)
y <- c(4,   1, 0, 1, 4)
z <- c(1,   2, 3, 4, NA)
v <- c(1,   2, 3, 4, 5)
cbind(x,y,z,v) #5(sample/observation) by 4 features

result <- rcorr(cbind(x,y,z,v))
result$r #matrix of correlation with 4 by 4
result$n #As there is a NA element in vector Z, so only 4 elements were used to calculate the correlation
result$P

cor(x[1:4],z[1:4]) #this number is equal to the result$r[1,2]
cor(y[1:4],z[1:4]) #this number is equal to the result$r[2,3]

########2) calculate correlation of two matrice#####
x1 <- c(-2, -1, 0, 1, 2)
y1 <- c(4,   1, 0, 1, 4)
z1 <- c(1,   2, 3, 4, NA)
v1 <- c(1,   2, 3, 4, 5)
a <- cbind(x1,y1,z1,v1) #5(sample/observation) by 4 features

x2 <- c(0,-2,3,3,5)
y2 <- c(3,-1,4,8,-4)
z2 <- c(-2,1,0,1,2)
v2 <- c(-8,-6,3,5,9)
b <- cbind(x2,y2,z2,v2)#5(sample/observation) by 4 features

result <- rcorr(a,b)
result$r #matrix of correlation with 8 by 8
result$n #As there is a NA element in vector Z, so only 4 elements were used to calculate the correlation
result$P

cor(x1,x2)

