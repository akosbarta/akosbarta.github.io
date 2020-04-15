#Problem 1
#(a)
seq(48,14)^2
#(b)
seq(3,100)^2*pi

#Problem 2
#(a)
rep(seq(0,4),each=5)
#(b)
rep(seq(1,5),times=5)
#(c)
rep(seq(0,4),each=5) + rep(seq(1,5),times=5)

#Problem 3
#(a) 

#(b)
100000/(1+seq(1,10)/100)^5

#Problem 5

for(n in seq(10,40,by=10)) {
  print(sum(1.08^seq(0,n)))
}

for(n in seq(10,40,by=10)) {
  print((1-1.08^(n+1))/(1-1.08))
}

mysum <- sum((1-1.08^(seq(0,100)+1))/(1-1.08))
print(mysum)

#Problem 6
#(a)
sum(seq(1:10)^2)

#(b)

x <- seq(1,5*10^8)^2
sum(as.numeric(x))

n <- 5*10^8
(1/6)*n*(n+1)*(2*n+1)

#Problem 7
n7 <- seq(1,10)
#(a)
n7[which(abs(4-3*n7)<5)]
#(b)
n7[which(4<abs(1-2*n7)&abs(1-2*n7)<10)]
#(c)
n7[which(abs(n7^2-2)<4)]
#(d)
n7[which(abs(7-2*n7) == 1)]

#Problem 8

s <- c(98.85,98.55,100.31,97.34,99.81,99.90,101.87,99.22,100.28,98.02,100.75,102.29
       ,98.68,99.39,101.58,101.01,98.23,99.15,99.16,101.14,100.21,101.26,100.23,101.49
       ,99.52,98.03,101.62,102.29,100.29,98.67)
#(a)
s[1]
s[length(s)]
#(b)
s[seq(2,length(s),by=2)]
#(c)
cseq <- c(s[seq(1,15)]^2,s[seq(16,30)]/2)
print(cseq)
#(d)
sum(s[seq(5,length(s),by=5)])
#(e)
index1 <- c(4,7,16,21)
value1 <- s[index1]^2
index2 <- c(1,8,17,23)
value2 <- mean(s[index2])
print(value1)
print(value2)
print(value1-value2)
#(f)
print(s[seq(2,length(s)-1)])
#(g)
s[seq(2,30)]-s[seq(1,29)]
