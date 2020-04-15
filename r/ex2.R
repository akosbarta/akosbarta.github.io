#Problem 1

numbers <- c(3,5,8,10,12) 
save(numbers, file="numbers.RData") 
rm("numbers") 
ls()
load("numbers.RData")
numbers

#Problem 2
curve(.5*(x-1)^2-2,xlim=c(-3,4))
abline(v=0, h=0)

curve(x^3-x^2-8*x+12,xlim=c(-4,4))
abline(v=0, h=0)

#Problem 3
#(a)
a3 <- function(x) {
  if(x<0) {
    -1
  }
  else if(x>0) {
   1
  }
}
print(a3(11))

#(b)
b3 <- function(x) {
if(abs(x)>pi) {
  TRUE
}
else {
  FALSE
}
}
print(b3(4))

#(c)
c3 <- function(var1,var2) {
  sqrt(prod(var1,var2))
}
c3(4,8)

#(d)
d3 <- function(a,b) {
  remainder <- a%%b
  if (a > 0 && b > 0 && remainder == 0) {
    TRUE
  }
  else {
    FALSE
  }
}
d3(50,10)

#(e)
e3 <- function(n) {
  if(n>=0&&n%%1==0) {
  prod(1:n)
  }
  else {
    FALSE
  }
}
print(e3(8))

#Problem 4

ssr <- function(x, a) {
  sumvar <- 0
  for (i in seq(1,length(x))) {
    sumvar <- (x[i]-a)^2 + sumvar
  }
  print(sumvar)
}
x <- c(1, 5, 6, 8, 7, 0, 10, 12, 2, 5)
a <- seq(4.4,6.8,by=.2)
ssr(x,a)
plot(a,ssr(x,a),type="b")

abline(min(ssr(x,a)),0,col='#ff0000')

#Problem 5
#(a)
cost <- function (pieces) { 270000 + 20*pieces }
revenue <- function (pieces) { pieces*30 }

#(b)
pieces <- 0:30000
plot(pieces,cost(pieces),type="l")
lines(pieces,revenue(pieces))

#(c)
# cost [270000,n+[
# rev [0,n+[

#(d)
profit <- function (pieces) { revenue(pieces) - cost(pieces) }

#(e)
pieces[which(profit(pieces)==0)]

#(f)
profit(25000)

#(g)
# Don't.

#Problem 6
#Paper based

#Problem 7
#(a)
#print(polyroot(c(1,0,1,0,-6)))
#plot( function(x) x^4+x^2-6 , -2, 2) ; abline(h=0,v=0,lty=3)

polynomial <- function(coef,from,to) {
  if(!is.character(coef)) {
    equ <- 0
    for(i in seq(1,length(coef))) {
      tempequ <- paste(coef[i],"*x^",i-1,sep="")
      equ <- paste(equ,tempequ,sep="+")
    }
    print(equ)
    roots=polyroot(coef)
    realequ <- function(x) { eval(parse(text=equ)) }
    curve(realequ,xlim=c(from,to))
    abline(0,0,col='red')
    realroot <- Re(roots)[abs(Im(roots)) < 1e-6]
    print(realroot)
    text(realroot,1,label=round(realroot,4),adj=c(1,-1),col='blue')
  } else {
    pos <- unlist(gregexpr(pattern='x', coef))
    coefvector <- c()
    degreevector <- c()
    for(i in seq(1,length(pos))) {
      degreevector <- c(degreevector,as.numeric(substr(coef,pos[i]+2,pos[i]+2)))
    }
    for (i in seq(1,max(degreevector)+1)) {
      coefvector <- c(coefvector,0)
    }
    for(i in seq(1,length(pos))) {
      degree <- as.numeric(substr(coef,pos[i]+2,pos[i]+2))
      if (substr(coef,pos[i]-1,pos[i]-1) == "*") {
        coefvector[degree+1] <- as.numeric(substr(coef,pos[i]-3,pos[i]-2))
      } else {
        coefvector[degree+1] <- 1
      }
      print(coefvector[degree+1])
    }
    equ <- 0
    for(i in seq(1,length(coefvector))) {
      tempequ <- paste(coefvector[i],"*x^",i-1,sep="")
      equ <- paste(equ,tempequ,sep="+")
    }
    print(equ)
    realequ <- function(x) { eval(parse(text=equ)) }
    print(coefvector)
    roots=polyroot(coefvector)
    curve(realequ,xlim=c(from,to))
    abline(0,0,col='red')
    realroot <- Re(roots)[abs(Im(roots)) < 1e-6]
    print(realroot)
    text(realroot,1,label=round(realroot,4),adj=c(1,-1),col='blue')
  }
}
polynomial(c(20,1,5,1,3,2),-4,6)
polynomial("1*x^3-x^2-5*x^2+x^1-20*x^0",-4,6)
abline(0,0,col='red')

#Problem 8
t <- 0
plot(function(x) x^2+4*sqrt(t)*x+t^2,xlim=c(-10,5),ylim=c(-15,20),col='green')
for (t in seq(.1,10,by=.1)) {
  if (t > 4) {
    plot(function(x) x^2+4*sqrt(t)*x+t^2,xlim=c(-10,5),ylim=c(-15,20),add=TRUE,col='#ff0000')
  } else {
    plot(function(x) x^2+4*sqrt(t)*x+t^2,xlim=c(-10,5),ylim=c(-15,20),add=TRUE,col='#00ff00')
  }
}
abline(0,0,col='#333333')
legend("bottomright",c("Intersects the x-axis or \"touches\" it (0 <= t <= 4)","Doesn't intersect the x-axis (t > 4)"),col=c("#00ff00","#ff0000"),lty=c(1),lwd=c(1))
