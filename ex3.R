#Problem 1

x <- seq(.01,2,.01)

#(a)
plot(x,x,ylim=c(-12,10),col="black",type="p")
lines(x, sqrt(x),col="blue",type="p")
lines(x, x^2,col="red",type="p")
lines(x, x^3,col="green",type="p")
lines(x, exp(x) - 1,col="orange",type="p")

vertical <- seq(0,2,by=0.5)
abline(v=vertical,lty="dashed")

#(b)
plot(x, log(x),ylim=c(-12,3),col="black",type="p")
lines(x, log(sqrt(x)),col="blue",type="p")
lines(x, log(x^2),col="red",type="p")
lines(x, log(x^3),col="green",type="p")
lines(x, log(exp(x)),col="orange",type="p")
vertical <- seq(0,2,by=0.5)
abline(v=vertical,lty="dashed")


#Problem 2

#(a) (b)

relDiff <- function(x1=100,x2,x3) {
  if (is.numeric(x1)&&is.numeric(x2)&&is.numeric(x3)) {
    c((x2-x1)/x1,(x3-x2)/x2)
  }
}

relDiff(80, 50, 100)
relDiff(x3 = 10, x2 = 40)
relDiff(70, 90)

relDiff 

#(c)

relDiff <- function(sequence) {
  returned <- c()
  for(i in seq(2,length(sequence))) {
    returned <- paste(returned,round((sequence[i] - sequence[i-1])/sequence[i-1],3))
  }
  returned
}

relDiff(seq(1,10,by=1))

#(d)
x <- seq(1,100)
y1 <- diff(x)
y2 <- relDiff(x)
y3 <- diff(log(x))
y4 <- relDiff(exp(x))

print(y1)
print(y2)
print(y3)
print(y4)

#Problem 3
#(a)
powerFun <- function(x,a,r) {
  if (x > 0 && is.numeric(x) && is.numeric(a) && is.finite(r)) {
    a*x^r
  }
}

powerFun(3,1,sqrt(2))

#(b)
x <- seq(.5, 2, .01)
a <- 1
r <- c(-2,-1,-0.5,0.5,1,2)

plot(x,powerFun(x,1,r),type="p",xlim=c(0,2),ylim=c(0,4))

#(c)
lines(log(x), log(powerFun(x,a,r)),col="red",type="p")
#They become linear.

#Problem 4
polyEval <- function(x,a) {
  if (is.numeric(x) && is.vector(a)) {
    result <- 0
    for (ai in seq(0,length(a)-1)) {
      result <- a[ai+1]*x^(ai) + result
    }
    result
  }
}

polyEval(2, c(2,3,1))

#Problem 5
curve(exp(x),xlim=c(-10,10),ylim=c(-20,20))
abline(h=0,col="red")
abline(v=0,col="red")
curve(-exp(x),add=TRUE)
curve(-exp(x+2),add=TRUE)
curve(-1.5*exp(x+2),add=TRUE)
curve(-1.5*exp(-x+2)-1,add=TRUE,col="green")

#Problem 6
#(a)

#(b)

#(c)
rm(x)
curve(exp(-x^2)+x/5-0.7,xlim=c(-10,10))
abline(h=0,lty="dashed")
uniroot(function(x)exp(-x^2)+x/5-0.7,lower=-10,upper=10)
abline(v=3.5,lty="dotted")
abline(v=-0.4781401,lty="dotted")
abline(v=0.7803416,lty="dotted")

#Problem 7

func7 <- function(x) {
  exp(-(x-2)^2)
}

curve(func7,xlim=c(-10,10))
optimize(func7, interval=c(-10, 10), maximum=TRUE)

#Problem 8

t1 <- function(i) {
  ifelse(i <= 30,i*.2,30*.2 + (i-30)*.4)
}

t2 <- function(i) {
  ifelse(i <= 20,i*.15,ifelse(i > 30,20*.15 + 10*.3 + (i-30)*.6,20*.15 + (i-20)*.3))
}
#(a)
for (x in seq(0:100)) {
  if (t1(x) == t2(x)) {
    print(x)
  }
}
t1(30)
t2(30)

#(b)
curve(t1(x),xlim=c(0,50))
curve(t2(x),xlim=c(0,50),add=TRUE,col="red")

#(c)
# The first one favours the rich.
# The second one favours the poor.

#(d)
limit <- c(0,100)
curve(t1(x),xlim=limit,lwd=2)
curve(t1(x-2),xlim=limit,lty="dashed",lwd=2,add=TRUE)
curve(t1(x)-2,xlim=limit,lty="dotted",lwd=2,add=TRUE)
curve(t2(x),xlim=limit,add=TRUE,col="red",lwd=2)
curve(t2(x-2),xlim=limit,add=TRUE,col="red",lwd=2,lty="dashed")
curve(t2(x)-2,xlim=limit,add=TRUE,col="red",lwd=2,lty="dotted")

#(e)
curve(t1(x),xlim=limit,lwd=2,col="darkgreen",add=TRUE)
curve(t1(x*.95),xlim=limit,lty="dashed",lwd=2,add=TRUE,col="darkgreen")
curve(t1(x)*.9,xlim=limit,lty="dotted",lwd=2,add=TRUE,col="darkgreen")
curve(t2(x),xlim=limit,add=TRUE,col="orange",lwd=2)
curve(t2(x*.95),xlim=limit,add=TRUE,col="orange",lwd=2,lty="dashed")
curve(t2(x)*.9,xlim=limit,add=TRUE,col="orange",lwd=2,lty="dotted")

#(f)
#The best tax reduction plan for people with lower incomes is obviously the one where the government deducts 2k off the calculated tax.
