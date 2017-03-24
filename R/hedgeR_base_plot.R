# base R plot of heging strategies

bt <- o14

# test data
hed <- bt@Results$HedgeRate*100
date <- bt@Results$Date
mar <- bt@Results$Price
tar <- bt@Results$Target
por <- bt@Results$PortfPrice

# add legend function
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

par(mar=c(5,4,4,5)+.1)
plot(date,
     mar,
     type="l",
     col = "gray",
     yaxt = "n",
     xlab="Price",
     ylab="",
     main = "Strategy plot in base R")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
grid(nx = NULL, col = "white", lty =1)
opar <- par()
par(new=TRUE)
plot(date,hed,type="l",col="lightsteelblue3",
     ylim = c(0,100), xaxt="n",yaxt="n",xlab="",ylab="")
polygon(c(date[1],date,tail(date, 1)),
        c(0,hed,0),col='lightsteelblue3', border = NA)
axis(4)
par(opar)
lines(date, mar, col = "indianred")
lines(date,tar,col="black", lty =2)
lines(date,por,col="steelblue")
axis(2)


par(new=TRUE)
plot(date,hed,type="l",col="lightsteelblue3",
     ylim = c(0,100), xaxt="n",yaxt="n",xlab="",ylab="")
polygon(c(date[1],date,tail(date, 1)),
         c(0,hed,0),col='lightsteelblue3', border = NA)
#polygon(date,hed,col='lightsteelblue3', border = NA)
axis(4)
mtext("Hedge %",side=4,line=3)
#legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))
grid(nx = NULL, col = "white", lty =1)

add_legend("topleft",
           legend=c("Market", "Target", "Portfolio", "Hedge"),
           pch=20,
           col=c("indianred","black", "steelblue", "lightsteelblue3"),
           horiz=TRUE, bty='n', cex=0.8)



par(mar=c(3,5,3,5)+.1)
plot(date,
     hed,
     type="l",
     col = "gray",
     yaxt = "n",
     xlab="",
     ylab="",
     ylim=c(0,100),
     main = "Strategy plot in base R")
axis(4)
mtext("Hedge %",side=4,line=3)
# polygon(c(date[1],date,tail(date, 1)),
#         c(0,hed,0),col='lightsteelblue3', border = NA)
#par(bg = "grey")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
# grid(nx = NULL, col = "white", lty =1)

polygon(c(date[1],date,tail(date, 1)),
        c(0,hed,0),col='lightsteelblue3', border = NA)
#grid(nx = NULL, col = "white", lty =1)
par(new=TRUE)
plot(date, mar, col = "indianred", type = "l", ylab = "Price",xlab = "")
lines(date,tar,col="black", lty =2)
lines(date,por,col="steelblue")
grid(nx = NULL, col = "white", lty =1)

#legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))
