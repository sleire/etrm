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

ymin <-min(c(mar,tar,por))
ymax <- max(c(mar,tar,por))

#par(mar=c(5,4,4,5)+.1)
par(mar=c(4,4.5,3,4.5))
plot(date,
     mar,
     type="l",
     col = "gray",
     yaxt = "n",
     xlab="",
     ylab="",
     main = "Strategy plot in base R",
     ylim = c(ymin, ymax))
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
lines(date,por,col="steelblue4")
axis(2)
mtext("Price",side=2,line=3)
mtext("Hedge %",side=4,line=3)

#legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))
#grid(nx = NULL, col = "white", lty =1)

# add_legend("bottom",
#            legend=c("Market", "Target", "Portfolio", "Hedge"),
#            pch=20,
#            col=c("indianred","black", "steelblue4", "lightsteelblue3"),
#            horiz=TRUE, bty='n', cex=0.8)

#http://stackoverflow.com/questions/11794436/stacking-multiple-plots-vertically-with-the-same-x-axis-but-different-y-axes-inhttp://stackoverflow.com/questions/11794436/stacking-multiple-plots-vertically-with-the-same-x-axis-but-different-y-axes-in
