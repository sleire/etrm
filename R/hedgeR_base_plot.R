# base R plot of heging strategies

bt <- o14

# test data
hed <- bt@Results$HedgeRate
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
     xlab="",
     ylab="Price",
     #ylim=c(10,100),
     main = "Strategy plot in base R")
#par(bg = "grey")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
grid(nx = NULL, col = "white", lty =1)
lines(date, mar, col = "indianred")
lines(date,tar,col="black", lty =2)
lines(date,por,col="steelblue")

par(new=TRUE)
plot(date,hed*100,type="l",col="lightsteelblue3",
     ylim = c(0,100), xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Hedge %",side=4,line=3)
#legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))

add_legend("topright",
           legend=c("Market", "Target", "Portfolio", "Hedge"),
           pch=20,
           col=c("indianred","black", "steelblue", "lightsteelblue3"),
           horiz=TRUE, bty='n', cex=0.8)

# for hedge area plot
# https://github.com/VictoriaLynn/plotting-examples/blob/master/fill_between/fill_between.R

