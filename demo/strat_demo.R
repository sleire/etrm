library(ggplot2)
library(gridExtra)
library(reshape2)
library(gtable)
library(grid)

# source
source("../etrm/R/strategy_classes.R")
source("../etrm/R/cppi_strategy.R")
source("../etrm/R/dppi_strategy.R")
source("../etrm/R/shpi_strategy.R")
source("../etrm/R/obpi_strategy.R")
source("../etrm/R/slpi_strategy.R")

# load data
load("../etrm/data/ENOYR.Rda")

############# SHPI
# y6
y6 <- tail(ENO$ENOYR.06[!is.na(ENO$ENOYR.06)],200)
d6 <- tail(ENO$Date[!is.na(ENO$ENOYR.06)],200)

sh6 <- shpi(q=100,f=y6,tper=0.1,tdate=d6,daysleft=(length(y6)))
#c6 <- cppi(q=100, tper=0.1,f=y6,rper=rep(0.2,length(y6)),tdate=d6)
plot(sh6)


# y14
y14 <- tail(ENO$ENOYR.14[!is.na(ENO$ENOYR.14)],200)
d14 <- tail(ENO$Date[!is.na(ENO$ENOYR.14)],200)

sh14 <- shpi(q=-100,f=y14,tper=-0.05,tdate=d14,daysleft = length(y14))
plot(sh14)

############# SLPI
# y6
y6 <- tail(ENO$ENOYR.06[!is.na(ENO$ENOYR.06)],200)
d6 <- tail(ENO$Date[!is.na(ENO$ENOYR.06)],200)

s6 <- slpi(q=100,f=y6,tper=0.1,tdate=d6)
#c6 <- cppi(q=100, tper=0.1,f=y6,rper=rep(0.2,length(y6)),tdate=d6)
plot(s6)


# y14
y14 <- tail(ENO$ENOYR.14[!is.na(ENO$ENOYR.14)],200)
d14 <- tail(ENO$Date[!is.na(ENO$ENOYR.14)],200)

s14 <- slpi(q=100,f=y14,tper=0.1,tdate=d14)
plot(s14)


############# CPPI
# y6
y6 <- tail(ENO$ENOYR.06[!is.na(ENO$ENOYR.06)],200)
d6 <- tail(ENO$Date[!is.na(ENO$ENOYR.06)],200)

c6 <- cppi(q=-100, tper=-0.1,f=y6,tdate=d6)
#c6 <- cppi(q=100, tper=0.1,f=y6,rper=rep(0.2,length(y6)),tdate=d6)
plot(c6)


# y14
y14 <- tail(ENO$ENOYR.14[!is.na(ENO$ENOYR.14)],200)
d14 <- tail(ENO$Date[!is.na(ENO$ENOYR.14)],200)

c14 <- cppi(q=-100, tper=-0.1,f=y14,tdate=d14)
plot(c14)

############# DPPI
# y6
y6 <- tail(ENO$ENOYR.06[!is.na(ENO$ENOYR.06)],200)
d6 <- tail(ENO$Date[!is.na(ENO$ENOYR.06)],200)

dp6 <- dppi(q=100, f=y6,tdate=d6)
plot(dp6)


# y14
y14 <- tail(ENO$ENOYR.14[!is.na(ENO$ENOYR.14)],200)
d14 <- tail(ENO$Date[!is.na(ENO$ENOYR.14)],200)

d14 <- dppi(q=100, tper=0.1,f=y14,tdate=d14)
plot(d14)

############# OBPI

# y6
y6 <- tail(ENO$ENOYR.06[!is.na(ENO$ENOYR.06)],200)
d6 <- tail(ENO$Date[!is.na(ENO$ENOYR.06)],200)

o6 <- obpi(q=100, f=y6,tdate=d6,vol=0.2,daysleft=length(y6))
plot(o6)


# y14
y14 <- tail(ENO$ENOYR.14[!is.na(ENO$ENOYR.14)],200)
d14 <- tail(ENO$Date[!is.na(ENO$ENOYR.14)],200)

o14 <- obpi(q=100, f=y14,tdate=d14,vol=0.2,daysleft=length(y6))
plot(o14)

