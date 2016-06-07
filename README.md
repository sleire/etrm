# hedgeR: Portfolio insurance strategies for commodity price risk management
Futures trading strategies for both long and short hedgers. All models below aim to reduce (increase) the commodity portfolio price, while preventing it from breaching a pre defined cap (floor).

Portfolio insurance strategies implemented as functions:

- cppi(): Constant Proportion Portfolio Insurance (CPPI)
- dppi(): Dynamic Proportion Portfolio Insurance (DPPI)
- obpi(): Option Based Portfolio Insurance (OBPI)
- shpi(): Step Hedge Portfolio Insurance (SHPI)
- vbpi(): VaR Based Portfolio Insurance (VBPI)
- slpi(): Stop Loss Portfolio insurance (SLPI)

### Dynamic hedging strategies for
- Long and short hedgers
- Back testing and decision support for traded contracts

### Install from GitHub
```
install.packages("devtools")  
library(devtools)
install_github("sleire/hedgeR")
```
