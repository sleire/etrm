# hedgeR: Commodity Market Risk Management in R

test

### Maximum Smoothness Forward Curve function:
A typical characteristic of some energy markets (such as electricity and natural gas) is a delivery of the commodity over a period, instead of a delivery on a single date. A smooth forward curve is an essential tool for pricing non-standard OTC contracts having any settlement period.

The function
- mcfc() - Maximum Smoothness Forward Curve 

outputs an S4 object of type "MSFC", with methods "show", "summary" and "plot".

### Commodity portfolio insurance strategy functions:
Futures trading strategies for commercial hedgers with long or short exposure. All models below aim to reduce (increase) the commodity portfolio price, while preventing it from breaching a pre defined cap (floor).

- cppi() - Constant Proportion Portfolio Insurance   
- dppi() - Dynamic Proportion Portfolio Insurance   
- obpi() - Option Based Portfolio Insurance         
- shpi() - Step Hedge Portfolio Insurance            
- slpi() - Stop Loss Portfolio insurance             

Output S4 objects of type "CPPI", "DPPI", "OBPI", "SHPI" and "SLPI", with methods "show", "summary" and "plot".

### Dynamic hedging strategies for
- Long and short hedgers
- Back testing and decision support for traded contracts

### Install from GitHub
```
install.packages("devtools")  
library(devtools)
install_github("sleire/hedgeR")
```
