# etrm: Energy Trading and Risk Management in R


### Maximum Smoothness Forward Curve.
A typical characteristic of energy commodities such as electricity and natural gas is that delivery takes place over a period in time, not on a single date. Listed futures contracts cover standardised periods, such as "Day", "Week", "Month", "Season" or "Year". A smooth forward curve is an essential tool for pricing non-standard OTC contracts having any settlement period.The function

- mcfc() - Maximum Smoothness Forward Curve 

outputs an S4 object of type "MSFC", with methods "show", "summary" and "plot".

### Commodity portfolio insurance strategies.
Futures trading strategies for price risk management, for commercial hedgers with long or short exposure. All models below aim to reduce (increase) the commodity portfolio price, while preventing it from breaching a pre defined cap (floor). The functions

- cppi() - Constant Proportion Portfolio Insurance   
- dppi() - Dynamic Proportion Portfolio Insurance   
- obpi() - Option Based Portfolio Insurance         
- shpi() - Step Hedge Portfolio Insurance            
- slpi() - Stop Loss Portfolio insurance             

Output S4 objects of type "CPPI", "DPPI", "OBPI", "SHPI" and "SLPI", with methods "show", "summary" and "plot".

### Use cases for the CTRM package
- Forward curves for market representation and non-standard pricing
- Trading strategies for price risk management (back testing, monte carlo experiments, trading)

### Install from GitHub
```
install.packages("devtools")  
library(devtools)
install_github("sleire/hedgeR")
```
