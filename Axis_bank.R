library(tseries)

## -- Time serie sanalysis on Axis bank stock price data --- ##
AxisBank= read.csv("Axis Bank.csv")

# > head(AxisBank)      
# Date  Open   High    Low  Close Volume Adj.Close
# 1  12/5/2016 458.5 464.55 441.50 456.30 523200    456.30
# 2 11/28/2016 464.1 477.00 458.30 459.80 651600    459.80
# 3 11/21/2016 479.2 482.15 458.60 471.35 499800    471.35
# 4 11/14/2016 498.4 503.45 470.10 473.45 434700    473.45

AxisBank$Date_stock <- as.Date(AxisBank$Date, format = "%m/%d/%Y") 
AxisBank <- AxisBank[order(AxisBank$Date_stock),]		

## Axis Bank had last split the face value of its shares 
## from Rs 10 to Rs 2 in 2014.The share has been quoting on an ex-split basis from July 28, 2014.

# we need to multiple by 5* share price after the split
Date_split <- as.Date('2014-07-14')
AxisBank$split <- 1
AxisBank$split[which(AxisBank$Date_stock > Date_split)] <- 5  
AxisBank$Open <- AxisBank$Open*AxisBank$split

myts <- ts(AxisBank$Open, frequency=52, start=c(1998,46))

## ------------------ stationary ----------
Box.test(myts, lag=20, type="Ljung-Box")

## adf test for stationary
adf.test(myts, alternative = "stationary")
adf.test(diff(myts), alternative = "stationary")

## kpss  test for stationary
kpss.test(myts)
kpss.test(diff(myts))
