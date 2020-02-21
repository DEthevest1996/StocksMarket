#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Run sript with time
# Max Franke & Damian Etchevest
# 01/22/20
#=======================================================================================================================================

# Install packages
install.packages("alphavantager")
install.packages("jsonlite")
# Load packages
library(alphavantager)
library(jsonlite)

# Call our profile
url_profile <- "https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=profile"
profile <- fromJSON(url_profile)
print(profile)
# Define the symbols we want to use for our predictions
symbols <- c("AAPL", "EURUSD")

# Define the buy function
buy <- function(symbols){
  quantity <- (0.02*profile$budget)
  buy <- "buy"
  url <- paste("https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=",buy,"&quantity=",quantity,"&symbol=",symbols, sep = "")
  buy_it <- fromJSON(url)
}


# Define the sell function
sell <- function(symbols){
  quantity <- (0.02*profile$budget)
  sell <- "sell"
  url <- paste("https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=",sell,"&quantity=",quantity,"&symbol=",symbols, sep = "")
  sell_it <- fromJSON(url)
}


# for (i in symbols) {
  # call the key
  # time series
  # call sma
  # call rsi 
  #-------------------------------------------------
  # Buy
  #-------------------------------------------------
  #                Conditions
  # Is the actual value <= 'Low 80'?
  # RSI not overvalued (RSI < 70)
  # SMA trend is positive in the last hour and/or negative trend last 10min
  #-------------------------------------------------
  # Sell
  #-------------------------------------------------
  #               Conditions
  # Do we have shares?
  # Is the actual value >= 'High 80'?
  # RSI not undervalued (RSI > 30)
  # SMA trend is negative in the last hour and/or postive trend last 10min





