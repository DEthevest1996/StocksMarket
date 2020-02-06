#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Just buy and sell to play for the start 
# Max Franke & Damian Etchevest
# 02/05/20
#=======================================================================================================================================

# Install packages
'install.packages("alphavantager")
install.packages("jsonlite")
install.packages("TTR")
install.packages("ncar")
install.packages("xts")
install.packages("dplyr")
install.packages("quantmod")
install.packages("tidyverse")
install.packages("quantmod)'

# Load packages
#library(alphavantager)
library(jsonlite)
#library(forecast)
#library(TTR)
#library(ncar)
#library(dplyr)
#library(xts)
#library(quantmod)
#library(tidyverse)
library(RMySQL)
library(quantmod)

# CONSTANT VARIABLES (CALLS are for gaining informations, Transactions are for buying and selling)
#=======================================================================================================================================
#TOTAL_TIME_SEC <- 21600
#TOTAL_CALLS_TRANSACTION <- 300
#CALLS <- 50
TRANSACTION <- 250

# DATABASE
#=======================================================================================================================================
# Connect to database
mydb <- dbConnect(MySQL(),
                  username = 'exolar',
                  password = '4157922486',
                  dbname = "Stock_Market",
                  host = "stockproject.cyakxe88a8zf.us-east-1.rds.amazonaws.com"
)

# Write the output into table transaction
#---------------------------------------------------------------------------------------------------------------------------------------

# Define colnames for Transactions
transactions_colnames <- c("status","description","symbol","price","quantity","budget","Datetime","Transaction","TNo")

# Create empty buy_it dataframe
buy_it <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(buy_it) <- transactions_colnames

# Create empty sell_it dataframe
sell_it <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(sell_it) <- transactions_colnames

# Define the buy query
buy_query_function <- function(buy_it){
  buy_query <- paste("INSERT INTO DailyLogBook (status,description,symbol,price,quantity,budget,Datetime,Transaction,TNo) 
                   VALUES (", 
                     buy_it$status, ", ",
                     paste("'",buy_it$description,"'", rep = ""), ", ",
                     paste("'",buy_it$symbol,"'", rep = ""), ", ",
                     buy_it$price, ", ", 
                     buy_it$quantity, ", ", 
                     buy_it$budget, ", ", 
                     paste("'",buy_it$Datetime,"'", rep = ""), ", ", 
                     paste("'",buy_it$Transaction,"'", rep = ""), ", ", 
                     buy_it$TNo, 
                     ")", rep = "")
  return(buy_query)
}

# Define the sell query
sell_query_function <- function(sell_it){
  sell_query <- paste("INSERT INTO DailyLogBook (status,description,symbol,price,quantity,budget,Datetime,Transaction,TNo) 
                   VALUES (", 
                      sell_it$status, ", ",
                      paste("'",sell_it$description,"'", rep = ""), ", ",
                      paste("'",sell_it$symbol,"'", rep = ""), ", ",
                      sell_it$price, ", ", 
                      sell_it$quantity, ", ", 
                      sell_it$budget, ", ", 
                      paste("'",sell_it$Datetime,"'", rep = ""), ", ", 
                      paste("'",sell_it$Transaction,"'", rep = ""), ", ", 
                      sell_it$TNo,
                      ")", rep = "")
  return(sell_query)
}

#=======================================================================================================================================
# PREPERATION
#=======================================================================================================================================
# Inconstant variables
transaction <- 0
#calls <- 0

# Keys and Symbols
#---------------------------------------------------------------------------------------------------------------------------------------

Symbols_and_Keys <- data.frame(Symbols = c("AAPL", "SINA", "MSFT"),
                                           #"USDJPY","GBPUSD","AUDUSD","NZDUSD","EURUSD","USDJPY","GBPUSD","AUDUSD","NZDUSD"),
                               Keys = c("PS4R6O34SKO7QMI7","OZ1OB4JLZO38F64B","LJN45VDK4LQUODHI"))
                                        #"MAZM65O84SDP2ZSI",
                                        #"626T9PJYPCBZXJH4","LMRU8T7Q1DVRE7ZV",
                                        #"ECYLECGWH064HPP8","73IWZI0WQ5ROP6Y6",
                                        #"GCTSR9ASVV0D8W7S","6NF4N7TVGI7UFVYC"))


#=======================================================================================================================================
# GLOBAL FUNCTIONS
#=======================================================================================================================================

# Get the price
#---------------------------------------------------------------------------------------------------------------------------------------

Price <- function(symbol,key){
  df_price <- read.csv.zoo(paste("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",
                                 symbol,
                                 "&interval=1min&outputsize=full&apikey=",
                                 key,"&datatype=csv", sep = ""))
  return(df_price)
}

# Slope Function, calculate the degree of slope [FOR THE FIRST COLUMN OF THE INPUT DATAFRAME]
#---------------------------------------------------------------------------------------------------------------------------------------
slope <- function(x){
  # Load data
  x <- as.data.frame(x)
  # Define slope
  slope <- (tail(x[,1], 1) - head(x[,1], 1))/(1-0)
  # Degree of the slope
  angle <- atan(slope) * 180 / pi
  # Return degree of the slope
  return(angle)
}

# Indicators
#---------------------------------------------------------------------------------------------------------------------------------------
indicators <- function(df_price){
  
  # Slope for EMA
  slope_EMA_150 <- slope(as.data.frame(tail(EMA(Cl(df_price),150),150)))
  slope_EMA_100 <- slope(as.data.frame(tail(EMA(Cl(df_price),100),100)))
  slope_EMA_50 <- slope(as.data.frame(tail(EMA(Cl(df_price),50),50)))
  # EMA
  EMA_150 <- tail(EMA(Cl(df_price),150),1) 
  EMA_100 <-tail(EMA(Cl(df_price),100),1) 
  EMA_50 <- tail(EMA(Cl(df_price),50),1) 
  # BB & RSI
  bb <- BBands(Cl(df_price),s.d=2) %>% tail(n=1)
  rsi <- RSI(Cl(df_price),n=8) %>% tail(n=1)
  # MACD
  macd <- MACD(Cl(df_price), nFast=12, nSlow=26,
               nSig=9, maType=SMA) %>% tail(n=1)
  # Save to an Object
  Objects <- list("slope_150" = slope_EMA_150,
                  "slope_100" = slope_EMA_100,
                  "slope_50" = slope_EMA_50,
                  "EMA_150" = EMA_150, 
                  "EMA_100" = EMA_100,
                  "EMA_50" = EMA_50, 
                  "bb" = bb,
                  "rsi" = rsi, 
                  "macd" = macd)
  # Save to the global environment
  assign("Indicators",as.data.frame(Objects),envir = .GlobalEnv)
  return(Objects)
}

# Buy call
#---------------------------------------------------------------------------------------------------------------------------------------

buy <- function(symbol){
  # url <- paste("https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=buy&quantity=1&symbol=",
  #  symbol,
  #  sep = "")
  url <- paste("BUY ", symbol)
  return(url)
}

#=======================================================================================================================================
# Define the strategies
#=======================================================================================================================================

# EMA strategy
#---------------------------------------------------------------------------------------------------------------------------------------

EMA_Strategy <- function(df_price){
  # Define the current price
  current_price <- tail(Cl(df_price),1)
  # Check the indicators
  # indicators(df_price)
  
  # Attach the dataframe for Price
  attach(Indicators)
  # ROC current Price
  roc_currentPrice_ema100 <- ((current_price - EMA_100) / EMA_100) * 100 
  # If clause
  if (between(slope_150,30,60) == TRUE & 
      between(slope_100,30,60) == TRUE & 
      between(slope_50,30,60) == TRUE & 
      between(roc_currentPrice_ema100,0,10) == TRUE){ 
    result <- TRUE
  } else {
    result <- FALSE
  }
  return(result)
}

#=======================================================================================================================================
# While loop for transactions (buy and sell)
#=======================================================================================================================================

while (transaction <= TRANSACTION){
  #=======================================================================================================================================
  # Buy
  #=======================================================================================================================================
  
  # Define empty result dataframe
  result <- data.frame(matrix(ncol = 3, nrow = 0))
  # Colnames
  colnames(result) <- c("PositionID", "Symbol", "Strategy")
  
  for (i in 1:nrow(Symbols_and_Keys)){
    # Global Variable
    Symbol <- Symbols_and_Keys[i,1]
    Key <- Symbols_and_Keys[i,2]
    df_price <- Price(Symbol,Key)
    # Check for indicators
    indicators(df_price)
    # Attach Indicators
    attach(Indicators)
    # Check for EMA Strategy
    if (EMA_Strategy(df_price) == TRUE) {
      # Add to dataframe result
      result <- rbind(result, data.frame(PositionID = sample(1:100000,1), Symbol = Symbol, Strategy = "EMA-Strategy"))
    # Buy it
    url_buy <- (buy(Symbol))
    buy_it <- fromJSON(url_buy)
    # Add the DateTime
    buy_it$Datetime <- Sys.time()
    # Add the type of transaction
    buy_it$Transaction <- "Buy"
    # Count transaction
    transaction <- transaction + 1
    # Add transaction number to dataframe
    buy_it$TNo <- transaction
    # Define the sell query
    buy_query <- buy_query_function(buy_it)
    # Send the query to the transaction table
    dbSendQuery(mydb, buy_query)
    }
  }
  
  
  
  
  }
  
  




  # Sleep
  Sys.sleep(2)
  
  # Sell
  # ------------------------------------------------------------------------------------------------------------------------------------
  #url_sell <- "https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=sell&quantity=1&symbol=MCD"
  #sell_it <- fromJSON(url_sell)
  # Add the DateTime
  sell_it$Datetime <- Sys.time()
  # Add the type of transaction
  sell_it$Transaction <- "Sell"
  # Count transaction
  transaction <- transaction + 1
  # Add transaction number to dataframe
  sell_it$TNo <- transaction
  # Define the sell query
  sell_query <- sell_query_function(sell_it)
  # Send the query to the transaction table
  dbSendQuery(mydb, sell_query)
  # Sleep
  Sys.sleep(60)
}

# Reset the transactions to 0 for the next day
while(format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M") < "21:10"){
  
} 
transaction <- 0

