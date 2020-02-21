#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# For EC2
# Max Franke & Damian Etchevest
# 02/12/20
#=======================================================================================================================================

# Install packages
#install.packages("alphavantager")
#install.packages("jsonlite")
#install.packages("TTR")
#install.packages("ncar")
#install.packages("xts")
#install.packages("dplyr")
#install.packages("quantmod")
#install.packages("tidyverse")
#install.packages("quantmod)
#install.packages("magrittr)

# Load packages
#library(alphavantager)
library(jsonlite)
library(forecast)
#library(TTR)
#library(ncar)
#library(dplyr)
#library(xts)
#library(quantmod)
#library(tidyverse)
library(RMySQL)
library(quantmod)
library(magrittr)

# CONSTANT VARIABLES
#=======================================================================================================================================

ALL_SYMBOLS <- c("AEY", "NIO", "ZNGA", "BBD", "ITUB", "SBI", "TCS", "VEDL", "TCS", "CSCO", "YESBANK.NS", "ZEEL.NS", "TSLA", "INO", "GGB")

# DATABASE
#=======================================================================================================================================
# Connect to database
mydb <- dbConnect(MySQL(),
                  username = 'exolar',
                  password = '4157922486',
                  dbname = "Stock_Market",
                  host = "stockproject.cyakxe88a8zf.us-east-1.rds.amazonaws.com"
)

#=======================================================================================================================================
# PREPERATION
#=======================================================================================================================================

# Define colnames for Buy
colnames <- c("Position_ID", "Symbol", "Price", "Quantity", "Datetime","Strategy","EMA150","EMA100","EMA50",
              "Slope150","Slope100","Slope50","RSI","bb_dn","bb_mavg","bb_up","bb_pctB","macd","Transaction", "image")

# Keys and Symbols
#---------------------------------------------------------------------------------------------------------------------------------------

Symbols_and_Keys <- data.frame(Symbols = c("YESBANK.NS", "ZEEL.NS", "TSLA", "INO", "GGB"),
                               Keys = c("93A7HD9MNUW5E6JL", "59F3TSOPON7TS9NR", "BMEURJNG8NUEPS4F", "BK884WY2PRPFI9I3", "ITDOAF8KTODZTXD1"))

# Save the translation vector from m to degree
#---------------------------------------------------------------------------------------------------------------------------------------
degree <- data.frame(degree = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.57, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 5.74, 6:90))
m <- data.frame(m = c(573.0,286.5,191.0,143.2,114.6,100,95.49,81.85,71.62,63.66,57.29,28.64,19.08,14.30,11.43,10,9.514,8.144,7.115,6.314,5.671,5.145,4.705,4.331,4.011,3.732,3.487,3.271,3.078,2.904,2.747,2.605,2.475,2.356,2.246,2.145,2.050,1.963,1.881,1.804,1.732,1.664,1.600,1.540,1.483,1.428,1.376,1.327,1.280,1.235,1.192,1.150,1.111,1.072,1.036,1.000,0.9657,0.9325,0.9004,0.8693,0.8391,0.8098,0.7813,0.7536,0.7265,0.7002,0.6745,0.6494,0.6249,0.6009,0.5774,0.5543,0.5317,0.5095,0.4877,0.4663,0.4452,0.4245,0.4040,0.3839,0.3640,0.3443,0.3249,0.3057,0.2867,0.2679,0.2493,0.2309,0.2126,0.1944,0.1763,0.1584,0.1405,0.1228,0.1051,0.08749,0.06993,0.05241,0.03492,0.01746,0.00000))
degree_m <- cbind(degree, m)
degree_m <- rbind(degree_m, data.frame(degree = degree * -1, m = m*-1))

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
  # Colname
  colnames(x) <- "Price"
  # Count every Minute
  x$Min <- c(1:nrow(x))
  # Normalize the distance in order to calculate the slope
  x$Min_nom <- (x$Min - min(x$Min)) / (max(x$Min) - min(x$Min))
  # slope from last point to the first point
  slope <- (tail(x[,1], 1) - head(x[which(!is.na(x$Price)),1], 1))/(tail(x$Min_nom, 1) - head(x$Min_nom, 1))
  # Position in translation dataframe
  position <- which.min(abs(degree_m$m - slope))
  # Degree
  slope_to_degree <- degree_m[position, 1] 
  # Return degree
  return(slope_to_degree)
}

# Indicators
#---------------------------------------------------------------------------------------------------------------------------------------
indicators <- function(df_price){
  
  # Slope for EMA
  slope_EMA_150 <- slope(as.data.frame(tail(EMA(Cl(df_price),150),150)))
  slope_EMA_100 <- slope(as.data.frame(tail(EMA(Cl(df_price),100),100)))
  slope_EMA_50 <- slope(as.data.frame(tail(EMA(Cl(df_price),50),50)))
  # EMA
  EMA_150 <- tail(EMA(Cl(df_price),150),1)[[1]] 
  EMA_100 <-tail(EMA(Cl(df_price),100),1)[[1]] 
  EMA_50 <- tail(EMA(Cl(df_price),50),1)[[1]] 
  # BB & RSI
  bb <- BBands(Cl(df_price),s.d=2) %>% tail(n=1)
  rsi <- RSI(Cl(df_price),n=8) %>% tail(n=1)
  # MACD
  macd <- MACD(Cl(df_price), nFast=12, nSlow=26,
               nSig=9, maType=SMA) %>% tail(n=1)
  # Stochastic
  stochastic <- mean(tail(stoch(df_price[,c("high","low","close")]),1)[,2:3]) * 100
  # Save to an Object
  Objects <- list("slope_150" = slope_EMA_150,
                  "slope_100" = slope_EMA_100,
                  "slope_50" = slope_EMA_50,
                  "EMA_150" = EMA_150, 
                  "EMA_100" = EMA_100,
                  "EMA_50" = EMA_50, 
                  "bb" = bb,
                  "rsi" = rsi, 
                  "macd" = macd,
                  "stochastic" = stochastic)
  # Save to the global environment
  assign("Indicators",as.data.frame(Objects),envir = .GlobalEnv)
  return(Objects)
}

# Time Series
#---------------------------------------------------------------------------------------------------------------------------------------

ts <- function(df_price){
  fit <- auto.arima(Cl(df_price))
  fit.forecast <- forecast(fit,h=1)
  fitted <- as.data.frame(fit.forecast)
  return(fitted)
}

#=======================================================================================================================================
# Define the strategies
#=======================================================================================================================================

# EMA strategy 
#---------------------------------------------------------------------------------------------------------------------------------------
EMA_Strategy <- function(df_price){
  # Define the current price
  current_price <- tail(Cl(df_price),1)[[1]]
  # Default for long and short
  long <- FALSE
  short <- FALSE 
  # Define open long and open short
  open_long <- FALSE
  open_short <- FALSE
  close_short <- FALSE
  close_long <- FALSE
  # If clause preparation
  if((tail(Cl(df_price[-nrow(df_price),]),1)[[1]] < Indicators$EMA_50) & 
     (tail(Cl(df_price[-nrow(df_price),]),1)[[1]] > Indicators$EMA_100)){
    long <- TRUE
  } else if ((tail(Cl(df_price[-nrow(df_price),]),1)[[1]] > Indicators$EMA_50) & 
             (tail(Cl(df_price[-nrow(df_price),]),1)[[1]] < Indicators$EMA_100)){
    short <- TRUE
  }
  # If clause open long and open short
  if ((Indicators$slope_150 >= 30) == TRUE & (Indicators$slope_150 <= 90) == TRUE |
      (Indicators$slope_100 >= 30) == TRUE & (Indicators$slope_100 <= 90) == TRUE | 
      (Indicators$slope_50 >= 30) == TRUE & (Indicators$slope_50 <= 90) == TRUE |
      long == TRUE |
      current_price > Indicators$EMA_50){
    
    open_long == TRUE
  } else if (Indicators$slope_150 < 0 &
             Indicators$slope_100 < 0 &
             Indicators$slope_50 < 0 &
             short == TRUE |
             current_price < Indicators$EMA_50) {
    open_short == TRUE
  }
  #If clause close short
  if (Indicators$rsi <= 30 |
      Indicators$stochastic <= 20 |
      (tail(Cl(df_price),1) >= (Indicators$EMA_100 - Indicators$EMA_100*0.001))) {
    close_short <- TRUE
  }
  #close long
  if (Indicators$rsi <= 70 |
      Indicators$stochastic <= 80 |
      (tail(Cl(df_price),1) <= (Indicators$EMA_100 - Indicators$EMA_100*0.001))) {
    close_long <- TRUE
  }
  
  Objects <- list(open_long, open_short, close_long, close_short)
  return(Objects)
}

# Time Series strategy for buy
#---------------------------------------------------------------------------------------------------------------------------------------

time_series_buy <- function(df_price){
  # Create prediction for next minute
  ts_object <- ts(df_price)
  # Define the current price
  current_price <- tail(Cl(df_price),1)
  # If clause
  if (current_price <= round(ts_object$`Lo 80`, digits = 2)) {
    decision <- TRUE
  } else{
    decision <- FALSE
  }
  return(decision)
}

# Time Series strategy for sell
#---------------------------------------------------------------------------------------------------------------------------------------

time_series_sell <- function(df_price){
  # Create prediction for next minute
  ts_object <- ts(df_price)
  # Define the current price
  current_price <- tail(Cl(df_price),1)
  # If clause
  if (current_price >= round(ts_object$`Hi 80`, digits = 2)) {
    decision <- TRUE
  } else{
    decision <- FALSE
  }
  return(decision)
}

# Stochastic strategy
#---------------------------------------------------------------------------------------------------------------------------------------

Stochastic_Strategy <- function(df_price,Symbol){
  # Define the current price
  current_price <- tail(Cl(df_price),1)[[1]]
  # Do we have signals?
  #---------------------------------------------------------------------------------------------------------------------------------------
  # Define long query
  long_query <- dbSendQuery(mydb, paste0("SELECT * FROM signals WHERE Symbol = ", paste("'",Symbol,"'",rep = "")," AND 
                                         LongShort = 'long' AND
                                         Datetime >= '",Sys.time()-600,"'"))
  # Retrieve all results from the server
  long <- fetch(long_query, n = -1)
  
  # Define short query
  short_query <- dbSendQuery(mydb, paste0("SELECT * FROM signals WHERE Symbol = ", paste("'",Symbol,"'",rep = "")," AND 
                                         LongShort = 'short' AND
                                         Datetime >= '",Sys.time()-600,"'"))
  # Retrieve all results from the server
  short <- fetch(short_query, n = -1)
  # Default open long and open short
  open_long <- FALSE
  open_short <- FALSE
  close_long <- FALSE
  close_short <- FALSE
  # If clause long open
  if (nrow(long) >= 1 & 
      (tail(Cl(df_price[-nrow(df_price),]),1)[[1]] >= Indicators$bb.mavg) & 
      (indicators(df_price[-nrow(df_price),])['stochastic'] <= Indicators$stochastic)) {
    open_long = TRUE
  }
  # If clause short open
  if(nrow(short) >= 1 &
     (tail(Cl(df_price[-nrow(df_price),]),1)[[1]] < Indicators$bb.mavg) &
     (indicators(df_price[-nrow(df_price),])['stochastic'] >= Indicators$stochastic)){
    open_short = TRUE
  }
  # If clause close_long
  if ((tail(Cl(df_price),1)[[1]]) >= Indicators$bb.up |
      (tail(Cl(df_price),1)[[1]] <= (Indicators$bb.mavg - Indicators$bb.mavg*0.001))){
    close_long <- TRUE
  }
  # If clause close_short
  if ((tail(Cl(df_price),1)[[1]]) <= Indicators$bb.up |
      (tail(Cl(df_price),1)[[1]] >= (Indicators$bb.mavg - Indicators$bb.mavg*0.001))){
    close_short <- TRUE
  }
  Objects <- list(open_long,open_short, close_long, close_short)
  return(Objects)
}

#=======================================================================================================================================
# While loop for transactions (buy and sell) 
#=======================================================================================================================================

# First Loops from 10 am to 1:45 pm and transactions are smaller or equal 250 Transactions
#---------------------------------------------------------------------------------------------------------------------------------------
while (format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M") >= "15:00" & 
       format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M") < "19:00") {
  # BUY
  #=======================================================================================================================================
  
  # FOR EVERY SYMBOLS IN KEYS CHECK THE STRATEGIES
  #---------------------------------------------------------------------------------------------------------------------------------------
  for (i in 1:nrow(Symbols_and_Keys)){
    # Define Price for each symbol and key
    #---------------------------------------------------------------------------------------------------------------------------------------
    Symbol <- Symbols_and_Keys[i,1]
    Key <- Symbols_and_Keys[i,2]
    df_price <- Price(Symbol,Key)
    # Check for indicators
    #---------------------------------------------------------------------------------------------------------------------------------------
    indicators(df_price)
    
    # Stochastic strategy SIGNAL GENERATOR
    #---------------------------------------------------------------------------------------------------------------------------------------
    if ((Indicators$rsi <= 30) & 
        (Indicators$stochastic <= 15)){
      # Query
      stochastic_query_long <- paste("INSERT INTO signals (Symbol, LongShort, Datetime)
                                VALUES (",
                                     paste("'",Symbol,"'", rep = ""), ", ",
                                     "'long'", ", ",
                                     paste("'",Sys.time(),"'", rep = ""),
                                     ")", rep = "")
      # Send to database
      dbSendQuery(mydb, stochastic_query_long)
    }
    if ((Indicators$rsi >= 70) & 
        (Indicators$stochastic >= 85)){
      # Query
      stochastic_query_short <- paste("INSERT INTO signals (Symbol, LongShort, Datetime)
                                VALUES (",
                                      paste("'",Symbol,"'", rep = ""), ", ",
                                      "'short'", ", ",
                                      paste("'",Sys.time(),"'", rep = ""),
                                      ")", rep = "")
      # Send to database
      dbSendQuery(mydb, stochastic_query_short)
    }
    
    # Check for Strategies
    #---------------------------------------------------------------------------------------------------------------------------------------
    if (EMA_Strategy(df_price)[[1]] == TRUE |
        EMA_Strategy(df_price)[[2]] == TRUE |
        time_series_buy(df_price) == TRUE |
        Stochastic_Strategy(df_price, Symbol)[[1]] == TRUE |
        Stochastic_Strategy(df_price, Symbol)[[2]] == TRUE) {
      # Define which strategy was TRUE
      strategy <- data.frame(Strategy = c("EMA Strategy_long", "EMA Strategy_short", 
                                          "Time Series", "Stochastic_long", "Stochastic_short"), Result = c(EMA_Strategy(df_price)[[1]],
                                                                                                            EMA_Strategy(df_price)[[2]],
                                                                                                            time_series_buy(df_price),
                                                                                                            Stochastic_Strategy(df_price, Symbol)[[1]],
                                                                                                            Stochastic_Strategy(df_price, Symbol)[[2]]))
      # Position of decision for strategy
      position <- which(strategy$Result == TRUE)
      
      # Add true strategies to dataframe result
      for (i in position) {
        # Query to Database
        # There are more instances which are checking for different symbols and if the conditions are TRUE then the results are stored
        result_query <- paste("INSERT INTO result (PositionID, Symbol, Datetime, Strategy, Transaction, Price, EMA150,EMA100,EMA50,
                       Slope150,Slope100,Slope50,RSI,bb_dn,bb_mavg,bb_up,bb_pctB,macd, stochastic)
                                VALUES (",
                              sample(1:100000,1),", ",
                              paste0("'",Symbol,"'", rep = ""), ", ",
                              paste0("'",Sys.time(),"'", rep = ""), ", ",
                              paste0("'",strategy[i,1],"'", rep = ""), ", ",
                              "'Buy'", ", ",
                              tail(Cl(df_price),1), ", ",
                              Indicators$EMA_150, ", ", Indicators$EMA_100, ", ", Indicators$EMA_50, ", ", 
                              Indicators$slope_150, ", ", Indicators$slope_100, ", ", Indicators$slope_50, ", ", 
                              Indicators$rsi, ", ", Indicators$bb.dn, ", ", Indicators$bb.mavg, ", ", 
                              Indicators$bb.up, ", ", Indicators$bb.pctB, ", ", Indicators$macd.macd,", ", Indicators$stochastic,
                              ")", rep = "")
        # Send to database
        dbSendQuery(mydb, result_query)
      }
    }
  }
  Sys.sleep(60)
}
