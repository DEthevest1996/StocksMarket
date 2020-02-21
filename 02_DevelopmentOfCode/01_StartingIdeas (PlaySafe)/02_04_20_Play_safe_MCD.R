#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Just buy and sell to play for the start 
# Max Franke & Damian Etchevest
# 02/02/20
#=======================================================================================================================================

# Install packages
'install.packages("alphavantager")
install.packages("jsonlite")
install.packages("TTR")
install.packages("ncar")
install.packages("xts")
install.packages("dplyr")
install.packages("quantmod")
install.packages("tidyverse")'

# Load packages
#library(alphavantager)
library(jsonlite)
library(forecast)
library(TTR)
library(ncar)
library(dplyr)
library(xts)
library(quantmod)
library(tidyverse)
library(RMySQL)

# CONSTANT VARIABLES (CALLS are for gaining informations, Transactions are for buying and selling)
#=======================================================================================================================================
TOTAL_TIME_SEC <- 21600
TOTAL_CALLS_TRANSACTION <- 300
CALLS <- 50
TRANSACTION <- 250

# GLOBAL FUNCTIONS
#=======================================================================================================================================
# Time Series function
# -------------------------------------------------------------------------------------------------------------------------------------
ts <- function(dataframe){
  fit <- auto.arima(dataframe$close)
  fit.forecast <- forecast(fit,h=1)
  fitted <- as.data.frame(fit.forecast)
  return(fitted)
}

# Price function
# -------------------------------------------------------------------------------------------------------------------------------------
Price <- function(symbol){
  df_price <-   as.data.frame(av_get(symbol = symbol,
                                     av_fun = "TIME_SERIES_INTRADAY",
                                     interval = "1min",
                                     outputsize= "full"))
  df_price <- xts(df_price[,-1], order.by=df_price[,1])
  return(df_price)
}

# Slope function
# -------------------------------------------------------------------------------------------------------------------------------------
slope <- function(x){
  slope <- (tail(x,1) - head(x,1)) / (nrow(x)-1)
  return(slope)
}

# Ema function
# -------------------------------------------------------------------------------------------------------------------------------------
Ema.s <- function(df_price){ 
  EMA_150 <- as.data.frame(EMA(Cl(df_price),n=150)) %>% tail(n=30)
  EMA_100 <- as.data.frame(EMA(Cl(df_price),n=100) %>% tail(n=30)) 
  EMA_50 <- as.data.frame(EMA(Cl(df_price),n=50) %>% tail(n=30))
  Objects <- c(EMA_150)#,EMA_100,EMA_50)
  return(Objects)
}

# z-Score Normalization function
#---------------------------------------------------------------------------------------------------------------------------------------

zScore <- function(x){
  return(scale(x))
}

# DATABASE
#=======================================================================================================================================
# Connect to database
mydb <- dbConnect(MySQL(),
                  username = 'exolar',
                  password = '4157922486',
                  dbname = "Stock_Market",
                  host = "stockproject.cyakxe88a8zf.us-east-1.rds.amazonaws.com"
)

# Write the output into table buy
#---------------------------------------------------------------------------------------------------------------------------------------

test <- fromJSON("~/Desktop/05_Big Data Analytics/04_Classes/03 SP:Term1/CIS-544 DATA MINING & MACHINE LRNG/06_Final_Project/Alpha_Vantage/Json_example.json")
test$Datetime <- Sys.time()

# Define colnames for buy table
buy_table_colnames <- c("Position_ID, Symbol, Price, Quantity, Datetime, Strategy, EMA150, EMA100, EMA50, 
Slope150, Slope100, Slope50, RSI, bb_dn, bb_mavg, bb_up, bb_pctB, macd, image")

# Define the buy query
buy_query <- paste("INSERT INTO buy 
(",buy_table_colnames,")", 
"VALUES
(21, 'MSFT', 250, 1, '2020-02-04 14:20:21', 2, 150, 250,300, 64, 62, 66, 100, 100, 100, 100, 100, 100, 50)", rep="")


# Send the query to the buy table
dbSendQuery(mydb, buy_query)


# Preparation
#=======================================================================================================================================
# Inconstant variables
transaction <- 0
calls <- 0

# Create dataframe to store the results in case of buy
#---------------------------------------------------------------------------------------------------------------------------------------
buy_transaction <- data.frame(matrix(ncol = 19, nrow = 0))

# Define colnames for buy table
buy_table_colnames <- c("Position_ID, Symbol, Price, Quantity, Datetime, Strategy, EMA150, EMA100, EMA50, 
Slope150, Slope100, Slope50, RSI, bb_dn, bb_mavg, bb_up, bb_pctB, macd, image")

# Insert to dataframe
colnames(all_transactions) <- col_names


#=======================================================================================================================================
# While loop for transactions (buy and sell)
#=======================================================================================================================================

while (transaction <= TRANSACTION){
  
  # Buy
  # ------------------------------------------------------------------------------------------------------------------------------------
  url_buy <- "https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=buy&quantity=1&symbol=MCD"
  buy_it <- fromJSON(url_buy)
  # Add the DateTime
  buy_it$Datetime <- Sys.time()
  # Add the type of transactoin
  buy_it$Transaction <- "Buy"
  # Count transaction
  transaction <- transaction + 1
  # Add transaction number to dataframe
  buy_it$Number <- transaction
  # Safe to daily log book
  
  
  
  # Sleep
  Sys.sleep(2)
  
  # Sell
  # ------------------------------------------------------------------------------------------------------------------------------------
  url_sell <- "https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=sell&quantity=1&symbol=MCD"
  sell_it <- fromJSON(url_sell)
  # Add the Date & Time
  sell_it$Month <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m")
  sell_it$Day <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%d")
  sell_it$Time <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
  # Add the type of transactoin
  sell_it$Transaction <- "Sell"
  # Count transaction
  transaction <- transaction + 1
  # Add transaction number to dataframe
  sell_it$Number <- transaction
  # Safe to daily log book
  write.table(sell_it,
              file = paste("Transactions ", format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m-%d"), ".csv", sep = ""),
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE)
  # Sleep
  Sys.sleep(60)
}


# Reset the transactions to 0 for the next day
while(format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M") < "21:10"){
  
} 
transaction <- 0

