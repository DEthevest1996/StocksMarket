#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Calculating the slope
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
library(alphavantager)
library(jsonlite)
library(forecast)
library(TTR)
library(ncar)
library(dplyr)
library(xts)
library(quantmod)
library(tidyverse)

# Define av_api_key
av_api_key("PS4R6O34SKO7QMI7")

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

#=======================================================================================================================================

# Define the slope of MSFT for close
# -------------------------------------------------------------------------------------------------------------------------------------

# Load Dataframe
price <- as.data.frame(tail(Price("MSFT"),200))

# Count every Minute
price$Min <- c(1:nrow(price))

# Normalize the distance in order to calculate the slope
price$Min_nom <- scale(price$Min)

# Plot with ggplot
plot1 <- ggplot(data = price, aes(Min_nom, close)) + geom_line() + geom_smooth(method = "lm", se = FALSE, formula = y ~ x)
print(plot1)

# Define y
y <- tail(price$close, 1) - head(price$close, 1)

# Define x
x <- tail(price$Min_nom, 1) - head(price$Min_nom, 1)

# Angle of the slope
angle <- atan2(y,x)
print(angle)

slope <- function(x){
  # Load data
  x <- as.data.frame(x)
  # Define slope
  slope <- (tail(x$close, 1) - head(x$close, 1))/(1-0)
  # Degree of the slope
  angle <- atan(slope) * 180 / pi
  # Return degree of the slope
  return(angle)
}

slope(df_Price)
