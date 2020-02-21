#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Stocks Iterations
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

