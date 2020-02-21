# Install packages
#install.packages("alphavantager")
#install.packages("jsonlite")
# Load packages

library(jsonlite)
library(forecast) 
library(quantmod)
library(magrittr) # for %>%
library(RMySQL)
library(xts)


# DATABASE
# Connect to database
mydb <- dbConnect(MySQL(),
                  username = 'exolar',
                  password = '4157922486',
                  dbname = "Stock_Market",
                  host = "stockproject.cyakxe88a8zf.us-east-1.rds.amazonaws.com"
)


# Main_key <- av_api_key("6EMR51HT1VKKE6ZA")
Symbols_and_Keys <- data.frame(	Symbols = c("KGC","WMT","BYND","HSBC","MSFT"),
                                Keys = c("ILLBCSQSF6QZG3V2","YQYWX550HET7ACKU","ODOF8RR9NB8WVJAX",
                                         "FK8GRIBTB4WXKZJX","ZVJ8BX088IL2PXIP")
)



                            
Price <- function(symbol,key){
   df_price <- read.csv.zoo(paste("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",
                              symbol,
                              "&interval=1min&outputsize=full&apikey=",
                              key,"&datatype=csv", sep = ""))
 return(df_price)
}

ts <- function(df_price){
        fit <- auto.arima(Cl(df_price))
        fit.forecast <- forecast(fit,h=1)
        fitted <- as.data.frame(fit.forecast)
        return(fitted)
}

indicators <- function(df_price){
  
  slope_EMA_150 <- slope(tail(EMA(Cl(df_price),150),50))
  slope_EMA_100 <- slope(tail(EMA(Cl(df_price),100),50))
  slope_EMA_50 <- slope(tail(EMA(Cl(df_price),50),50))
  
  EMA_150 <- round(tail(EMA(Cl(df_price),150),1),3)
  EMA_100 <-round(tail(EMA(Cl(df_price),100),1),3)
  EMA_50 <- round(tail(EMA(Cl(df_price),50),1),3)
  
  bb <- round(BBands(Cl(df_price),s.d=2) %>% tail(n=1),3)
  rsi <- round(RSI(Cl(df_price),n=8) %>% tail(n=1),3)
  
  macd <- round(MACD(Cl(df_price), nFast=12, nSlow=26,
               nSig=9, maType=SMA) %>% tail(n=1),3)
  stochastic <- round(mean(tail(stoch(df_price[,c("high","low","close")]),1)[,2:3]) * 100,3)
  
  
  Objects <- list("slope_150" = slope_EMA_150,
                  "slope_100" = slope_EMA_100,
                  "slope_50" = slope_EMA_50,
                  "EMA_150" = EMA_150, 
                        "EMA_100" =EMA_100,
                        "EMA_50" =EMA_50, 
                        "bb" =bb,
                        "rsi" =rsi, 
                        "macd" =macd,
                  "stochastic" = stochastic) 

  return(Objects)
}

degree <- data.frame(degree = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.57, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 5.74, 6:90))
m <- data.frame(m = c(573.0,286.5,191.0,143.2,114.6,100,95.49,81.85,71.62,63.66,57.29,28.64,19.08,14.30,11.43,10,9.514,8.144,7.115,6.314,5.671,5.145,4.705,4.331,4.011,3.732,3.487,3.271,3.078,2.904,2.747,2.605,2.475,2.356,2.246,2.145,2.050,1.963,1.881,1.804,1.732,1.664,1.600,1.540,1.483,1.428,1.376,1.327,1.280,1.235,1.192,1.150,1.111,1.072,1.036,1.000,0.9657,0.9325,0.9004,0.8693,0.8391,0.8098,0.7813,0.7536,0.7265,0.7002,0.6745,0.6494,0.6249,0.6009,0.5774,0.5543,0.5317,0.5095,0.4877,0.4663,0.4452,0.4245,0.4040,0.3839,0.3640,0.3443,0.3249,0.3057,0.2867,0.2679,0.2493,0.2309,0.2126,0.1944,0.1763,0.1584,0.1405,0.1228,0.1051,0.08749,0.06993,0.05241,0.03492,0.01746,0.00000))
degree_m <- cbind(degree, m)
degree_m <- rbind(degree_m, data.frame(degree = degree * -1, m = m*-1))

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
  return(round(slope_to_degree))
}

EMA_Strategy <- function(df_price){

  
  price_bought <- dbSendQuery(mydb, paste("select Price from Entry where Symbol = '",
                                          Symbol,
                                          "' order by Datetime limit 1",sep = "")) %>% fetch(n = -1)
  
  # Default for long and short
  long <- FALSE
  short <- FALSE 
  # Define open long and open short
  open_long <- FALSE
  open_short <- FALSE
  close_short <- FALSE
  close_long <- FALSE
  # If clause preparation
  if(last_min_price <= EMA_50 & 
     last_min_price >= EMA_100){
    long <- TRUE
  } else if (last_min_price >= EMA_50 & 
             last_min_price <= EMA_100){
    short <- TRUE
  }
  # If clause
  if (slope_150 >= 20 & slope_100 >= 20  & slope_50 >= 20 &
      long == TRUE &
      current_price > EMA_50){
    open_long <-  TRUE
  } else if (slope_150 <= -20 & slope_100 <= -20  & slope_50 <= -20 &
      short == TRUE &
      current_price < EMA_50){
    open_short <-  TRUE
  }
  #If clause close short
  if (rsi <= 35 |
      stochastic <= 25 | 
      current_price <= (price_bought - price_bought* 0.003) |
      current_price > EMA_50) {
    close_short <- TRUE
  }
  #close long
  if (rsi >= 65 |
      stochastic >= 75 |
      current_price >= (price_bought + price_bought* 0.003) |
      current_price < EMA_50) {
    close_long <- TRUE
  }
  
  
  Objects <- list(open_long,open_short,close_long,close_short )
  return(Objects)
}

Stochastic_Strategy <- function(df_price,Symbol){

  # Define long query
  long_query <- dbSendQuery(mydb, paste("SELECT * FROM signals WHERE Symbol = '",Symbol,"' AND 
                                         LongShort = 'long' AND
                                         Datetime >= timestampadd(minute,-7,current_timestamp)",sep = ""))
  # Retrieve all results from the server
  long <- fetch(long_query, n = -1)
  
  # Define short query
  short_query <- dbSendQuery(mydb, paste("SELECT * FROM signals WHERE Symbol = '",Symbol,"' AND 
                                         LongShort = 'short' AND
                                         Datetime >= timestampadd(minute,-7,current_timestamp)",sep = ""))
  # Retrieve all results from the server
  short <- fetch(short_query, n = -1)
  
  open_long <- FALSE
  open_short <- FALSE
  
  close_long <- FALSE
  close_short <- FALSE

  if(nrow(long) >= 1){
    if(last_min_price >= bb$mavg & current_price > bb$mavg){
      open_long <- TRUE
    }} 
  if(nrow(short) >= 1){
      if(last_min_price <= bb$mavg & current_price < bb$mavg){
            open_short <-  TRUE
          }}
  # If clause close_long
  if ((current_price > (bb$up - bb$up *0.001)) |
      (current_price < bb$mavg)){
    close_long <- TRUE
  }
  # If clause close_short
  if ((current_price < (bb$dn + bb$dn*0.001)) | 
      (current_price > bb$mavg)){
    close_short <- TRUE
  }
  
  Objects <- list(open_long,open_short,close_long,close_short )
  return(Objects)
  
}

charting <- function(df_price){
  index <- which(.index(df_price) == paste(Sys.Date(), "09:31:00"))
  df_price <- df_price[index:nrow(df_price)]
  image_code <- sample(1:10000000, 1)
  png(filename = paste("StockCharts/",image_code,".png", sep = ""))
  par(mfrow = c( 8, 1 ) )
  lineChart(tail(df_price,200),line.type = 'h')
  dev.off()
  return(image_code)
}

while (format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M") >= "15:00" & 
       format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M") < "20:55"){
  
  for (i in 1:nrow(Symbols_and_Keys)){
    
    # Global Variable
    position_id <- 0
    Symbol <- Symbols_and_Keys[i,1]
    Key <- Symbols_and_Keys[i,2]
    df_price <- Price(Symbol,Key)
    
    # Define the current price
    current_price <- tail(Cl(df_price),1)[[1]]
    last_min_price <- tail(Cl(df_price[-nrow(df_price),]),1)[[1]]
    
    Volume <- floor(25 / (0.003 * current_price))
   
    
    attach(indicators(df_price))
    
    # Stochastic strategy SIGNAL GENERATOR
    if ((rsi <= 35) & 
        (stochastic <= 15)){
      # Query
      stochastic_query_long <- paste("INSERT INTO signals (Symbol, LongShort)
                                VALUES ( '",Symbol, "' , 'long')", sep = "")
      # Send to database
      dbSendQuery(mydb, stochastic_query_long)
    }
    if ((rsi >= 65) & 
        (stochastic >= 85)){
      # Query
      stochastic_query_short <- paste("INSERT INTO signals (Symbol, LongShort)
                                VALUES ( '",Symbol, "' , 'short')", sep = "")
      # Send to database
      dbSendQuery(mydb, stochastic_query_short)
    }
    
    Other_entries <- dbSendQuery(mydb, paste("select * 
                                             from Portfolio 
                                             where Symbol = '",Symbol,"' and Status = 'entry'",sep = "")) %>% fetch(n = -1)
    
    Other_closed_last_5_min <- dbSendQuery(mydb, paste("select * 
                                             from Portfolio 
                                             where Symbol = '",Symbol,"' and 
                                                      closed_at >= timestampadd(minute,-5,current_timestamp)",sep = "")) %>% fetch(n = -1)
    
    if(nrow(Other_entries) == 0 & nrow(Other_closed_last_5_min) == 0){
      # Inserting indicators as soon as any open strategies equals true (per symbol)
      if(EMA_Strategy(df_price)[[1]] == TRUE | EMA_Strategy(df_price)[[2]] == TRUE |
         Stochastic_Strategy(df_price,Symbol)[[1]] == TRUE | Stochastic_Strategy(df_price,Symbol)[[2]] == TRUE){
        dbSendQuery(mydb, paste("insert into Indicators (Symbol, slope_EMA_150,slope_EMA_100, slope_EMA_50,
                                EMA_150, EMA_100, EMA_50, bb_dn, bb_mavg, bb_up, bb_pctB, rsi,
                                macd_macd, macd_signal,stochastic) values ('",
                                Symbol,"' , ",
                                slope_150,",", slope_100,",", slope_50,",", EMA_150,",",
                                EMA_100,",", EMA_50,",", bb$dn,",", bb$mavg,",",  
                                bb$up,",", bb$pctB,",", rsi,",", macd$macd,",", macd$signal,",",
                                stochastic,")", sep = ""))
      }
      
      if(EMA_Strategy(df_price)[[1]] == TRUE){
        position_id <- sample(1:10000000, 1)
        dbSendQuery(mydb, paste("insert into Portfolio ( PositionID ,Symbol, Strategy, Market ,Status) values ("
                                ,position_id,", '", Symbol,"' ,", "'EMA', 'long', 'entry')"  , sep = ""))
        dbSendQuery(mydb, paste("insert into Entry (PositionID, Symbol, Price, Volume, Image)
                                    values (",
                                position_id,", '", Symbol,"' , ",
                                current_price,"," ,Volume,",",
                                charting(df_price),") ", sep = ""))
        
        
      } 
      if(EMA_Strategy(df_price)[[2]] == TRUE){
        position_id <- sample(1:10000000, 1)
        dbSendQuery(mydb, paste("insert into Portfolio ( PositionID ,Symbol, Strategy, Market ,Status) values ("
                                ,position_id,", '", Symbol,"' ,", "'EMA', 'short', 'entry')"  , sep = ""))
        dbSendQuery(mydb, paste("insert into Entry (PositionID, Symbol, Price, Volume, Image)
                                    values (",
                                position_id,", '", Symbol,"' , ",
                                current_price,"," ,Volume,",",
                                charting(df_price),") ", sep = ""))
        
      }
          
      if(Stochastic_Strategy(df_price,Symbol)[[1]] == TRUE){
        position_id <- sample(1:10000000, 1)
        dbSendQuery(mydb, paste("insert into Portfolio ( PositionID ,Symbol, Strategy, Market ,Status) values ("
                                ,position_id,", '", Symbol,"' ,", "'Stochastic', 'long', 'entry')"  , sep = ""))
        dbSendQuery(mydb, paste("insert into Entry (PositionID, Symbol, Price, Volume, Image)
                                    values (",
                                position_id,", '", Symbol,"' , ",
                                current_price,"," ,Volume,",",
                                charting(df_price),") ", sep = ""))
      }  
      if(Stochastic_Strategy(df_price,Symbol)[[2]] == TRUE){
          position_id <- sample(1:10000000, 1)
          dbSendQuery(mydb, paste("insert into Portfolio ( PositionID ,Symbol, Strategy, Market ,Status) values ("
                                  ,position_id,", '", Symbol,"' ,", "'Stochastic', 'short', 'entry')"  , sep = ""))
          dbSendQuery(mydb, paste("insert into Entry (PositionID, Symbol, Price, Volume, Image)
                                  values (",
                                  position_id,", '", Symbol,"' , ",
                                  current_price,"," ,Volume,",",
                                  charting(df_price),") ", sep = ""))
      }
    }
      
    df_for_Close <- dbSendQuery(mydb, paste("select p.PositionID , e.Price, p.Strategy, p.Market, e.Volume
                                 from Portfolio p left join Entry e 
                                 on p.PositionID = e.PositionID
                                 where p.Symbol = '",Symbol,"' and p.Status = 'entry'",sep = "")) %>% fetch(n = -1)
    
    if(dim(df_for_Close)[1] != 0){
      for(i in 1:nrow(df_for_Close)){
      
        if ((df_for_Close[i,"Strategy"] == "EMA") & (df_for_Close[i,"Market"] == "long")){
          if (EMA_Strategy(df_price)[[3]] == TRUE){
            dbSendQuery(mydb,  paste("insert into Close (PositionID, Symbol, Price)
                                    values (",df_for_Close[i,"PositionID"],", '",
                                     Symbol,"' ,", 
                                    current_price,")",sep = ""))
            dbSendQuery(mydb, paste("update Portfolio set 
                                      Revenue = ",round((current_price - df_for_Close[i,"Price"]) * df_for_Close[i,"Volume"]),
                                    ", Image = ", charting(df_price),
                                    ", Status = 'close'
                                      where PositionID = '",df_for_Close[i,"PositionID"],"' 
                                      and Symbol = '",Symbol,"'",sep = ""))
  
          }
        }
        if ((df_for_Close[i,"Strategy"] == "EMA") & (df_for_Close[i,"Market"] == "short")){
          if (EMA_Strategy(df_price)[[4]] == TRUE){
            dbSendQuery(mydb,  paste("insert into Close (PositionID, Symbol, Price)
                                    values (",df_for_Close[i,"PositionID"],", '",
                                     Symbol,"' ,", 
                                     current_price,")",sep = ""))
            dbSendQuery(mydb, paste("update Portfolio set 
                                      Revenue = ",round((df_for_Close[i,"Price"] - current_price) * df_for_Close[i,"Volume"]),
                                    ", Image = ", charting(df_price),
                                    ", Status = 'close'
                                      where PositionID = '",df_for_Close[i,"PositionID"],"' 
                                      and Symbol = '",Symbol,"'",sep = ""))
          }
        }
        if ((df_for_Close[i,"Strategy"] == "Stochastic") & (df_for_Close[i,"Market"] == "long")){
          if (Stochastic_Strategy(df_price,Symbol)[[3]] == TRUE){
            dbSendQuery(mydb,  paste("insert into Close (PositionID, Symbol, Price)
                                    values (",df_for_Close[i,"PositionID"],", '",
                                     Symbol,"' ,", 
                                     current_price,")",sep = ""))
            dbSendQuery(mydb, paste("update Portfolio set 
                                      Revenue = ", round((current_price - df_for_Close[i,"Price"]) * df_for_Close[i,"Volume"]),
                                    ", Image = ", charting(df_price),
                                    ", Status = 'close'
                                      where PositionID = '",df_for_Close[i,"PositionID"],"' 
                                      and Symbol = '",Symbol,"'",sep = ""))
          }
        }
        if ((df_for_Close[i,"Strategy"] == "Stochastic") & (df_for_Close[i,"Market"] == "short")){
          if (Stochastic_Strategy(df_price,Symbol)[[4]] == TRUE){
            dbSendQuery(mydb,  paste("insert into Close (PositionID, Symbol, Price)
                                    values (",df_for_Close[i,"PositionID"],", '",
                                     Symbol,"' ,", 
                                     current_price,")",sep = ""))
            dbSendQuery(mydb, paste("update Portfolio set 
                                      Revenue = ",round((df_for_Close[i,"Price"] - current_price) * df_for_Close[i,"Volume"]),
                                    ", Image = ", charting(df_price),
                                    ", Status = 'close'
                                      where PositionID = '",df_for_Close[i,"PositionID"],"' 
                                      and Symbol = '",Symbol,"'",sep = ""))
          }
        }
    }
    }
  }
  Sys.sleep(60)
}


