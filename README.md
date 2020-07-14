# Class: CIS-544 DATA MINING & MACHINE LRNG
## Fantasy Stock Market Game
### last modified date 02/23/20

This project was about a Fantasy Stock Market Game - stocks competition. The students were seperated into teams of two students. Each player got 100,000 fantasy money.
The aim of the project was to increase its budget by developing complex machine learning algorithm in R which automatically buy and sell (day trading). Different rules were defined:
 - All trading must be done from R and completely automated.
 - You are limited to 2 calls per minute, maximum 300 API calls per day.
 - You must make at least 250 successful trades (buy/sell) per day.
 - All trading is to be done between 10am-4pm.
 - Failed transactions will result in a $1,000BDD penalty.
 - There is a minimum of 250 successful transactions per day. Any accounts below that threshold will be penalized after the market closes that day. The penalty will be $100 per missed transaction. So if you only make 100 successful transactions in a day, you will be penalized $1,500.

    Additional Rules at Feb 12:

 - At 4pm, budgets > $0.00 will be set to $0.00
 - All buy/sell transactions must come from AWS
 - You cannot have more than 30 transactions with the same company in the same day

## Getting Started / About this repository

In order to understand the structure of this repository, please read following instructions:

### 00_Archiv

In this folder there are old scripts, which are not used anymore.

### 01_Instances

In this folder you find the different instances, hosted on aws. Each instance was used in order to run a script with a different IP, so that more than 4 calls per minute were achieved.

### 02_DevelopmentOfCode

In this folder you find the development of the code. First, there was a play-safe strategy in order to avoid loosing money. In addition, some strategies were developed, which needed to be connected. You find the different strategies and functions in different R files.

### 03_FinalCode

In this folder you find the final code in two versions. The authors tried two versions of implementing the different strategies and funcitons. The difference between these versions is concerning the amount of connecting with the database in order to get signals or buy and sell open closing positions. 

### 04_Presentation

In this folder you find the final presentation of this project as a powerpoint.

## Prerequisites

Install in R following packages / libraries (the install.packages commands are commented out, if you need to install, please run it first.)
 - jsonlite
 - forecast
 - RMySQL
 - quantmod
 - magrittr

## Database

In order to connect with the database, which is hosted on aws, please connect via R in the final R scripts. The database called "Stock_Market" and shows different tables which are used in order to increase the chance of getting stocks which are profitable.
(may getting an error after 02/23 because of budget in aws)

## Built With

R (https://www.r-project.org)
RStudio as an environment (https://rstudio.com)

## Approach

We are running in the main part of the scripts a while loop between 10 am and 4 pm and while transactions are <= 250. Then we are sending signals to a database for possible buy and sell options (we call it SIGNAL GENERATOR). All Instances are checking for different stocks and are sending the signals to the databases. Doing this, we are getting signals of 12 Instances and each Instance is checking for different symbols. After this, we check for strategies (EMA, time series, stochastic), which are based on different approaches in order to identify profitable stocks. If the strategies apply (are TRUE) then open a position in the database. The main script pulls then the buy options and send a request to projectrex for buying. Now, an open position is created in the database, we check if the strategies for sell (closing position) apply. If the strategies aplly we send a request to projectrex for selling this stock. We track in the database if we won or lost money by buying and selling the stock. Doing this we can implement in the future deep learning methods in order to detect which stock is concerning the probability is the best to buy. In order to see this visually, please see the powerpoint, slide 5.



## How to run the code

The code is hosted on AWS EC2 Instances. Every day from Monday to Friday a cronjob startet automatically from 10 am to 4 pm the scripts in order to be ready for the market (e.g. for cronjob: 00 10 * * 1-5 Rscript filename.R). If you want to try the Code you can run the global functions, and the strategies. If you want to check the different strategies e.g. for "MSFT" (Microsoft), please get at first the Price with the function Price(Symbol,Key) (in this case Price("MSFT), Key). If you save this to a variable, you can insert this variable into the different strategies. Otherwise, you can run the script between 10 am and 4 pm, if the connection to alphavantager and projectrex is available. 

## Authors

Max Franke & 
Damian Etchevest


## License

This project was a part of the class CIS-544 DATA MINING & MACHINE LRNG at St Thomas University

## Acknowledgments

Dr. Mondesire

