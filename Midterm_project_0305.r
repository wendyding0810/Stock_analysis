###########################################################################################
###########################################################################################
###################              For Question 1 -6                    #####################
###########################################################################################
###########################################################################################


# Changing the working directory of files location.
setwd("/Users/twbanana/Downloads/CS636/Stocks")
IBM <- read.csv("IBM.csv")
MSFT <- read.csv("MSFT.csv")
GOOG <- read.csv("GOOG.csv")
AAPL <- read.csv("AAPL.csv")
AMZN <- read.csv("AMZN.csv")
FB <- read.csv("FB.csv")
NFLX <- read.csv("NFLX.csv")
TSLA <- read.csv("TSLA.csv")
ORCL <- read.csv("ORCL.csv")
SAP <- read.csv("SAP.csv")


####Close price for acj stock

IBM_Close <- IBM$Close
MSFT_Close <- MSFT$Close
GOOG_Close <- GOOG$Close
AAPL_Close <- AAPL$Close
AMZN_Close <- AMZN$Close
FB_Close <- FB$Close
NFLX_Close <- NFLX$Close
TSLA_Close <- TSLA$Close
ORCL_Close <- ORCL$Close
SAP_Close <- SAP$Close


#####Adj price for each stock
IBM_Adj.Close <- IBM$Adj.Close
MSFT_Adj.Close <- MSFT$Adj.Close
GOOG_Adj.Close <- GOOG$Adj.Close
AAPL_Adj.Close <- AAPL$Adj.Close
AMZN_Adj.Close <- AMZN$Adj.Close
FB_Adj.Close <- FB$Adj.Close
NFLX_Adj.Close <- NFLX$Adj.Close
TSLA_Adj.Close <- TSLA$Adj.Close
ORCL_Adj.Close <- ORCL$Adj.Close
SAP_Adj.Close <- SAP$Adj.Close



###### close_price_data
Close_Price_Data <- data.frame(cbind(IBM_Close,
                         MSFT_Close,
                         GOOG_Close,
                         AAPL_Close,
                         AMZN_Close,
                         FB_Close,
                         NFLX_Close,
                         TSLA_Close,
                         ORCL_Close,
                         SAP_Close))


names(Close_Price_Data) = c("IBM",
                           "MSFT",
                           "GOOG",
                           "AAPL",
                           "AMZN",
                           "FB",
                           "NFLX",
                           "TSLA",
                           "ORCL",
                           "SAP")
head(Close_Price_Data)



#####Adj.close price data
Adj.Close_Price_Data <- data.frame(cbind(IBM_Adj.Close,
                         MSFT_Adj.Close,
                         GOOG_Adj.Close,
                         AAPL_Adj.Close,
                         AMZN_Adj.Close,
                         FB_Adj.Close,
                         NFLX_Adj.Close,
                         TSLA_Adj.Close,
                         ORCL_Adj.Close,
                         SAP_Adj.Close))

names(Adj.Close_Price_Data) = c("IBM",
                           "MSFT",
                           "GOOG",
                           "AAPL",
                           "AMZN",
                           "FB",
                           "NFLX",
                           "TSLA",
                           "ORCL",
                           "SAP")
head(Adj.Close_Price_Data)


######################################################################
########### Calculate the Dividend for each stock ####################
######################################################################

############### analysis of data 
#### IBM 
pre_close=c(1,IBM$Close[1:(length(IBM$Close)-1)])
pre_adj_close=c(1,IBM$Adj.Close[1:(length(IBM$Close)-1)])
IBM_pre=cbind(IBM,ratio_close=(IBM$Close-pre_close)/IBM$Close,ratio_adj_close=(IBM$Adj.Close-pre_adj_close)/IBM$Adj.Close)  
diff_IBM_pre = (IBM_pre$ratio_close - IBM_pre$ratio_adj_close)*IBM_pre$Close
Dividend_table=c(0,abs(round(diff_IBM_pre[-1],2)))

####MSFT
pre_close=c(1,MSFT$Close[1:(length(IBM$Close)-1)])
pre_adj_close=c(1,MSFT$Adj.Close[1:(length(IBM$Close)-1)])
MSFT_pre=cbind(MSFT,ratio_close=(MSFT$Close-pre_close)/MSFT$Close,ratio_adj_close=(MSFT$Adj.Close-pre_adj_close)/MSFT$Adj.Close)  
diff_MSFT_pre = (MSFT_pre$ratio_close - MSFT_pre$ratio_adj_close)*MSFT_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_MSFT_pre[-1],2))))


####GOOG
pre_close=c(1,GOOG$Close[1:(length(GOOG$Close)-1)])
pre_adj_close=c(1,GOOG$Adj.Close[1:(length(GOOG$Close)-1)])
GOOG_pre=cbind(GOOG,ratio_close=(GOOG$Close-pre_close)/GOOG$Close,ratio_adj_close=(GOOG$Adj.Close-pre_adj_close)/GOOG$Adj.Close)  
diff_GOOG_pre = (GOOG_pre$ratio_close - GOOG_pre$ratio_adj_close)*GOOG_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_GOOG_pre[-1],2))))


####AAPL
pre_close=c(1,AAPL$Close[1:(length(AAPL$Close)-1)])
pre_adj_close=c(1,AAPL$Adj.Close[1:(length(AAPL$Close)-1)])
AAPL_pre=cbind(AAPL,ratio_close=(AAPL$Close-pre_close)/AAPL$Close,ratio_adj_close=(AAPL$Adj.Close-pre_adj_close)/AAPL$Adj.Close)  
diff_AAPL_pre = (AAPL_pre$ratio_close - AAPL_pre$ratio_adj_close)*AAPL_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_AAPL_pre[-1],2))))


####AMZN
pre_close=c(1,AMZN$Close[1:(length(AMZN$Close)-1)])
pre_adj_close=c(1,AMZN$Adj.Close[1:(length(AMZN$Close)-1)])
AMZN_pre=cbind(AMZN,ratio_close=(AMZN$Close-pre_close)/AMZN$Close,ratio_adj_close=(AMZN$Adj.Close-pre_adj_close)/AMZN$Adj.Close)  
diff_AMZN_pre = (AMZN_pre$ratio_close - AMZN_pre$ratio_adj_close)*AMZN_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_AMZN_pre[-1],2))))



####FB
pre_close=c(1,FB$Close[1:(length(FB$Close)-1)])
pre_adj_close=c(1,FB$Adj.Close[1:(length(FB$Close)-1)])
FB_pre=cbind(FB,ratio_close=(FB$Close-pre_close)/FB$Close,ratio_adj_close=(FB$Adj.Close-pre_adj_close)/FB$Adj.Close)  
diff_FB_pre = (FB_pre$ratio_close - FB_pre$ratio_adj_close)*FB_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_FB_pre[-1],2))))


####NFLX
pre_close=c(1,NFLX$Close[1:(length(NFLX$Close)-1)])
pre_adj_close=c(1,NFLX$Adj.Close[1:(length(NFLX$Close)-1)])
NFLX_pre=cbind(NFLX,ratio_close=(NFLX$Close-pre_close)/NFLX$Close,ratio_adj_close=(NFLX$Adj.Close-pre_adj_close)/NFLX$Adj.Close)  
diff_NFLX_pre = (NFLX_pre$ratio_close - NFLX_pre$ratio_adj_close)*NFLX_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_NFLX_pre[-1],2))))


####TSLA
pre_close=c(1,TSLA$Close[1:(length(TSLA$Close)-1)])
pre_adj_close=c(1,TSLA$Adj.Close[1:(length(TSLA$Close)-1)])
TSLA_pre=cbind(TSLA,ratio_close=(TSLA$Close-pre_close)/TSLA$Close,ratio_adj_close=(TSLA$Adj.Close-pre_adj_close)/TSLA$Adj.Close)  
diff_TSLA_pre = (TSLA_pre$ratio_close - TSLA_pre$ratio_adj_close)*TSLA_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_TSLA_pre[-1],2))))

####ORCL
pre_close=c(1,ORCL$Close[1:(length(ORCL$Close)-1)])
pre_adj_close=c(1,ORCL$Adj.Close[1:(length(ORCL$Close)-1)])
ORCL_pre=cbind(ORCL,ratio_close=(ORCL$Close-pre_close)/ORCL$Close,ratio_adj_close=(ORCL$Adj.Close-pre_adj_close)/ORCL$Adj.Close)  
diff_ORCL_pre = (ORCL_pre$ratio_close - ORCL_pre$ratio_adj_close)*ORCL_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_ORCL_pre[-1],2))))


####SAP
pre_close=c(1,SAP$Close[1:(length(SAP$Close)-1)])
pre_adj_close=c(1,SAP$Adj.Close[1:(length(SAP$Close)-1)])
SAP_pre=cbind(SAP,ratio_close=(SAP$Close-pre_close)/SAP$Close,ratio_adj_close=(SAP$Adj.Close-pre_adj_close)/SAP$Adj.Close)  
diff_SAP_pre = (SAP_pre$ratio_close - SAP_pre$ratio_adj_close)*SAP_pre$Close
Dividend_table=cbind(Dividend_table,c(0,abs(round(diff_SAP_pre[-1],2))))


####### Combine all dividend data ##############
Dividend_table=data.frame(Dividend_table)
names(Dividend_table) = c("IBM",
                          "MSFT",
                          "GOOG",
                          "AAPL",
                          "AMZN",
                          "FB",
                          "NFLX",
                          "TSLA",
                          "ORCL",
                          "SAP")



######################################################################
###########        Max_shares, Cashs, Daily_MTM         ##############
######################################################################

#####Calculate the max share, remaining cash and Daily_MTM
Cal_Max_Shares <- function(Close_Price, Buying_Power){
    shares <- Buying_Power %/% Close_Price
    return(shares)
}

Cal_Cash <- function(Shares, Close_Price, Buying_Power){
    Cash <- (Buying_Power - (Close_Price * Shares))
    return(Cash)
}

Cal_Daily_MTM <- function(myPortfolio, Close_Price_Data, i) {
        Count_Portfolio_Row <- nrow(myPortfolio)
        MTM_daily <- 0
        for (k in 1:Count_Portfolio_Row) {
            Stock_Index <- myPortfolio[k, 3]
            Shares <- myPortfolio[k, 4]
            Cash <- myPortfolio[k, 6]
            Close <- Close_Price_Data[i, Stock_Index]
            diviend <- Dividend_table[i,Stock_Index]
            MTM_daily <- sum((Shares * Close + Cash)+ MTM_daily+(diviend*Shares))
        }
        return(MTM_daily)
}

install.packages("formattable")
library(formattable)
Cal_Adj.Close_Diff <- function(old_adj_close, new_adj_close) {
    diff_adj_close <- percent((new_adj_close - old_adj_close) / old_adj_close)
    return (diff_adj_close)
}






#####################################################################
#################  Calculate MTM Buying High  #######################
#####################################################################

options(scipen = 999)
Count_Row <- nrow(Close_Price_Data)
Count_Col <- ncol(Adj.Close_Price_Data) 
MTM_Buying_High <- data.frame()
Value <- 5000000
# Rebalancing the portfolio and choosing the stocks every 5 days
for (i in seq(from = 1, to = Count_Row, by = 5)) {
    if (i > Count_Row) {
        break
    }
    
    Buying_Power <- Value / 5
    
    # Reset the Result and Data when rebalancing 
    Result <- data.frame()
    Data <- data.frame()
    
    # Getting the 5 stocks Adj Close dropped the most
    for (j in 1:Count_Col) {
        if (j > Count_Col) {
            break
        }
        # Getting Shares, Close, Cash in order to calculate MTM
        Shares <-Cal_Max_Shares(Close_Price_Data[i, j], Buying_Power)
        Close <- Close_Price_Data[i, j]
        New_Close <- Close_Price_Data[i + 5, j]
        Cash <- round(Cal_Cash(Shares, Close, Buying_Power), 2)
        
        
        # Getting the Adj.Close Difference
        Adj_Close <- Cal_Adj.Close_Diff(Close, New_Close) 
        Col_Name <- colnames(Adj.Close_Price_Data)[j]
        
        # Getting the Adj_Close, j, Shares, Close, Cash, New_Close as Columns
        # Corrospond to the Stock name as row
        Data = cbind.data.frame(Col_Name, Adj_Close, j, Shares, Close, Cash, New_Close)
        Result = rbind.data.frame(Result, Data)
        
    }
    
    names(Result) = c("Stock", "Adj_Close", "Stock_Index", "Shares", "Close", "Cash", "New_Close")
    
    
    # Initial portfolio we pick the first 5 stocks in the Result
    if (is.data.frame(MTM_Buying_High) && nrow(MTM_Buying_High) == 0){
        myPortfolio <- Result[1:5,]
        # sell the current Stocks with Close Price in 5 days as Value
        Value <- sum(myPortfolio$Shares * myPortfolio$New_Close + myPortfolio$Cash)
        
        #get the MTM daily results
        for(i in i : (i + 4)) {
            Daily_MTM <- Cal_Daily_MTM(myPortfolio, Close_Price_Data, i)
            MTM_Buying_High = rbind.data.frame(MTM_Buying_High, Daily_MTM)
        }
    }
    
    # After sorting, Get the 5 stocks Adj Close dropped the most
    else {
        res <- Result[order(Result$Adj_Close),]
        myPortfolio <- res[1:5,]
        
        # sell the current Stocks with Close Price in 5 days as Value
        Value <- sum(myPortfolio$Shares * myPortfolio$New_Close + myPortfolio$Cash)
        
        #get the MTM daily results
        for(i in i : (i + 4)) {
            Daily_MTM <- Cal_Daily_MTM(myPortfolio, Close_Price_Data, i)
            MTM_Buying_High = rbind.data.frame(MTM_Buying_High, Daily_MTM)
        }        
    }             
}
names(MTM_Buying_High) = "MTM"
MTM_Buying_High_five_result = MTM_Buying_High[1:251,]



#####################################################################
#################  Calculate MTM Buying Low  #######################
#####################################################################
#### Buying low stock 
options(scipen = 999)
Count_Row <- nrow(Close_Price_Data)
Count_Col <- ncol(Adj.Close_Price_Data) 
MTM_Buying_Low <- data.frame()
Value <- 5000000
# Rebalancing the portfolio and choosing the stocks every 5 days
for (i in seq(from = 1, to = Count_Row, by = 5)) {
    if (i > Count_Row) {
        break
    }
    
    Buying_Power <- Value / 5
    
    # Reset the Result and Data when rebalancing 
    Result <- data.frame()
    Data <- data.frame()
    
    # Getting the 5 stocks Adj Close dropped the most
    for (j in 1:Count_Col) {
        if (j > Count_Col) {
            break
        }
        # Getting Shares, Close, Cash in order to calculate MTM
        Shares <-Cal_Max_Shares(Close_Price_Data[i, j], Buying_Power)
        Close <- Close_Price_Data[i, j]
        New_Close <- Close_Price_Data[i + 5, j]
        Cash <- round(Cal_Cash(Shares, Close, Buying_Power), 2)
        
        
        # Getting the Adj.Close Difference
        Adj_Close <- Cal_Adj.Close_Diff(Adj.Close_Price_Data[i, j],Adj.Close_Price_Data[i + 5, j]) 
        Col_Name <- colnames(Adj.Close_Price_Data)[j]
        
        # Getting the Adj_Close, j, Shares, Close, Cash, New_Close as Columns
        # Corrospond to the Stock name as row
        Data = cbind.data.frame(Col_Name, Adj_Close, j, Shares, Close, Cash, New_Close)
        Result = rbind.data.frame(Result, Data)
        
    }
    
    names(Result) = c("Stock", "Adj_Close", "Stock_Index", "Shares", "Close", "Cash", "New_Close")
    
    
    # Initial portfolio we pick the first 5 stocks in the Result
    if (is.data.frame(MTM_Buying_Low) & nrow(MTM_Buying_Low) == 0){
        myPortfolio <- Result[1:5,]
        # sell the current Stocks with Close Price in 5 days as Value
        Value <- sum(myPortfolio$Shares * myPortfolio$New_Close + myPortfolio$Cash)
        
        #get the MTM daily results from curret portfolio
        for(i in i : (i + 4)) {
            Daily_MTM <- Cal_Daily_MTM(myPortfolio, Close_Price_Data, i)
            MTM_Buying_Low = rbind.data.frame(MTM_Buying_Low, Daily_MTM)
        }
    }
    
    # After sorting, Get the 5 stocks Adj Close dropped the most
    else {
        res <- Result[order(-Result$Adj_Close),]
        myPortfolio <- res[1:5,]
        
        # sell the current Stocks with Close Price in 5 days as Value
        Value <- sum(myPortfolio$Shares * myPortfolio$New_Close + myPortfolio$Cash)
        
        #get the MTM daily results from curret portfolio
        for(i in i : (i + 4)) {
            Daily_MTM <- Cal_Daily_MTM(myPortfolio, Close_Price_Data, i)
            MTM_Buying_Low = rbind.data.frame(MTM_Buying_Low, Daily_MTM)
        }        
    }             
}
names(MTM_Buying_Low) = "MTM"
MTM_Buying_Low
MTM_Buying_Low_five_result = MTM_Buying_Low[1:251,]



##########################################################################################
#######################################  Plotting  #######################################
##########################################################################################
##########################################################################################
#############  7. create a "high tech index" which is simply the daily       #############
#############  average of the 10 stocks "Close" prices.                      #############
#############  Compare your MTM series with the "high tech index"            #############
#############  and plot their curves.                                        #############
##########################################################################################
##########################################################################################

###### 1. Calculate daily mean of ten stocks  

combine_close_1 = cbind(IBM_Close,MSFT_Close)
combine_close_2 = cbind(combine_close_1,GOOG_Close)
combine_close_3 = cbind(combine_close_2,AAPL_Close)
combine_close_4 = cbind(combine_close_3,AMZN_Close)
combine_close_5 = cbind(combine_close_4,FB_Close)
combine_close_6 = cbind(combine_close_5,NFLX_Close)
combine_close_7 = cbind(combine_close_6,TSLA_Close)
combine_close_8 = cbind(combine_close_7,ORCL_Close)
combine_close_9 = cbind(combine_close_8,SAP_Close)

stock_daily_mean_0 = rowMeans(combine_close_9, na.rm = FALSE, dims = 1)

stock_daily_mean = (stock_daily_mean_0-stock_daily_mean_0[1])/stock_daily_mean_0[1]


######## 2. draw a high_tech_index to compare stock_daily_mean, MTM_Buying_High_five_result, MTM_Buying_Low_five_result
install.packages("tidyverse")
install.packages("plotly")
library(tidyverse)
library(plotly)
MTM_Buying_High_start_price = MTM_Buying_High[2,1]
MTM_Buying_Low_start_price = MTM_Buying_Low[2,1]

Percentage_high = (MTM_Buying_High_five_result-MTM_Buying_High_start_price)/MTM_Buying_High_start_price
Percentage_low = (MTM_Buying_Low_five_result-MTM_Buying_High_start_price)/MTM_Buying_High_start_price


data.frame(Date = IBM$Date,stock_daily_mean, Percentage_low, Percentage_high ) %>% 
    reshape2::melt(id.vars = "Date") %>%
    plot_ly(x=~Date,y=~value,color =~variable )%>% add_lines




##########################################################################################
#######################################  Plotting  #######################################
##########################################################################################
##########################################################################################
#############  8. Download the USD/JPY 2018 historical data                  #############
#############  then use the "Close" column as the rate to                    #############
#############  convert your MTM series from USD to JPY.                      #############
#############  Plot the two MTM curves. You will need to convert to          #############
#############  daily percentage change too.                                  #############
##########################################################################################
##########################################################################################
 

#############  1. selecting date from recrrency ####################
 

JPY_currency = read.csv("dollar-yen-exchange-rate-historical-chart.csv")

JPY_currency$date = as.Date(JPY_currency$date,'%m/%d/%Y')

J_2018 = JPY_currency[JPY_currency$date%in%as.Date(as.character(IBM$Date),"%Y-%m-%d"),]

Cur_IBM = cbind(IBM,J_2018)
Cur_MSFT = cbind(MSFT,J_2018)
Cur_GOOG =cbind(GOOG,J_2018)
Cur_AAPL =cbind(AAPL,J_2018)
Cur_AMZN =cbind(AMZN,J_2018)
Cur_FB =cbind(FB,J_2018)
Cur_NFLX =cbind(NFLX,J_2018)
Cur_TSLA =cbind(TSLA,J_2018)
Cur_ORCL =cbind(ORCL,J_2018)
Cur_SAP =cbind(SAP,J_2018)



####################  2.exchange USD to JPY  ######################


MTM_high_JPY = MTM_Buying_High_five_result  * J_2018$value
MTM_high_JPY
MTM_high_JPY_percentage = (MTM_high_JPY-MTM_high_JPY[1])/MTM_high_JPY[1]


MTM_low_JPY = MTM_Buying_Low_five_result  * J_2018$value
MTM_low_JPY
MTM_low_JPY_percentage = (MTM_low_JPY-MTM_low_JPY[1])/MTM_low_JPY[1]

####################  3. draw a high_tech_index after USD change to JPY  ######################
install.packages("tidyverse")
install.packages("plotly")
library(tidyverse)
library(plotly)
options(warn=-1)
data.frame(Date = IBM$Date,MTM_high_JPY_percentage,MTM_low_JPY_percentage) %>% 
    reshape2::melt(id.vars = "Date") %>%
    plot_ly(x=~Date,y=~value,color =~variable )%>% add_lines
options(warn=0)

#data.frame(Date = IBM$Date,Percentage_high, MTM_high_JPY_percentage ) %>% 
#    reshape2::melt(id.vars = "Date") %>%
#    plot_ly(x=~Date,y=~value,color =~variable )%>% add_lines




##########################################################################################################
#################################### Calculate different interval ########################################
##########################################################################################################

################ Calculate buying high #########################

options(scipen = 999)
Count_Row <- nrow(Close_Price_Data)
Count_Col <- ncol(Adj.Close_Price_Data)
MTM_buying_high_interval=data.frame()

for (k in 1:Count_Row){  
    MTM_Buying_High <- data.frame()
    Value <- 5000000
    # Rebalancing the portfolio and choosing the stocks every 5 days
    
    for (i in seq(from = 1, to = Count_Row, by = k)) {
        if (i > Count_Row) {
            break
        }
        
        Buying_Power <- Value / 5
        
        # Reset the Result and Data when rebalancing 
        Result <- data.frame()
        Data <- data.frame()
        
        # Getting the 5 stocks Adj Close dropped the most
        for (j in 1:Count_Col) {
            if (j > Count_Col) {
                break
            }
            # Getting Shares, Close, Cash in order to calculate MTM
            Shares <-Cal_Max_Shares(Close_Price_Data[i, j], Buying_Power)
            Close <- Close_Price_Data[i, j]
            New_Close <- Close_Price_Data[i + k, j]
            Cash <- round(Cal_Cash(Shares, Close, Buying_Power), 2)
            
            
            
            # Getting the Adj.Close Difference
            Adj_Close <- Cal_Adj.Close_Diff(Close, New_Close) 
            Col_Name <- colnames(Adj.Close_Price_Data)[j]
            
            # Getting the Adj_Close, j, Shares, Close, Cash, New_Close as Columns
            # Corrospond to the Stock name as row
            Data = cbind.data.frame(Col_Name, Adj_Close, j, Shares, Close, Cash, New_Close)
            Result = rbind.data.frame(Result, Data)
            
        }
        
        names(Result) = c("Stock", "Adj_Close", "Stock_Index", "Shares", "Close", "Cash", "New_Close")
        
        
        # Initial portfolio we pick the first 5 stocks in the Result
        if (is.data.frame(MTM_Buying_High) && nrow(MTM_Buying_High) == 0){
            myPortfolio <- Result[1:5,]
            # sell the current Stocks with Close Price in 5 days as Value
            Value <- sum(myPortfolio$Shares * myPortfolio$New_Close + myPortfolio$Cash)
            
            #get the MTM daily results
            for(i in i : (i + k-1)) {
                Daily_MTM <- Cal_Daily_MTM(myPortfolio, Close_Price_Data, i)
                MTM_Buying_High = rbind.data.frame(MTM_Buying_High, Daily_MTM)
            }
        }
        
        # After sorting, Get the 5 stocks Adj Close dropped the most
        else {
            res <- Result[order(Result$Adj_Close),]
            myPortfolio <- res[1:5,]
            
            # sell the current Stocks with Close Price in 5 days as Value
            Value <- sum(myPortfolio$Shares * myPortfolio$New_Close + myPortfolio$Cash)
            
            #get the MTM daily results
            for(i in i : (i + k-1)) {
                Daily_MTM <- Cal_Daily_MTM(myPortfolio, Close_Price_Data, i)
                MTM_Buying_High = rbind.data.frame(MTM_Buying_High, Daily_MTM)
                
            }        
        }
    }
    MTM_buying_high_interval=rbind.data.frame(MTM_buying_high_interval,MTM_Buying_High[251,])
}

MTM_buying_high_interval

####Find the max value of MTM_buying_high_interval

best_cycle_day_for_high = which.max(MTM_buying_high_interval[,1])
best_cycle_day_for_high




##########################################################################################################
#################################### Calculate different interval ########################################
##########################################################################################################

############# Calculate buying low #########################

options(scipen = 999)
Count_Row <- nrow(Close_Price_Data)
Count_Col <- ncol(Adj.Close_Price_Data)
MTM_Buying_Low_interval=data.frame()

for (k in 1:Count_Row){  
    MTM_Buying_Low <- data.frame()
    Value <- 5000000
    # Rebalancing the portfolio and choosing the stocks every 5 days
    
    for (i in seq(from = 1, to = Count_Row, by = k)) {
        if (i > Count_Row) {
            break
        }
        
        Buying_Power <- Value / 5
        
        # Reset the Result and Data when rebalancing 
        Result <- data.frame()
        Data <- data.frame()
        
        # Getting the 5 stocks Adj Close dropped the most
        for (j in 1:Count_Col) {
            if (j > Count_Col) {
                break
            }
            # Getting Shares, Close, Cash in order to calculate MTM
            Shares <-Cal_Max_Shares(Close_Price_Data[i, j], Buying_Power)
            Close <- Close_Price_Data[i, j]
            New_Close <- Close_Price_Data[i + k, j]
            Cash <- round(Cal_Cash(Shares, Close, Buying_Power), 2)
            
            
            
            # Getting the Adj.Close Difference
            Adj_Close <- Cal_Adj.Close_Diff(Close, New_Close) 
            Col_Name <- colnames(Adj.Close_Price_Data)[j]
            
            # Getting the Adj_Close, j, Shares, Close, Cash, New_Close as Columns
            # Corrospond to the Stock name as row
            Data = cbind.data.frame(Col_Name, Adj_Close, j, Shares, Close, Cash, New_Close)
            Result = rbind.data.frame(Result, Data)
            
        }
        
        names(Result) = c("Stock", "Adj_Close", "Stock_Index", "Shares", "Close", "Cash", "New_Close")
        
        
        # Initial portfolio we pick the first 5 stocks in the Result
        if (is.data.frame(MTM_Buying_Low) && nrow(MTM_Buying_Low) == 0){
            myPortfolio <- Result[1:5,]
            # sell the current Stocks with Close Price in 5 days as Value
            Value <- sum(myPortfolio$Shares * myPortfolio$New_Close + myPortfolio$Cash)
            
            #get the MTM daily results
            for(i in i : (i + k-1)) {
                Daily_MTM <- Cal_Daily_MTM(myPortfolio, Close_Price_Data, i)
                MTM_Buying_Low = rbind.data.frame(MTM_Buying_Low, Daily_MTM)
            }
        }
        
        # After sorting, Get the 5 stocks Adj Close dropped the most
        else {
            res <- Result[order(-Result$Adj_Close),]
            myPortfolio <- res[1:5,]
            
            # sell the current Stocks with Close Price in 5 days as Value
            Value <- sum(myPortfolio$Shares * myPortfolio$New_Close + myPortfolio$Cash)
            
            #get the MTM daily results
            for(i in i : (i + k-1)) {
                Daily_MTM <- Cal_Daily_MTM(myPortfolio, Close_Price_Data, i)
                MTM_Buying_Low = rbind.data.frame(MTM_Buying_Low, Daily_MTM)
                
            }        
        }
    }
    MTM_Buying_Low_interval=rbind.data.frame(MTM_Buying_Low_interval,MTM_Buying_Low[251,])
}

MTM_Buying_Low_interval

####Find the max value of MTM_Buying_Low_interval

best_cycle_day_for_low = which.max(MTM_Buying_Low_interval[,1])
best_cycle_day_for_low




