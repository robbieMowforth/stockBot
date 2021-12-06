install.packages('quantmod', dependencies = TRUE)
install.packages('TTR', dependencies = TRUE)
install.packages('hash', dependencies = TRUE)

library(TTR) 
library(quantmod)
library(hash)

#TODO:
# - Connect with SQL
# - Making a better error checking solution for bad stock tickers (Use dates due too NA remove?)
# - Make the user signal input area more formalized & research more signal methods
# - Add test enviroment where it places fake money

#===START OF FUNCTIONS===#

#Main data loading function, main features:
#1. Loads all data from a list of tickers into one table
#2. Provides Buy,Sell,Hold signals for each given day of each stock ticker
#3. Stores in SQL friendly table structure
DataLoad = function(symbols,dataset,date,sett){
  
  # cool progress bar to see the % of completion
  n = length(symbols)
  pb = txtProgressBar(min = 0, max = n, style=3)
  
  #start of loop though symbols
  for(i in 1:length(symbols)) {
    symbol = symbols[i]
    # does to test to see if the ticker can be identified
    tryit = try(getSymbols(symbol, from=date, to=Sys.Date(), src='yahoo'))
    if(inherits(tryit, "try-error")){
      
      cat(str(symbol),"Couldn't Load") #make this look better, error for symbol not loading
      i = i+1
      
    } else {
      #This means the ticker was found, now we begin to load the data for the ticker
      data = na.omit(getSymbols(symbol, from=date, to=Sys.Date(), src='yahoo', auto.assign=FALSE))
      
      #merge in all the signal checkers in this area below
      data = merge(data, BBands(HLC(data), n=sett[["BBands_n"]], sd=sett[["BBands_sd"]]))
      data = merge(data, MACD(data[,6], nFast=sett[["MACD_nFast"]], nSlow=sett[["MACD_nSlow"]], nSig=sett[["MACD_nSig"]]))
      data = merge(data, RSI(data[,6], n=sett[["RSI_n"]]))
      data = merge(data, ADX(HLC(data), n=sett[["ADX_n"]]))

      #creates the column which we store the Buy,Sell,Hold (BSH) signals in
      data = merge(data, "BSHsignals")
      colnames(data)[ncol(data)] = "BSHsignals"
      
      data = data[-c(1:33),] #removes all NA values, signal line values from MACD drives this
      
      #Generation of the BSH signals
      data = SignalMakerFunc(data, sett)
      
      #Grabs the name of the ticker from colname manipulation, then adds a column for the ticker name
      
      TickerStrCheck = gregexpr(pattern = '.Adjusted', colnames(data)[6])
      TickerName = substr(colnames(data)[1],1,as.integer(TickerStrCheck[1])-1)
      data = merge(data, TickerName)
      
      #cleans the ticker name from the column names
      for(nm in 1:6){
        colnames(data)[nm] = substr(colnames(data)[nm],nchar(TickerName)+2,nchar(colnames(data)[nm]))
      }
      
      #turns the xts() series into a dataframe that stores the date as a new column
      dataDF = data.frame(date=index(data),coredata(data))
      
      #merges the complete data.frame to the main dataset to return to the user
      dataset = rbind(dataset,dataDF)
      setTxtProgressBar(pb, i)
      
    }
  }
  
  #return the data table
  return(dataset)
}

SignalMakerFunc = function(stockData, sett){
  
  
  for(j in 1:nrow(stockData)){
    signalChoose = ifelse(stockData[j,]$rsi < sett[["RSI_low"]] &
                          stockData[j,]$pctB < 0.19 &
                          ##stockData[j,]$macd < stockData[j,]$signal & ===NOT WORKING=== WHHHHHY?!?!?!
                          stockData[j,]$DIp < stockData[j,]$DIn &
                          stockData[j,]$ADX < 25,
                          "Buy",
                          
                          ifelse(stockData[j,]$rsi > sett[["RSI_high"]] &
                                  stockData[j,]$pctB > 0.81 &
                                  ##stockData[j,]$macd > stockData[j,]$signal & ===NOT WORKING===
                                  stockData[j,]$DIp > stockData[j,]$DIn &
                                  stockData[j,]$ADX < 25,
                                             "Sell", 
                                             "Hold"
                                             )
                                      )
    stockData[j,]$BSHsignals = signalChoose
    
    
    
  }
  
  return(stockData)
  
}

#===RUNNING AREA===#

# Fetch all Symbols & store only the tickers to retrieve the data
datasetALL = data.frame()
symbolsALL = stockSymbols()
symbolsALL = symbolsALL[,1]
symbolsClean = c("AP-WT") #formalize this 
symbolsALL = symbolsALL[!symbolsALL %in% symbolsClean]
dateALL = '2019-01-01'

datasetALL = DataLoad(symbolsALL, datasetALL, dateALL, sigSettM1)

#Custom list of Robbie Stocks
datasetRM = data.frame()

symbolsRM = c("BYND","AAPL","REGI","TDOC","JET")
dateRM = '2020-10-01'

#Signals Method 1, make this one line?
sigSettM1 = hash()

sigSettM1[["BBands_n"]]=20; sigSettM1[["BBands_sd"]]=2
sigSettM1[["MACD_nFast"]]=12; sigSettM1[["MACD_nSlow"]]=26; sigSettM1[["MACD_nSig"]]=9
sigSettM1[["RSI_n"]]=14
sigSettM1[["ADX_n"]]=14
sigSettM1[["RSI_high"]]=69; sigSettM1[["RSI_low"]]=31
sigSettM1[["MACD_Enabled"]]=TRUE
sigSettM1[["BBands_Enabled"]]=TRUE
sigSettM1[["ADX_Enabled"]]=TRUE

datasetRM = DataLoad(symbolsRM, datasetRM, dateRM, sigSettM1)
names(datasetRM)
#===TEST AREA===#

getSymbols('BYND', from="2020-10-01", to=Sys.Date())
chartSeries(BYND,
            theme = 'white',
            TA='addBBands();addMACD();addADX();addRSI()',
            subset = paste('2020-11-01::',Sys.Date(),sep=""))
addMACD()
addBBands()

sigSettM1 = hash()

sigSettM1[["BBands_n"]]=20; sigSettM1[["BBands_sd"]]=2
sigSettM1[["MACD_nFast"]]=12; sigSettM1[["MACD_nSlow"]]=26; sigSettM1[["MACD_nSig"]]=9
sigSettM1[["RSI_n"]]=14
sigSettM1[["ADX_n"]]=14
sigSettM1[["RSI_high"]]=69; sigSettM1[["RSI_low"]]=31
sigSettM1[["MACD_Enabled"]]=TRUE
sigSettM1[["BBands_Enabled"]]=TRUE
sigSettM1[["ADX_Enabled"]]=TRUE

stockData = na.omit(getSymbols('JET', from="2020-10-01", to=Sys.Date(), src='yahoo', auto.assign=FALSE))
stockData = merge(stockData, BBands(HLC(stockData), n=sigSettM1[["BBands_n"]], sd=sigSettM1[["BBands_sd"]]))
stockData = merge(stockData, MACD(stockData[,6], nFast=sigSettM1[["MACD_nFast"]], nSlow=sigSettM1[["MACD_nSlow"]], nSig=sigSettM1[["MACD_nSig"]]))
stockData = merge(stockData, RSI(stockData[,6], n=sigSettM1[["RSI_n"]]))
stockData = merge(stockData, ADX(HLC(stockData), n=sigSettM1[["ADX_n"]]))

stockData = stockData[-c(1:33),]

stockData = merge(stockData, "rsi_lv_high")
colnames(stockData)[ncol(stockData)] = "rsi_lv_high"
stockData = merge(stockData, "rsi_lv_low")
colnames(stockData)[ncol(stockData)] = "rsi_lv_low"
rsi_high = c()
rsi_low = c()
j=1
for(j in 1:nrow(stockData)){
  if(stockData[j,]$rsi > 60){ rsi_high = append(rsi_high, stockData[j,]$rsi) }
  else { rsi_low = append(rsi_low, stockData[j,]$rsi) }
  
  stockData[j,]$rsi_lv_high = mean(rsi_high)
  stockData[j,]$rsi_lv_low = mean(rsi_low)
  
}

j=34
stockData[j,]$macd
stockData[j,]$signal
if(stockData[j,]$rsi < sigSettM1[["RSI_low"]] &
   stockData[j,]$pctB < 0.19 &
   stockData[j,]$macd < stockData[j,]$signal &
   stockData[j,]$DIp < stockData[j,]$DIn &
   stockData[j,]$ADX < 25 
){stockData[j,]$signal}

if(stockData[j,]$rsi < sigSettM1[["RSI_low"]] &&
   stockData[j,]$macd < stockData[j,]$signal && #===FIX THIS TOMORROW===#
   stockData[j,]$pctB < 0.19 ##&&
   ##stockData[j,]$ADX < 25 &&
   ##stockData[j,]$Dlp < stockData[j,]$Dln
){ stockData[j,]$BSHsignals = "Buy"

} ifelse(stockData[j,]$rsi > sigSettM1[["RSI_high"]] &&
           stockData[j,]$macd > stockData[j,]$signal && #Work on boolean check to do this
           stockData[j,]$pctB > 0.81 ##&& #hash this
         ##stockData[j,]$ADX < 25 && #hash this
         ##stockData[j,]$Dlp > stockData[j,]$Dln #Work on boolean check to do this
) { stockData[j,]$BSHsignals = "Sell"

} else { stockData[j,]$BSHsignals = "Hold" }
