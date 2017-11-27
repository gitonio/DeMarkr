#install.packages("quantmod")
#library(quantmod)


TomDemark <- function(symbol) {
  getSymbols(symbol, from = "2005-08-24", to="2005-10-14")
  sym <- eval(parse(text = symbol))
  cs <- chart_Series(sym)
  plot(cs)
  sym$ClLag <- lag(sym[,4],4)
  sym$Prev <- lag(sym[,4],1)
  sym$BS <- 0
  sym$SS <- 0
  sym$n <- 1:nrow(sym)
  sym$perfection <- NA
  sym$BuyCountdown <- 0
  
  buy <- 0
  sell <- 0
  goingup <- 0
  goingdown <- 0
  wasgoingdown <- 0
  wasgoingup <- 0
  ls <- length(sym)
  buysetup <- 0
  
  bcd <- 0
  tdsetup <- NULL
  phase <- "start"
  
  for (x in 6:(nrow(sym)-1)) {  
    wasgoingup <- goingup
    wasgoingdown <- goingdown
    # goingup <- as.numeric( Cl(sym[x]) > Lag(Cl(sym[x]),4 ))
    # goingdown <- as.numeric(Cl(sym[x]) < Lag(Cl(sym[x]),4 ))
    goingup <- (as.numeric( Cl(sym[x])) > as.numeric(Cl(sym[x-4] )))
    goingdown <- (as.numeric(Cl(sym[x])) < as.numeric(Cl(sym[x-4] )))
    
    if (wasgoingup & goingdown) {
      # Bearish Price flip
      in_buysetup <- TRUE
      # Ct -1 > Ct -5 followed by Ct < Ct-4
      ifelse(is.null(tdsetup), buy <-  1, buy <- buy +1)
      buysetup <- buy
      sell <- 0
      cat( "bearish flip du ",sym[x,1], x, "buy ", buy, "sell ", sell,tdsetup, "\n")
      #sym[x,9] <- 1
      sym$BS[x] <- ifelse(buy >= 9, 0, buy)
      #sym[x,10] <- NA
      sym$SS[x] <- 0
    } else if (wasgoingup & goingup) {
      #Continuing Up
      #buy <- 0
      sell = sell + 1
      cat( "uu ", x, "buy ", buy, "sell ", sell,"\n")
      sym[x,9] <- 0
      sym[x,10] <- sell
      
    } else if (wasgoingdown & goingup) {
      #Bullish Price flip
      # Ct -5 > Ct -1 followed by Ct -4 < Ct
      sell = 0
      #buy = 0
      cat( "bullish flip ud ", x, "buy ", buy, "sell ", sell,"\n")
      sym[x,9] <- 0
      sym[x,10] <- 0
      
    } else {# (wasgoingdown & goingdown)
      #Continuing Down
      in_buysetup <- TRUE
      
      sell <- 0
      buy = buy + 1
      buysetup <- buy
      cat( "dd ", x, "buy ", buy, "sell ", sell,"\n")
      sym$BS[x] <- ifelse(buy >= 9, 0, buy)
      sym$SS[x] <- 0
      
    }
    
    if ( (as.numeric(buysetup >= 9)) && (as.numeric( Cl(sym[x])) <= as.numeric(Lo(sym[x-2] )))) {
      tdsetup = "buycomplete"
      bcd <- bcd + 1
      sym$BuyCountdown[x] <- bcd
      #sym$BS[x] <- bcd
      cat("BuyCountdown ",x,"\n")
      
    }
    
    # if  ((buy == 9 && as.numeric(AAPL[x]$AAPL.Close) <= as.numeric(AAPL[x-2]$AAPL.Close) && as.numeric(AAPL[x]$AAPL.Close) <= as.numeric(AAPL[x-3]$AAPL.Close)) ||
    #      (buy == 9 && as.numeric(AAPL[x-1]$AAPL.Close) <= as.numeric(AAPL[x-2]$AAPL.Close) && as.numeric(AAPL[x-1]$AAPL.Close) <= as.numeric(AAPL[x-3]$AAPL.Close)))
    # {
    #   cat("buy perfection", "\n") 
    #   AAPL[x,12] <- x
    # }
    # if  ((sell == 9 && as.numeric(AAPL[x]$AAPL.Close) >= as.numeric(AAPL[x-2]$AAPL.Close) && as.numeric(AAPL[x]$AAPL.Close) >= as.numeric(AAPL[x-3]$AAPL.Close)) ||
    #      (sell == 9 && as.numeric(AAPL[x-1]$AAPL.Close) >= as.numeric(AAPL[x-2]$AAPL.Close) && as.numeric(AAPL[x-1]$AAPL.Close) >= as.numeric(AAPL[x-3]$AAPL.Close)))
    # {
    #   cat("sell perfection ",x, "\n") 
    #   AAPL[x,12] <- x
    # }
    
  }
  
  lb <- ifelse(sym$BuyCountdown>0,sym$BuyCountdown, sym$BS)
  text(x = sym$n , y = Hi(sym) * 1.01 , label = sym$SS, col=2)
  text(x = sym$n , y = Lo(sym) *.99 , label = lb, col=3)
  return(sym)
#  arrows(AAPL$perfection,(AAPL$AAPL.Low)*.9, AAPL$perfection,(AAPL$AAPL.Low)*.95,col="black")
  
}



# 
# 
# getSymbols("AAPL", from = "2016-10-01" , to="2017-01-01")
# getSymbols("MSFT", from = "2005-09-01", to="2005-12-01")
# chart_Series(MSFT)
# #getSymbols("AAPL", from = "2016-11-09" , to="2016-12-20")
# getSymbols("AAPL")
# chart_Series(AAPL, type="candlesticks")
# AAPL$AAPL.CloseLag <- lag(AAPL$AAPL.Close,4)
# AAPL$AAPL.Prev <- lag(AAPL$AAPL.Close,1)
# AAPL$AAPL.BS <- 0
# AAPL$AAPL.SS <- 0
# AAPL$n <- 1:nrow(AAPL)
# AAPL$perfection <- NA
# 
# AAPL$direction4 <- as.numeric(Cl(AAPL) > Lag(Cl(AAPL),4))
# AAPL$direction3 <- lag(AAPL$direction4,1)
# AAPL$SS <- unlist(Map( function(x,y) {as.numeric(x & y)}, as.numeric(Cl(AAPL) > Lag(Cl(AAPL),4)) , as.numeric(Lag(Cl(AAPL)) > Lag(Cl(AAPL),5))))
# AAPL$BS <- unlist(Map( function(x,y) {as.numeric(x & y)}, as.numeric(Cl(AAPL) < Lag(Cl(AAPL),4)) , as.numeric(Lag(Cl(AAPL)) < Lag(Cl(AAPL),5))))
# AAPL$S <- AAPL$SS + Lag(AAPL$SS)
# 
# 
# AAPL$SSL <- cumsum(AAPL$SS)
# buy <- 0
# sell <- 0
# goingup <- 0
# goingdown <- 0
# wasgoingdown <- 0
# wasgoingup <- 0
# for (x in 6:length(AAPL)-1) {  
#   wasgoingup <- goingup
#   wasgoingdown <- goingdown
#   goingup <- as.numeric(AAPL[x]$AAPL.Close > AAPL[x]$AAPL.CloseLag)
#   goingdown <- as.numeric(AAPL[x]$AAPL.Close < AAPL[x]$AAPL.CloseLag)
#   
#   if (wasgoingup & goingdown) {
#     # Bearish Price flip
#     # Ct -1 > Ct -5 followed by Ct < Ct-4
#     buy <- 0
#     sell <- 0
#     #cat( "du ", "buy ", buy, "sell ", sell,"\n")
#     AAPL[x,9] <- NA
#     AAPL[x,10] <- NA
#   } else if (wasgoingup & goingup) {
#     #Continuing Up
#     buy <- 0
#     sell = sell + 1
#     #cat( "uu ", "buy ", buy, "sell ", sell,"\n")
#     AAPL[x,9] <- NA
#     AAPL[x,10] <- sell
#     
#   } else if (wasgoingdown & goingup) {
#     #Bullish Price flip
#     # Ct -5 > Ct -1 followed by Ct -4 < Ct
#     sell = 0
#     buy = 0
#     #cat( "ud ", "buy ", buy, "sell ", sell,"\n")
#     AAPL[x,9] <- NA
#     AAPL[x,10] <- NA
#     
#   } else {# (wasgoingdown & goingdown)
#     #Continuing Down
#     sell <- 0
#     buy = buy + 1
#     #cat( "dd ", "buy ", buy, "sell ", sell,"\n")
#     AAPL[x,9] <- buy
#     AAPL[x,10] <- NA
#     
#   }
#   
#   if  ((buy == 9 && as.numeric(AAPL[x]$AAPL.Close) <= as.numeric(AAPL[x-2]$AAPL.Close) && as.numeric(AAPL[x]$AAPL.Close) <= as.numeric(AAPL[x-3]$AAPL.Close)) ||
#        (buy == 9 && as.numeric(AAPL[x-1]$AAPL.Close) <= as.numeric(AAPL[x-2]$AAPL.Close) && as.numeric(AAPL[x-1]$AAPL.Close) <= as.numeric(AAPL[x-3]$AAPL.Close)))
#   {
#     cat("buy perfection", "\n") 
#     AAPL[x,12] <- x
#   }
#   if  ((sell == 9 && as.numeric(AAPL[x]$AAPL.Close) >= as.numeric(AAPL[x-2]$AAPL.Close) && as.numeric(AAPL[x]$AAPL.Close) >= as.numeric(AAPL[x-3]$AAPL.Close)) ||
#        (sell == 9 && as.numeric(AAPL[x-1]$AAPL.Close) >= as.numeric(AAPL[x-2]$AAPL.Close) && as.numeric(AAPL[x-1]$AAPL.Close) >= as.numeric(AAPL[x-3]$AAPL.Close)))
#   {
#     cat("sell perfection ",x, "\n") 
#     AAPL[x,12] <- x
#   }
#   
# }
# 
# text(x = AAPL$n , y = (AAPL$AAPL.High) * 1.01 , label = AAPL$AAPL.BS, col=3)
# text(x = AAPL$n , y = (AAPL$AAPL.Low) *.99 , label = (AAPL$AAPL.SS), col=2)
# 
# arrows(AAPL$perfection,(AAPL$AAPL.Low)*.9, AAPL$perfection,(AAPL$AAPL.Low)*.95,col="black")


