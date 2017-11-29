#install.packages("quantmod")
library(quantmod)
# start flip setup countdown


TomDemark <- function(symbol, f, t ) {
#  getSymbols(symbol, from = "2005-08-24", to="2005-10-14")
 #getSymbols(symbol, from = "2007-12-07", to = "2008-01-07")
 getSymbols(symbol, from = f, to = t)
  
  sym <- na.fill(get(symbol), "extend")
  #sym <- get(symbol)
  #sym <- eval(parse(text = symbol))
  cs <- chart_Series(sym)
  plot(cs)
  sym$ClLag <- lag(sym[,4],4)
  sym$Prev <- lag(sym[,4],1)
  sym$BS <- NA
  sym$SS <- NA
  sym$n <- 1:nrow(sym)
  sym$perfection <- NA
  sym$BuyCountdown <- NA
  sym$SellCountdown <- NA
  
  buy <- 0
  sell <- 0
  goingup <- 0
  goingdown <- 0

  BuyCountdown <- 0
  SellCountdown <- 0
  phase <- "start"
  
  for (x in 5:(nrow(sym)-1)) {  
    wasgoingup <- goingup
    wasgoingdown <- goingdown
    goingup <- (as.numeric( Cl(sym[x])) > as.numeric(Cl(sym[x-4] )))
    goingdown <- (as.numeric(Cl(sym[x])) < as.numeric(Cl(sym[x-4] )))
    
    if (phase == "start" || phase == "setup") {
      if (wasgoingup & goingdown) {
        # Bearish Price flip
        # Ct -1 > Ct -5 followed by Ct < Ct-4
        ifelse(phase=="start", buy <-  1, buy <- buy +1)
        phase <- "setup"
        sell <- 0
        cat( "bearish flip du ",format(index(sym[x])), x, "buy ", buy, "sell ", sell,phase, "\n")
        sym$BS[x] <- ifelse(buy >= 9, NA, buy)
        sym$SS[x] <- NA
      } else if (wasgoingup & goingup) {
        #Continuing Up
        #buy <- 0
        sell = sell + 1
        ifelse( sell >= 9 ,phase<-"countdown",phase<-phase)
        cat( "uu ", format(index(sym[x])), x, "buy ", buy, "sell ", sell,phase,"\n")
        sym$BS[x] <- NA
        sym$SS[x] <- sell
        
      } else if (wasgoingdown & goingup) {
        #Bullish Price flip
        # Ct -5 > Ct -1 followed by Ct -4 < Ct
        ifelse(phase=="start", sell <-  1, sell <- sell +1)
        phase <- "setup"
        cat( "bullish flip ud ", format(index(sym[x])), x, "buy ", buy, "seall ", sell,phase,"\n")
        sym$BS[x] <- NA
        sym$SS[x] <- ifelse(sell >= 9, NA, sell)
        
      } else {# (wasgoingdown & goingdown)
        #Continuing Down
        sell <- NA
        buy = buy + 1
        ifelse( buy >= 9 ,phase<-"countdown",phase<-phase)
        cat( "dd ", format(index(sym[x])), x, "buy ", buy, "sell ", sell,phase,"\n")
        sym$BS[x] <- ifelse(buy >= 9, NA, buy)
        sym$SS[x] <- NA
        
      }
    }  
    if (phase == "countdown") {
      if (  (as.numeric( Cl(sym[x])) <= as.numeric(Lo(sym[x-2] )))) {
        BuyCountdown <- BuyCountdown + 1
        sym$BuyCountdown[x] <- BuyCountdown
        cat( "BuyCountdown ", format(index(sym[x])), x, "buy ", buy, "sell ", sell,phase,BuyCountdown, "\n")
        if (BuyCountdown==13) {
          phase<-"start"
          buy<-0
          sell<-0
          BuyCountdown<-0
          cat("Count down complete","\n")
          }
      } else if (as.numeric( Cl(sym[x])) >= as.numeric(Lo(sym[x-2] )) ) {
        SellCountdown <- SellCountdown + 1
        sym$SellCountdown[x] <- SellCountdown
        cat( "SellCountdown ", format(index(sym[x])), x, "buy ", buy, "sell ", sell,phase,SellCountdown, "\n")
        if (SellCountdown==13) {
          phase<-"start"
          buy<-0
          sell<-0
          SellCountdown<-0
          cat("Count down complete","\n")
          
      }
      
    }
    
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
  
  lbBuyCountdown <- ifelse(ifelse(is.na(sym$BuyCountdown),0,sym$BuyCountdown)>0,sym$BuyCountdown, sym$BS)
  lbSellCountdown <- ifelse(sym$SellCountdown>0,sym$SellCountdown, sym$SS)
  
  text(x = sym$n , y = Lo(sym) *.994 , label = lbBuyCountdown, col=3)
  text(x = sym$n , y = Hi(sym) * 1.005 , label =lbSellCountdown, col=2)

    return(sym)
  #  arrows(AAPL$perfection,(AAPL$AAPL.Low)*.9, AAPL$perfection,(AAPL$AAPL.Low)*.95,col="black")
  
}



