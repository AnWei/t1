# FOR A GENERAL EXPLANATION OF REQUIREMENTS ON getNewPosList see rsi.R 

# This strategy only trades on certain markets, which is encoded in params$series.

# The strategy will be long (short) on contract whenever the close is below (above) 
# the lower (upper) Bollinger Band of the close.
# Holding period and stop loss are considered in this strategy. 


#params <- list(lookback=20,sdParam=1.5,series=c(1:4),hold=15,stoploss=0.05) initialization of the params for newbbands
getNewPosList <- function(store, newRowList, params) {
 
        if (is.null(store)) 
		store 	<- initStore(newRowList,params$series)
	else 
		store	<- updateStore(store, newRowList, params$series)
	
	pos <- rep(0,length(newRowList))
        
        #count: record the number of days of staying in a trade 
        #opprice: record the open price of each series
        count <- rep(NULL,length(newRowList)) 
        opprice <- rep(NULL,length(newRowList))
         
        if (store$iter > params$lookback) {

                   for (i in 1:length(params$series)) {
                       
                          opprice[params$series[i]] <- newRowList[[params$series[i]]]$Open
                          
                          if (store$pos[params$series[i]] == 0){
                           
                           cl           <- newRowList[[params$series[i]]]$Close
                           bbands 	<- BBands(store$cl[[i]],n=params$lookback,sd=params$sdParam)
                           dn 		<- last(bbands$dn)	
			   up 		<- last(bbands$up)
	
			   #cat("cl",cl,"dn",dn,"up",up,"\n")
               
			   if (cl < dn) {
				pos[params$series[i]] <- 1
			   }
			   else if (cl > up) {
				pos[params$series[i]] <- -1
			   }
                           if (store$pos[params$series[i]] != pos[params$series[i]]){
                           # start counting when a trade begins
                              count[params$series[i]]<- 1
                           }
                          
                        }
                          
                        # implemented by staying in a trade if count is smaller than hold and the loss of a trade is less than the stoploss.
                        else if (store$count[params$series[i]]<params$hold){
                                    return <- (opprice[params$series[i]]-store$opprice[params$series[i]])/(store$opprice[params$series[i]])
                                    #when buy (long position)
                                    if (store$pos[params$series[i]] == 1){
                                          if(return>(-params$stoploss)){
                                             count[params$series[i]] <- store$count[params$series[i]]+1
                                             pos[params$series[i]] <- store$pos[params$series[i]]
                                          }
                                     }
                                     #when sell (short position)
                                     else if (store$pos[params$series[i]] == -1){
                                          if(return<params$stoploss){
                                             count[params$series[i]] <- store$count[params$series[i]]+1
                                             pos[params$series[i]] <- store$pos[params$series[i]]
                                          }
                                     }
                        }
             }
         }
        store$opprice <- opprice
        store$count <- count
        store$pos <- pos
        #cat(store$iter,"------",store$pos,"\n")
        return(list(store=store,pos=pos))     
}

initClStore  <- function(newRowList,series) {
	return(lapply(series, function(x) newRowList[[x]]$Close))
}

updateClStore <- function(oldClStore, newRowList, series) {
	return(mapply(function(x,y) rbind(oldClStore[[x]],newRowList[[y]]$Close),1:length(series),series, SIMPLIFY=FALSE))
}

initStore <- function(newRowList,series) {
	return(list(iter=1,cl=initClStore(newRowList,series)))
}

updateStore <- function(store, newRowList, series) {
        store$iter 	<- store$iter + 1
        store$cl	<- updateClStore(store$cl,newRowList,series) 
	return(store)
}



        

