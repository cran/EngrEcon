#' Calculate present value and annual value from a given future payments of cash flows 
#'  
#' \code{cashflow} Compute a given cash flow data's present value and annual value.
#'  The first column is the cash flow year;the rest is the cash flow money.
#'  The number of rows is the lifespan 
#'      
#' @param i annual interest rate in percentage
#' @param cashflowdata cash flow data containing the life span and money value of the cash flow for each year 
#' 
#' @return cash.flow.table
#' 
#'  
#' @examples 
#' # Result : cash.flow.table  
#' # Result : future value = 232540.78
#' # Result : present value = 206330.96   
#'year = c(1:5)             
#'capital = c(63000,1300,1300,1600,1300)
#'year1 = c(60000,0,10000,0,0)
#'year2 = c(0,0,30000,50000,0)
#'cashflowdata <- data.frame (year, capital, year1, year2)
#'cashflow(0.055, cashflowdata)
#' @export
cashflow <- function(i, cashflowdata){
  
  #Count the number of cost inputs
  number.of.cost.items <- length(cashflowdata[1,])
  span<-length(cashflowdata[,1])
  
  #Compute the future cash flow
  cash.flow.FV <- apply(cashflowdata, 1, sum, na.rm=TRUE)
  cash.flow.PV <- c()
  for(n in 1:span){cash.flow.PV[n] <- future_to_present(i, n-1, cash.flow.FV[n])}
  
  #Compute the Annual cash flow
  cash.flow.FV <- apply(cashflowdata, 1, sum, na.rm=TRUE)
  cash.flow.AV <- c()
  for(n in 1:span){cash.flow.AV[n] <- future_to_annual(i, n-1, cash.flow.FV[n])}
  
  #create output table
  cash.flow.table <- cbind(seq(0,span-1,1),cash.flow.FV, cash.flow.PV, cash.flow.AV )
  colnames(cash.flow.table) <- c("Year", "FV", "PV", "AV")
  rownames(cash.flow.table) <- NULL
  
  #Sum cash flow and calculate annualized value
  total.FV <- sum(cash.flow.FV, na.rm=TRUE)
  total.PV <- sum(cash.flow.PV, na.rm=TRUE)
  
  #Send an output
  cashflow.summary <- data.frame(c("Future Value", "Present Value"),c(total.FV, total.PV))
  colnames(cashflow.summary) <- NULL
  
  cashflow.out <- list()
  cashflow.out[[1]] <- cash.flow.table 
  cashflow.out[[2]] <- cashflow.summary
  cashflow.out
}


