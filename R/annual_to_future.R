#' Calculate future value from a uniform annual payment
#'  
#' \code{annual_to_future} Compute future values from uniform annual payments
#'      using uniform series compound amount factor (uscaf)  
#'     
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param A series of uniform annual payments
#'
#' @return FV 
#'
#' @references
#'  Newnan, D. G., Eschenbach, T. G., Lavelle, J. P., & Oxford, N. Y. 
#'         Engineering Economic Analysis, 14th ed. 
#'         New York, Oxford University Press, 2020    
#' 
#'  David, W., & Terry, R. 
#'        Fundamentals of Engineering Economics and Decision Analysis. 
#'        Springer Nature, 2012
#'         
#' @examples  
#' # Result: FV = 5664161
#' annual_to_future(0.08, 30, 50000)  
#' 
#' @export
annual_to_future <- function(i, n, A){
  # compute uniform series compound amount factor
  uscaf <- ((1+i)^n - 1) / i
  
  # compute future value of costs
  FV <- A * uscaf
  
  #Compute total future value
  sum(FV)

  #Send the output
  FV
}