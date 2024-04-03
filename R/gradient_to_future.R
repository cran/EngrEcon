#' Calculate future value from uniform gradient payment 
#'  
#' \code{gradient_to_future} Compute future value from uniform gradient payments 
#'      using uniform gradient present worth factor (ugfw)
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param G uniform gradient payments
#'
#' @return fr.value
#'
#' @references
#'  Newnan, D. G., Eschenbach, T. G., Lavelle, J. P., & Oxford, N. Y. 
#'         Engineering Economic Analysis, 14th ed. 
#'         New York, Oxford University Press, 2020    
#' 
#'  David, W., & Terry, R. 
#'        Fundamentals of Engineering Economics and Decision Analysis. 
#'        Springer Nature, 2012
#' @examples  
#' # Result: fr.value = $ 312312
#' gradient_to_future(0.08, 30,300)  
#' 
#' @export
gradient_to_future <- function(i, n, G){
  # compute uniform series present worth factor
  ugfw <-(((1+i)^n-1) / (i^2))-(n/i)
  
  # compute present value of costs
  fr.value <- G * ugfw
  
  #Compute total present value
  sum(fr.value)
  
  #Send the output
  fr.value
  
}
