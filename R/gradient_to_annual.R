#' Calculate annual value from uniform gradient payment 
#'  
#' \code{gradient_to_annual} Compute annual value from uniform gradient payments 
#'      using uniform gra present worth factor (ugaw)
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param G uniform gradient payments
#' @param A initial annual payment
#' 
#' @return ann.value
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
#' # Result: ann.value = $ 105138.30
#' gradient_to_annual(0.08, 30, 6000, 50000)  
#' 
#' @export
gradient_to_annual <- function(i, n, G, A){
  # compute uniform series present worth factor
  ugaw <- (1/i)-(n/(((1+i)^n)-1))
  
  # compute present value of costs
  ann.value <- (G * ugaw) + A
  
  #Compute total present value
  sum(ann.value)
  
  #Send the output
  ann.value
  
}
