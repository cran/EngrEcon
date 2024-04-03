#' Calculate present value from uniform gradient payment 
#'  
#' \code{gradient_to_present} Compute present value from uniform gradient payments 
#'      using uniform gradient present worth factor (ugpw)
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param G uniform gradient payments
#'
#' @return pr.value
#'
#' @references
#' #'  Newnan, D. G., Eschenbach, T. G., Lavelle, J. P., & Oxford, N. Y. 
#'         Engineering Economic Analysis, 14th ed. 
#'         New York, Oxford University Press, 2020    
#' 
#'  David, W., & Terry, R. 
#'        Fundamentals of Engineering Economics and Decision Analysis. 
#'        Springer Nature, 2012
#'  
#' @examples  
#' # Result: pr.value = 310367.40
#' gradient_to_present(0.08, 30, 3000)
#' 
#' @export
gradient_to_present <- function(i, n, G){
  # compute uniform series present worth factor
  ugpw <-(((1+i)^n-1) / (i^2 *(1+i)^n))-(n/(i*(1+i)^n))
  
  # compute present value of costs
  pr.value <- G * ugpw
  
  #Compute total present value
  sum(pr.value)
  
  #Send the output
  pr.value
  
}
