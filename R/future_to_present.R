#' Compute present values from future payments (or cash flow)
#'  
#' \code{future_to_present} calculate present value from future value using 
#'      present worth factor (pwf) 
#'
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param FV future value
#'
#' @return pr.value 
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
#' # Result: pr.value = 2981.32
#' future_to_present(0.08, 30, 30000)  
#' 
#' @export
future_to_present <- function(i, n, FV){
  # compute present worth factor
  pwf <-1/((1+i)^n)
  
  # compute present value of costs
  pr.value <- FV * pwf
  
  #Compute total present value
  sum(pr.value)
  
  #Send the output
  pr.value
}
