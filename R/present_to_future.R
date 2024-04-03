#' Compute future values from present value
#' 
#' \code{present_to_future} calculate future value from present value using
#'      compound amount factor (caf)   
#'     
#' @param i discount rate in percent per year
#' @param n life span in years 
#' @param PV present value
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
#'  
#' @examples  
#' # Result: fr.value = 9056391
#' 
#' present_to_future (0.08, 30, 900000)  
#' 
#' @export
present_to_future <- function(i, n, PV){
  # compute compound amount factor 
  caf <-(1+i)^n
  
  # compute the future value of costs
  fr.value <- PV * caf
  
  #Send the output
  fr.value
}
