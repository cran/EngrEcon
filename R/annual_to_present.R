#' Calculate present value from a uniform annual payment 
#'  
#' \code{annual_to_present} Compute present value from uniform annual payments 
#'      using uniform series present worth factor (uswf)
#'      
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param A series of uniform annual payments
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
#' # Result: pr.value = $337733.5
#' annual_to_present(0.08, 30, 30000)  
#' 
#' @export
annual_to_present <- function(i, n, A){
  # compute uniform series present worth factor
  uswf <-((1+i)^n-1) / (i*(1+i)^n)
  
  # compute present value of costs
  pr.value <- A * uswf

  #Compute total present value
   sum(pr.value)
   
  #Send the output
  pr.value
  
}
