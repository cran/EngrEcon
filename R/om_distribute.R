#' Calculate the present value of periodic operations and maintenance costs
#'   
#' \code{om_distribute} Distribute periodic present value operations and maintenance costs 
#'      over a project life span, discount over project, and compute present value
#'  
#' @param i discount rate in percent per year
#' @param n life span in years
#' @param fq frequency of cost in years
#' @param OM operation and maintenance cost incurred at each interval in present value
#'
#' @return OM_dist
#'
#' @references
#' Add citation as needed.
#' 
#' @examples
#' 
#' #Result is the present value cost of periodic operations and maintenance expenses.
#' # Result: idc ($) = 8174.547
#' om_distribute(0.03, 50, 3, 1000)  
#' 
#' @export
om_distribute <- function(i, n, fq, OM){
  
  #Distribute periodic O&M through time
  om_periodic <- rep(c(rep(0, length.out=fq-1), OM), length.out=n)
  
  #Compute discount factor through time
  disfact <- 1 / ((1+i) ^ seq(1,n))
  
  #Compute present value over the life span
  om_present <- sum(disfact * om_periodic)
  
  #Send the output
  om_present
}
