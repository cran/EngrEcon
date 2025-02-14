% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annual_to_future.R
\name{annual_to_future}
\alias{annual_to_future}
\title{Calculate future value from a uniform annual payment

\code{annual_to_future} Compute future values from uniform annual payments
using uniform series compound amount factor (uscaf)}
\usage{
annual_to_future(i, n, A)
}
\arguments{
\item{i}{discount rate in percent per year}

\item{n}{life span in years}

\item{A}{series of uniform annual payments}
}
\value{
FV
}
\description{
Calculate future value from a uniform annual payment

\code{annual_to_future} Compute future values from uniform annual payments
using uniform series compound amount factor (uscaf)
}
\examples{
 
# Result: FV = 5664161
annual_to_future(0.08, 30, 50000)  

}
\references{
Newnan, D. G., Eschenbach, T. G., Lavelle, J. P., & Oxford, N. Y.
Engineering Economic Analysis, 14th ed.
New York, Oxford University Press, 2020

David, W., & Terry, R.
Fundamentals of Engineering Economics and Decision Analysis.
Springer Nature, 2012
}
