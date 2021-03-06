% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acceptance_functions.R
\name{thresholdAccept}
\alias{thresholdAccept}
\title{Acceptance based on a threshold}
\usage{
thresholdAccept(delta, th = 0, ...)
}
\arguments{
\item{delta}{Increment in the evaluation function computed as the evaluation of the new solution minus the evaluation of the existing solution}

\item{th}{Threshold for the acceptance. By default, the threshold is 0, meaning that the solution is accepted if it improves the evaluation function (remember the package is programmed to minimize objective functions)}
}
\value{
\code{TRUE} when the increment is bellow the threshold
}
\description{
This function only accepts solutions (returns \code{TRUE}) when the increment in the objective function value is below a given threhold
}
\examples{
thresholdAccept (delta=-1, th=-2)
thresholdAccept (delta=-3, th=-2)
thresholdAccept (delta=-1, th=0)

}
\seealso{
Other Acceptance functions: \code{\link{alwaysAccept}},
  \code{\link{boltzmannAccept}}
}

