% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local_search.R
\name{iteratedLocalSearch}
\alias{iteratedLocalSearch}
\title{Iterated local search (ILS)}
\usage{
iteratedLocalSearch(evaluate, initial.solution, neighborhood, selector, perturb,
  accept = thresholdAccept, num.restarts = NULL, do.log = TRUE,
  verbose = TRUE, non.valid = "ignore", valid = allValid,
  correct = doNothing, resources = cResource(), ...)
}
\arguments{
\item{evaluate}{Function of a single parameter. Given a solution, the function returns its evaluation as a real number}

\item{initial.solution}{Solution from where the search will start}

\item{neighborhood}{Object representing the type of neighborhood to be used}

\item{selector}{A function used to select a solution from the neighborhood. For an example of the parameters required and the result it should produce, see \code{\link{greedySelector}}}

\item{perturb}{A perturbation function. This function has to contain at least one parameter, named \code{solution}, the solution to be perturbed}

\item{accept}{A function to determine when a new local optimum is accepted. This function has to have, at least, one parameter named \code{delta}, corresponding to the difference between the evaluation of the new solution and the existing one.}

\item{num.restarts}{Number of restarts to perform. If NULL, then restarts will be performed unitl the resources are finished}

\item{do.log}{Logic value to indicate whether the progress shoul be tracked or not}

\item{verbose}{Logic value to indicate whether the function should print information about the search}

\item{non.valid}{Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions}

\item{valid}{A function that, given a solution, determines whether it is valid or not}

\item{correct}{A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option}

\item{resources}{Object of class \code{\link{cResource}} representing the available computational resources for the search}

\item{...}{Special argument to pass additional parameters to the functions used in the search}
}
\value{
The function returns an object of class \code{\link{mHResult}} with all the information about the search
}
\description{
This function performs a basic ILS algorithm
}
\details{
If the function provided in the \code{generateSolution} parameter generates solutions completely at random, then the function performs a random multistart search; if the funciton generates random greedy solutions, then the function performs a GRASP-like search
}

