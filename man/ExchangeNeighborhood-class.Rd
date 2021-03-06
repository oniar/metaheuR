% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExchangeNeighborhood.R
\docType{class}
\name{ExchangeNeighborhood-class}
\alias{ExchangeNeighborhood-class}
\title{An S4 class to represent exchange (2-opt) neighborhoods}
\description{
An S4 class to represent exchange (2-opt) neighborhoods
}
\details{
The new neighbors are generated swapping positions indicated in \code{position.list}.
}
\section{Slots}{

\describe{
\item{\code{base}}{A factor vector which will be the solution whose neighborhood we will explore}

\item{\code{position.list}}{A mtrix with pairs of positions in the permutation}

\item{\code{random}}{A logical value indicating whether the exploration is at random or not}

\item{\code{id}}{Numeric value indicating the current position (in the \code{position.list}) that will be used to generate a new neighbor}
}}

