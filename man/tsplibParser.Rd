% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{tsplibParser}
\alias{tsplibParser}
\title{Function to read cost matrices from TSPlib}
\usage{
tsplibParser(file)
}
\arguments{
\item{file}{It can be a local file or a URL corresponding to an XML file or compressed (zip) XML file}
}
\value{
A matrix containing the cost associated to each pair of cities
}
\description{
This function reads XML files (compressed or not) from TSPlib and builds the cost matrix associated
}
\seealso{
\code{\link{tspProblem}}
}

