#' Graph coloring problem
#'
#' This function generates an evaluation, validity and correction functions associated  with a classical graph coloring problem.
#' @family Problems
#' @param graph Graph to color
#' @return A list of functions to be used to solve a graph coloring problem. This includes the functions \code{evaluate}, for the evaluation of a solution, \code{valid}, to check whetehr a solution is valid or not, \code{correct}, to correct a non-valid solution and \code{plot} to graphically show the solution; all the functions have a single argument,  \code{solution}, representing the solution considered. Note that, given that the goal in all the algorithms in the library is minimizing the objective function, the \code{evaluate}. The solutions have to be vectors of factors indicating the color of each node
#' @examples
#'
#' library("igraph")
#' n <- 10
#' rnd.graph <- random.graph.game(n, p.or.m=0.5)
#' rnd.sol <- factor(paste("c",sample(1:5, size=n, replace=TRUE), sep=""),
#'                    levels = paste("c", 1:n, sep=""))
#' gcol.problem <- graphColoringProblem(rnd.graph)
#' gcol.problem$valid(rnd.sol)
#' corrected.sol <- gcol.problem$correct(rnd.sol)
#' gcol.problem$valid(corrected.sol)
#' gcol.problem$plot(rnd.sol)
#' gcol.problem$plot(corrected.sol)

hammingDistance <- function(s1, s2) {
  # funtzio honi deitzen zaionean jada berdinak dira tamainak if(length(s1)!=length(s2))
  return(sum(s1 != s2))
}
#probarako mat<-matrix(c(aaabc,baabc,cbbbc),ncol = length(aaabc),byrow = TRUE)
closestStringProblem <- function(matrix, alpha.vec) {
  size  <- ncol(matrix)
  num.string <- nrow(matrix)
  alphabet <- levels(factor(alpha.vec))
  
  evaluate <- function(solution) {
    if (!all(levels(factor(solution)) %in% alphabet)) {
      stop(paste(
        " The solution has to be of the same alphabet as the list of nodes"
      ))
    }
    
    if (length(solution) != size) {
      stop(paste(
        "The solution has to be of the same length as the list of nodes: ",
        size
      ))
    }
    f <- function(i) {
      return(hammingDistance(solution, matrix[i, ]))
    }
    distances <- sapply(1:num.string, FUN = f)
    return(max(distances))
  }
  
  valid <- function(solution) {
    if (!all(levels(factor(solution)) %in% alphabet)) {
      stop(paste(
        " The solution has to be of the same alphabet as the list of nodes"
      ))
    }
    
    if (length(solution) != size) {
      stop(paste(
        "The solution has to be of the same length as the list of nodes: ",
        size
      ))
    }
    
    if (length(levels(factor(solution))) > length(alpha.vec)) {
      return(FALSE)
    }
    return (all(levels(factor(solution)) %in% alpha.vec))
    
    
  }
  
  return(list(evaluate = evaluate, valid = valid))
}


#' Maximum independent set
#'
#' This function generates an evaluation, validity and correction functions associated  with a classical maximum independet set problem
#' @param graph Graph where we have to find the maximum independent set (MIS)
#' @return A list of functions to be used to solve a MIS problem. This includes the functions \code{evaluate}, for the evaluation of a solution, \code{valid}, to check whetehr a solution is valid or not, \code{correct}, to correct a non-valid solution and \code{plot} to graphically show the solution; all the functions have a single argument,  \code{solution}. The solutions passed to these functions has to be a logical vector indicating with \code{TRUE} which nodes are in the independent set.
#' @family Problems
#' @details The evaluation function includes another parameter, \code{penalization}, which can be used to penalize non-valid solutions. The penalization terms is the number of nodes that are connected in the solution, and it is weighted with the factor passed in the \code{penalization} parameter By default its value is 0.
#' @examples
#'
#' library("igraph")
#'

farthestStringProblem <- function (matrix, alpha.vec) {
  size  <- ncol(matrix)
  num.string <- nrow(matrix)
  #alphabet <- levels(factor(alpha.vec))
  
  evaluate <- function(solution) {
    if (!all(levels(factor(solution)) %in% alpha.vec)) {
      stop(paste(
        " The solution has to be of the same alphabet as the list of nodes"
      ))
    }
    if (length(solution) != size) {
      stop(paste(
        "The solution has to be of the same length as the list of nodes: ",
        size
      ))
    }
    
    f <- function(i) {
      return(hammingDistance(solution, matrix[i, ]))
    }
    distances <- sapply(1:num.string, FUN = f)
    return(min(distances))
  }
  
  valid <- function(solution) {
    if (!all(levels(factor(solution)) %in% alpha.vec)) {
      stop(paste(
        " The solution has to be of the same alphabet as the list of nodes"
      ))
    }
    if (length(solution) != size) {
      stop(paste(
        "The solution has to be of the same length as the list of nodes: ",
        size
      ))
    }
    
    if (length(levels(factor(solution))) > length(alpha.vec)) {
      return(FALSE)
    }
    return (all(levels(factor(solution)) %in% alpha.vec))
  }
  return(list(evaluate = evaluate, valid = valid))
}