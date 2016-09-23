#' A function to calculate variance explained by each Principal Component for MEDALS
#'
#' This function returns the variance explained by each PC and the cumulative variance explained in the medals package, this should be used with the output of imaging.cp
#' @param mat The crossproduct matrix, this can be the output of imaging.cp
#' @param n The number of observations in the original matrix. This can be the output for imaging.suff.stat
#' @keywords MEDALS, variance
#' @export
#' @return List of 2 elements: \code{var}: variance of each
#' component, \code{cum_pct}: cumulative percent variance explained
pc.var <- function(mat,n){
  var.pc <- svd(mat)$d / (n - 1)
  cumvar <- cumsum(var.pc)/sum(var.pc)
  out <- list(var = var.pc, cum_pct = cumvar)
  return(out)
}
