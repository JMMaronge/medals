#' A function to create the loadings matrix for  MEDALS.
#'
#' This function allows you to create loadings matrix for PC scores in the MEDALS pipeline. T
#' @param cov.mat The crossproduct matrix ($X^TX$) for decomposition to get loadings. Should be the output of imaging.cp.mat()
#' @export
#' @import fslr
#' @import ANTsR
#' @import extrantsr
#' @examples
#' get.loadings()
 

get.loadings<-function(cov.mat){
  loadings<-svd(cov.mat)$v
  return(loadings)
}
