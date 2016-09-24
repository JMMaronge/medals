#' A function to calculate the matrix of neighborhood moments for a subject MEDALS
#'
#' This function allows you to calculate the matrix of neighborhood moments for MEDALS.
#' @param imgs.path A list of paths to images for one subject.
#' @param mask.path A path to the braiin mask for each subject.
#' @param mpower A scalar specifying the highest moment wanted for the MEDALS analysis.
#' @param verbose print diagnostic messages
#' @keywords MEDALS, Sufficiency, Segmentation
#' @export
#' @importFrom extrantsr check_ants neighborhood
#'
 get.img.moment.dat <- function(imgs.path,
                               mask.path,
                               mpower = 4,
                               verbose = TRUE){
  nmod.power = 27 * length(imgs.path) * mpower
  mask = check_ants(mask.path)
  n = sum(mask)
  x_i = matrix(nrow = n, ncol = nmod.power)
  ind = 0
  for (j in 1:length(imgs.path)) {
    vals <- t(neighborhood(img = imgs.path[[j]],
                           mask = mask,
                           radius = rep(1,3),
                           boundary.condition = "mean",
                           verbose = verbose,
                           get.gradient = FALSE)$values
    )
    for (k in 1:mpower) {
      x_i[, seq(ind + 1, ind + 27)] = vals^k
      ind = ind + 27
    }
    rm(list = "vals"); gc();
    eval(gc(), parent.frame())
  }
  rm(list = "mask"); gc();
  eval(gc(),parent.frame())
  return(x_i)

}
