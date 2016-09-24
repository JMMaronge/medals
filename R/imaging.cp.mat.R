#' A function to create cross-product matrix for MEDALS
#'
#' This function allows you to create the cross-product matrix ($X^TX$) for MEDALS. This is the matrix the PCA will be performed on. This is the 2nd step in the MEDALS pipeline.
#' @param mean.vec A vector of column means of $X$. This will be used to center columns before the PCA.
#' @param sd.vec A vector of column standard deviations of $X$. This will be used to scale columns before the PCA.
#' @param n.vec A vector containing the number of voxels in the brain mask for subject
#' @param cp.list A list of subject-specific cross-product matrices
#' @keywords MEDALS, PCA, Segmentation
#' @export
imaging.cp.mat<-function(mean.vec,sd.vec,n.vec,cp.list){
  M = outer(mean.vec,mean.vec)
  S = outer(sd.vec,sd.vec)

  c_i= vector(mode = "list",length=length(cp.list))
  for(i in 1:length(cp.list)){
    c_i[[i]] <- (cp.list[[i]]-n.vec[i]*M)/S
  }
  cp<-do.call("+",c_i)
  return(cp)
  }
