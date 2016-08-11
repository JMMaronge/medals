#' A function to create cross-product matrix for MEDALS
#'
#' This function allows you to create the cross-product matrix ($X^TX$) for MEDALS. This is the matrix the PCA will be performed on. This is the 2nd step in the MEDALS pipeline.
#' @param path.img.list A list of sublists, where each sublist is a list of paths to the files for each imaging modality (ex. T1w, T2w, DWI, etc.) for a particular subject. This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for path.img.list
#' @param mean.vec A vector of column means of $X$. This will be used to center columns before the PCA.
#' @param sd.vec A vector of column standard deviations of $X$. This will be used to scale columns before the PCA.
#' @keywords MEDALS, PCA, Segmentation
#' @export
#' @import fslr
#' @import ANTsR
#' @import extrantsr
#' @examples
#' imaging.cp()

imaging.cp.mat<-function(path.img.list,path.mask.list,mean.vec,sd.vec){
  final.x<-matrix(0,nrow = length(mean.vec),ncol = length(mean.vec))
  for(i in 1:length(path.img.list)){
    print(paste0("Starting subject ",i))
    x_i<-get.img.moment.dat(path.img.list[[i]],path.mask.list[[i]],length(mean.vec)/27/length(path.img.list[[1]]))
    print("centering")
    x_i<-sweep(x_i,2,mean.vec,"-")
    print("scaling")
    x_i<-sweep(x_i,2,sd.vec,"/")
    print("calculating x^tx")
    x_i<-crossprod(x_i)
    final.x<-final.x+x_i
    rm(list = "x_i");
    gc() # R handles RAM funny, good to gc() after deleting
  }
  return(final.x)
}