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
    f.mask<-path.mask.list[[i]]
    dat.list<-vector(mode = "list",length=(length(mean.vec)/27/length(path.img.list[[1]])))
    for(j in 1:length(path.img.list[[1]])){
      for(k in 1:(length(mean.vec)/27/length(path.img.list[[1]]))){
        dat.list[[k]][[j]]<-t(neighborhood(img=path.img.list[[i]][[j]],
                                           mask=f.mask,
                                           radius = rep(1,3),
                                           boundary.condition="mean")[[1]]^k)
      }
    }
    dat.list<-unlist(dat.list, recursive = FALSE)
    x_i<-do.call("cbind",dat.list)
    temp.x<-sweep(x_i,2,mean.vec,"-")
    temp.x<-sweep(temp.x,2,sd.vec,"/")
    final.x<-final.x+crossprod(temp.x)
  }
  return(final.x)
}