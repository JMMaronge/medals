#' A function to calculate the matrix of neighborhood moments for a subject MEDALS
#'
#' This function allows you to calculate the matrix of neighborhood moments for MEDALS. 
#' @param imgs.path A list of paths to images for one subject.
#' @param path.mask.list A path to the braiin mask for each subject.
#' @param mpower A scalar specifying the highest moment wanted for the MEDALS analysis.
#' @keywords MEDALS, Sufficiency, Segmentation
#' @export
#' @import fslr
#' @import ANTsR
#' @import extrantsr
#' @examples
#' get.img.moment.dat()

get.img.moment.dat<-function(imgs.path,mask.path,mpower=4){
  nmod.power = 27*length(imgs.path)*mpower
    f.mask<-mask.path
    mask = antsImageRead(f.mask)
    n = sum(mask)
    rm(list="mask"); gc();
    x_i = matrix(nrow = n, ncol = nmod.power)
    ind = 0
    for(j in 1:length(imgs.path)){
      vals<-t(neighborhood(img=imgs.path[[j]],
                           mask=f.mask,
                           radius = rep(1,3),
                           boundary.condition="mean")$values)
      gc();
      for (k in 1:mpower){
        x_i[, seq(ind + 1, ind + 27)] = vals^k
        print(k)
        ind = ind + 27
        # dat.list[[k]][[j]]<-vals^k
      }
      rm(list= "vals");
    return(x_i)
    }
}    