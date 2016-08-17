#' A function to create score images for each subject for MEDALS.
#'
#' This function allows you to create the score images for each PC in the MEDALS pipeline. These will be used as predictors in a model to predict lesion areas. This should be the 3rd step in the MEDALS pipeline.
#' @param path.img.list A list of sublists, where each sublist is a list of paths to the files for each imaging modality (ex. T1w, T2w, DWI, etc.) for a particular subject. This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for path.img.list
#' @param loads The loadings matrix to rotate original data by.
#' @param which.scores A vector of the score images desired for each subject. Ex. 1:8.
#' @export
#' @import fslr
#' @import ANTsR
#' @import extrantsr
#' @examples
#' make.score.img()


make.score.img<-function(path.img.list,path.mask.list,loads=def.loads,which.scores=1:8){

  score.imgs<-vector(mode = "list",length = length(path.img.list))
  for(i in 1:length(path.img.list)){
    print(paste0("Starting subject ",i))
    x_i<-get.img.moment.dat(path.img.list[[i]],path.mask.list[[i]],dim(loads)[1]/27/length(path.img.list[[1]]))
    img.mask<-readnii(path.mask.list[[i]])
    for(l in which.scores){
      score<-x_i%*%loads[,l]
      img<-remake_img(score,img.mask,img.mask)
      img[img.mask==0]=NA
      score.imgs[[i]][[l]]<-img
    }
  }
  return(score.imgs)

}
