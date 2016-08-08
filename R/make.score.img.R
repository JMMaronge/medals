#' A function to create score images for each subject for MEDALS.
#'
#' This function allows you to create the score images for each PC in the MEDALS pipeline. These will be used as predictors in a model to predict lesion areas. This should be the 3rd step in the MEDALS pipeline.
#' @param path.img.list A list of sublists, where each sublist is a list of paths to the files for each imaging modality (ex. T1w, T2w, DWI, etc.) for a particular subject. This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for path.img.list
#' @param cov.mat The crossproduct matrix ($X^TX$) for decomposition to create score images. Should be the output of imaging.cp.mat()
#' @param which.scores A vector of the score images desired for each subject. Ex. 1:8.
#' @export
#' @examples
#' make.score.img()


make.score.img<-function(path.img.list,path.mask.list,cov.mat,which.scores){

  score.imgs<-vector(mode = "list",length = length(path.img.list))
  for(i in 1:length(path.img.list)){
    print(paste0("Starting subject ",i))
    img.mask<-readnii(path.mask.list[[i]])
    ants.mask<-check_ants(img.mask)
    img.list<-vector(mode = "list",length=length(path.img.list[[1]]))
    dat.list<-vector(mode = "list",length=nrow(cov.mat)/27/length(path.img.list[[1]]))
    for(j in 1:length(path.img.list[[1]])){
      img.list[[j]]<-readnii(path.img.list[[i]][[j]])
      ants.img<-check_ants(img.list[[j]])
      for (k in 1:(nrow(cov.mat)/27/length(path.img.list[[1]]))){
        dat.list[[k]][[j]]<-t(neighborhood(img=ants.img,
                                           mask=ants.mask,
                                           radius = rep(1,3),
                                           boundary.condition="mean")[[1]]^k)
      }
    }
    dat.list<-unlist(dat.list, recursive = FALSE)
    x_i<-do.call("cbind",dat.list)
    for(l in which.scores){
      score<-x_i%*%svd(cov.mat)$v[,l]
      img<-remake_img(score,img.mask,img.mask)
      img[img.mask==0]=NA
      score.imgs[[i]][[l]]<-img
    }
  }
  return(score.imgs)
  
}  
