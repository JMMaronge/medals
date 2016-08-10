#' A function to fit a logistic regression model for MEDALS
#'
#' This function allows you to get a model fit using the PC scores for MEDALS
#' @param score.img.list A list of sublists, where each sublist is a list of images created from the PC scores. This can be the output of make.score.img().This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for score.img.list
#' @param path.y.list A list of paths to the lesion mask for each subject. This should be in the same order as the sublists for score.img.list
#' @param subj.id A vector of subject identifiers.
#' @keywords MEDALS, model fitting
#' @export
#' @import fslr
#' @import ANTsR
#' @import extrantsr
#' @examples
#' get.model.fit()


get.model.fit<-function(score.img.list,path.mask.list,path.y.list,subj.id){
  df.list <- vector(mode = "list", length =length(path.mask.list))
  
  for(i in 1:length(path.mask.list)){
    mask<- readnii(path.mask.list[[i]])
    y.img<- readnii(path.y.list[[i]])
    dat<-vector(mode = "list", length = length(score.img.list[[1]]))
    for(j in 1:length(score.img.list[[1]])){
      dat[[j]]<-score.img.list[[i]][[j]][mask==1]  
    }
    dat<-do.call("cbind",dat)
    subj.df<- data.frame(dat,stringsAsFactors = FALSE)
    subj.df$y <- y.img[mask==1] 
    subj.df$id <-subj.id[i]
    df.list[[i]] <- subj.df 
  }
  dat<-do.call("rbind", df.list)
  fit<-glm(data=dat[,1:(ncol(dat)-1)],formula=y~.,family="binomial")
  return(fit)
}