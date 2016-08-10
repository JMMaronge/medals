#' A function to make prediction images for MEDALS
#'
#' This function allows you to create prediction images from the MEDALS pipeline. This should be the last step
#' @param score.img.list A list of sublists, where each sublist is a list of images created from the PC scores. This can be the output of make.score.img().This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for score.img.list
#' @param fit A model fit object, either use the default or use the output from get.model.fit().
#' @param subj.id A vector of subject identifiers.
#' @keywords MEDALS, Prediction
#' @export
#' @import fslr
#' @import ANTsR
#' @import extrantsr
#' @examples
#' make.pred.img()


make.pred.img<-function(score.img.list,path.mask.list,fit=def.fit,subj.id){
df.list <- vector(mode = "list", length =length(path.mask.list))
  
for(i in 1:length(path.mask.list)){
    mask<- readnii(path.mask.list[[i]])
    dat<-vector(mode = "list", length = length(score.img.list[[1]]))
    for(j in 1:length(score.img.list[[1]])){
      dat[[j]]<-score.img.list[[i]][[j]][mask==1]  
    }
    dat<-do.call("cbind",dat)
    subj.df<- data.frame(dat,stringsAsFactors = FALSE)
    subj.df$id <-subj.id[i]
    df.list[[i]] <- subj.df 
  }
dat<-do.call("rbind", df.list)
dat$pred<-predict(fit,dat,type="response")
pred.img.list<-vector(mode = "list", length =length(subj.id))
  for(i in 1:length(subj.id)){
    vec<-dat$pred[dat$id==subj.id[i]]
    mask<- readnii(path.mask.list[[i]])
    pred.tmp<-remake_img(vec,mask,mask)
    pred.img.list[[i]]<-pred.tmp
  }
return(pred.img.list)
}