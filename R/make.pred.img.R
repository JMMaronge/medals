#' A function to make prediction images for MEDALS
#'
#' This function allows you to create prediction images from the MEDALS pipeline. This should be the last step
#' @param score.img.list A list of sublists, where each sublist is a list of images created from the PC scores. This can be the output of make.score.img().This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for score.img.list
#' @param path.y.list A list of paths to the lesion mask for each subject. This should be in the same order as the sublists for score.img.list
#' @param subj.id A vector of subject identifiers.
#' @keywords MEDALS, Prediction
#' @export
#' @examples
#' make.pred.img()


make.pred.img<-function(score.img.list,path.mask.list,path.y.list,subj.id){
require(fslr)
df.list <- vector(mode = "list", length =length(path.mask.list))
  
for(i in 1:length(path.mask.list)){
    mask<- readnii(path.mask.list[[i]])
    y.img<- readnii(path.y.list[[i]])
    dat<-vector(mode = "list", length = length(score.img.list[[1]]))
    for(j in 1:length(score.img.list[[1]])){
      dat[[j]]<-score.img.list[[i]][[j]][mask==1]  
    }
    dat<-do.call("cbind",dat)
    subj.df<- data.frame(dat)
    subj.df$id <-subj.id[i]
    subj.df$y <- y.img[mask==1] 
    df.list[[i]] <- subj.df 
  }
dat<-do.call("rbind", df.list)
fit<-glm(data=dat,formula=y~.-id,family="binomial")
dat$pred<-predict(fit,type="response")
pred.img.list<-vector(mode = "list", length =length(subj.id))
  for(i in 1:length(subj.id)){
    vec<-dat$pred[dat$id==subj.id[i]]
    mask<- readnii(mask.file.path[[i]])
    pred.tmp<-remake_img(vec,mask,mask)
    pred.img.list[[i]]<-pred.tmp
  }
return(pred.img.list)
}