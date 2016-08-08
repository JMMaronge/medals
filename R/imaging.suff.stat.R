#' A function to calculate sufficient statistics for MEDALS
#'
#' This function allows you to calculate the sufficient statistics to center and scale each column in the huge matrix X for MEDALS. This should be the first step of the MEDALS pipeline
#' @param path.img.list A list of sublists, where each sublist is a list of paths to the files for each imaging modality (ex. T1w, T2w, DWI, etc.) for a particular subject. This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for path.img.list
#' @param mpower A scalar specifying the highest moment wanted for the MEDALS analysis. (Should be at least 1)
#' @keywords MEDALS, Sufficiency, Segmentation
#' @export
#' @examples
#' imaging.suff.stat()

imaging.suff.stat<-function(path.img.list,path.mask.list,mpower){
  mean.mat<-matrix(NA,nrow=27*length(path.img.list[[1]])*mpower,ncol=length(path.img.list))
  n.mat <-matrix(NA,nrow=27*length(path.img.list[[1]])*mpower,ncol=length(path.img.list))
  SOS.mat<-matrix(NA,nrow=27*length(path.img.list[[1]])*mpower,ncol=length(path.img.list))
  for(i in 1:length(path.img.list)){
    print(paste0("Starting subject ",i))
    img.mask<-fslr::readnii(path.mask.list[[i]])
    ants.mask<-extrantsr::check_ants(img.mask)
    img.list<-vector(mode = "list",length=length(path.img.list[[1]]))
    dat.list<-vector(mode = "list",length=mpower)
    for(j in 1:length(path.img.list[[1]])){
      img.list[[j]]<-readnii(path.img.list[[i]][[j]])
      ants.img<-extrantsr::check_ants(img.list[[j]])
      for (k in 1:mpower){
          dat.list[[k]][[j]]<-t(extrantsr::neighborhood(img=ants.img,
                                           mask=ants.mask,
                                           radius = rep(1,3),
                                           boundary.condition="mean")[[1]]^k)
      }
    }
    dat.list<-unlist(dat.list, recursive = FALSE)
    x_i<-do.call("cbind",dat.list)
    mean.mat[,i]<-colMeans(x_i)
    n.mat[,i]<-rep(nrow(x_i), nrow(n.mat))
    SOS.mat[,i]<-colSums(x_i^2)
  }
  pop.mean<-rowSums(mean.mat*n.mat)/rowSums(n.mat)
  pop.sd<-sqrt((rowSums(SOS.mat)-(rowSums(mean.mat*n.mat)/rowSums(n.mat))^2*rowSums(n.mat))/(rowSums(n.mat)-1))
  pop.stat<-list(length=2)
  pop.stat$mean<-pop.mean
  pop.stat$sd<-pop.sd
  pop.stat$total.n<-rowSums(n.mat)[1]
  return(pop.stat)
}