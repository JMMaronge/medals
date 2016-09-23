#' A function to calculate sufficient statistics for MEDALS
#'
#' This function allows you to calculate the sufficient statistics to center and scale each column in the huge matrix X for MEDALS. This should be the first step of the MEDALS pipeline
#' @param path.img.list A list of sublists, where each sublist is a list of paths to the files for each imaging modality (ex. T1w, T2w, DWI, etc.) for a particular subject. This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for path.img.list
#' @param mpower A scalar specifying the highest moment wanted for the MEDALS analysis. (Should be at least 1)
#' @param verbose print diagnostic messages
#' @keywords MEDALS, Sufficiency, Segmentation
#' @export
imaging.suff.stat <- function(path.img.list,
                              path.mask.list,
                              mpower=4,
                              verbose = TRUE){
  nmod_power = 27*length(path.img.list[[1]])*mpower
  mean.mat<-matrix(NA,nrow=nmod_power,ncol=length(path.img.list))
  SOS.mat <- n.mat <- mean.mat
  cp.mats<- vector(mode = "list",length = length(path.img.list))


  for(i in 1:length(path.img.list)){
    if (verbose) {
      message(paste0("Starting subject ",i))
    }
    x_i <- get.img.moment.dat(path.img.list[[i]],
                              path.mask.list[[i]],
                              mpower = mpower,
                              verbose = verbose)
    # dat.list<-unlist(dat.list, recursive = FALSE)
    # x_i<-do.call("cbind",dat.list)
    # rm(list = "dat.list"); gc();
    mean.mat[,i]<-colMeans(x_i)
    n.mat[,i]<-rep(nrow(x_i), nrow(n.mat))
    SOS.mat[,i]<-colSums(x_i^2)
    cp.mats[[i]]<-crossprod(x_i)
    rm(list = "x_i");
    gc() # R handles RAM funny, good to gc() after deleting
  }
  pop.mean <- rowSums(mean.mat*n.mat)/rowSums(n.mat)
  pop.sd <- sqrt((rowSums(SOS.mat)-(rowSums(mean.mat*n.mat)/rowSums(n.mat))^2*rowSums(n.mat))/(rowSums(n.mat)-1))
  # pop.stat<-vector(mode = "list", length = 4)
  pop.stat = list()
  pop.stat$mean<-pop.mean
  pop.stat$sd<-pop.sd
  pop.stat$n<-n.mat[1,]
  pop.stat$total.n<-rowSums(n.mat)[1]
  pop.stat$cp.mats<-cp.mats
  return(pop.stat)
}
