#' A function to calculate sufficient statistics and out cross-product matrix efficiently for medals
#'
#' This function allows you to calculate the sufficient statistics to center and scale each column in the huge (long) matrix X for medals, then outputs the center and scaled cross product matrices. This should be the first step of the MEDALS pipeline
#' @param path.img.list A list of sublists, where each sublist is a list of paths to the files for each imaging modality (ex. T1w, T2w, DWI, etc.) for a particular subject. This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for path.img.list
#' @param mpower A scalar specifying the highest moment wanted for the MEDALS analysis. (Should be at least 1)
#' @param verbose print diagnostic messages
#' @keywords MEDALS, Sufficiency, Segmentation
#' @export


moment.cor.mat<- function(path.img.list,
                              path.mask.list,
                              mpower=4,
                              verbose = TRUE){
  nmod_power = 27*length(path.img.list[[1]])*mpower ### number of columns in X matrix
  mean.vec<-rep(0,nrow=nmod_power) #empty vector of means
  SOS.vec <- mean.vec # assigning 0 vectors
  n.k<-0
  cp.mat<- matrix(0,nrow = nmod_power, ncol = nmod_power) # assigning 0 matrix
  
  
  
  for(i in 1:length(path.img.list)){
    if (verbose) {
      message(paste0("Starting subject ",i))# verbose
    }
    x_i <- get.img.moment.dat(path.img.list[[i]],
                              path.mask.list[[i]],
                              mpower = mpower,
                              verbose = verbose) # get local neighborhood moment data
    col.mean<-colMeans(x_i)
    n<-nrow(x_i)
    mean.vec<-(n.k*mean.vec+n*col.mean)/(n+n.k) # iteratively calculate column means
    n.k<-n.k+n # iteratively calculate n
    sos<-colSums(x_i^2)
    SOS.vec<-SOS.vec+sos # iteratively calculate sums of squares
    cp.mat<-cp.mat+crossprod(x_i) # iteratively calculate sums of cross-product matrices
    rm(list = "x_i","sos","col.mean","n") # empty out memorty
    gc() # R handles RAM funny, good to gc() after deleting
    }
  
  pop.sd <- sqrt((SOS.vec-mean.vec^2*n.k)/(n.k-1)) # population standard deviation
  c.mat<-(cp.mat-n.k*outer(mean.vec,mean.vec))/(outer(pop.sd,pop.sd)) # total center and scaled cross product matrix
  out<-list()
  out$c.mat<-c.mat #output center and scaled cross-product matrix
  out$n<-n.k #output sample size
  return(out)
  }