 
imaging.cp.mat<-function(path.img.list,path.mask.list,mean.vec,sd.vec){
  require(ANTsR)
  require(extrantsr)
  require(fslr)
  final.x<-matrix(0,nrow = length(mean.vec),ncol = length(mean.vec))
  for(i in 1:length(path.img.list)){
    print(paste0("Starting subject ",i))
    img.mask<-readnii(path.mask.list[[i]])
    ants.mask<-check_ants(img.mask)
    img.list<-vector(mode = "list",length=length(path.img.list[[1]]))
    dat.list<-vector(mode = "list",length=(length(mean.vec)/27/length(path.img.list[[1]])))
    for(j in 1:length(path.img.list[[1]])){
      img.list[[j]]<-readnii(path.img.list[[i]][[j]])
      ants.img<-check_ants(img.list[[j]])
      for(k in 1:(length(mean.vec)/27/length(path.img.list[[1]]))){
        dat.list[[k]][[j]]<-t(neighborhood(img=ants.img,
                                           mask=ants.mask,
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