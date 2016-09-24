####Function to do streaming PCA on Images


#dir <- "~/PCA_local/images_ISLES2015/"
library(fslr)
library(ANTsR)
library(extrantsr)


ln.pcs.img<-function(path.img.list,path.mask.list,mpower,which.scores){
  require(fslr)
  require(ANTsR)
  require(extrantsr)
  print("Calculating Sufficient Stats")
  stats<-imaging.suff.stat(path.img.list,path.mask.list,mpower)
  print("Calculating X^T%*%X")
  xtx<-imaging.pca.mat(path.img.list,path.mask.list,stats$mean,stats$sd)
  print("Rotating X and creating score images")
  score.list<-make.score.img(path.img.list,path.mask.list,xtx,which.scores)
  return(score.list)
}
imaging.suff.stat<-function(path.img.list,path.mask.list,mpower){
  require(ANTsR)
  require(extrantsr)
  require(fslr)
  mean.mat<-matrix(NA,nrow=27*length(path.img.list[[1]])*mpower,ncol=length(path.img.list))
  n.mat <-matrix(NA,nrow=27*length(path.img.list[[1]])*mpower,ncol=length(path.img.list))
  SOS.mat<-matrix(NA,nrow=27*length(path.img.list[[1]])*mpower,ncol=length(path.img.list))
  for(i in 1:length(path.img.list)){
    print(paste0("Starting subject ",i))
    img.mask<-readnii(path.mask.list[[i]])
    ants.mask<-check_ants(img.mask)
    img.list<-vector(mode = "list",length=length(path.img.list[[1]]))
    dat.list<-vector(mode = "list",length=mpower)
    for(j in 1:length(path.img.list[[1]])){
      img.list[[j]]<-readnii(path.img.list[[i]][[j]])
      ants.img<-check_ants(img.list[[j]])
      for (k in 1:mpower){
        dat.list[[k]][[j]]<-t(neighborhood(img=ants.img,
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
imaging.pca.mat<-function(path.img.list,path.mask.list,mean.vec,sd.vec){
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
make.score.img<-function(path.img.list,path.mask.list,cov.mat,which.scores){
  require(ANTsR)
  require(extrantsr)
  require(fslr)
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


get.var.cot






#testing
path.img<-list(list("~/Desktop/images_ISLES2015/training/flairTrimmedNormImg_Subject05.nii.gz","~/Desktop/images_ISLES2015/training/t1TrimmedNormImg_Subject05.nii.gz"),
               list("~/Desktop/images_ISLES2015/training/flairTrimmedNormImg_Subject06.nii.gz","~/Desktop/images_ISLES2015/training/t1TrimmedNormImg_Subject06.nii.gz"))

path.mask<-list("~/Desktop/images_ISLES2015/training/brainmask_Subject05.nii.gz","~/Desktop/images_ISLES2015/training/brainmask_Subject06.nii.gz")
path.y<-list("~/Desktop/images_ISLES2015/training/ymask_Subject05.nii.gz","~/Desktop/images_ISLES2015/training/ymask_Subject06.nii.gz")
system.time({
test.stat<- imaging.suff.stat(path.img,path.mask)
})

system.time({
  cov.mat<-imaging.make.pca.mat(path.img,path.mask,test.stat$mean,test.stat$sd)
})