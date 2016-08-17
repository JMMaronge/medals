## ---- echo=TRUE----------------------------------------------------------
library(medals)
dir<-"~/Desktop/images_ISLES2015/training/"
subj<-c("05","06")

image.file<-vector(length = length(subj), mode = "list")
mask.file<-vector(length = length(subj), mode = "list")
y.file<-vector(length = length(subj), mode = "list")

for(i in 1:length(subj)){
image.file[[i]][[1]]<-paste0(dir,"flairTrimmedNormImg_Subject",subj[i],".nii.gz") 
image.file[[i]][[2]]<-paste0(dir,"t1TrimmedNormImg_Subject",subj[i],".nii.gz") 
image.file[[i]][[3]]<-paste0(dir,"t2TrimmedNormImg_Subject",subj[i],".nii.gz") 
image.file[[i]][[4]]<-paste0(dir,"dwiTrimmedNormImg_Subject",subj[i],".nii.gz") 
mask.file[[i]]<-paste0(dir,"brainmask_Subject",subj[i],".nii.gz")
y.file[[i]]<-paste0(dir,"ymask_Subject",subj[i],".nii.gz")
}

## ----echo=TRUE-----------------------------------------------------------
print(image.file)
print(mask.file)
print(y.file)
print(subj)

## ----echo=TRUE,cache=TRUE------------------------------------------------
#### library(devtools) ### for install_github()
#### install_github("jmmaronge/medals") ### installing medals
library(medals)
suff<-imaging.suff.stat(path.img.list = image.file,
                        path.mask.list = mask.file,
                        mpower = 2 #### denotes highest order moment wanted
    )

## ----echo=TRUE,cache=TRUE------------------------------------------------
cp<-imaging.cp.mat(mean.vec = suff$mean,
                   sd.vec = suff$sd,
                   n.vec = suff$n,
                   cp.list = suff$cp.mats
  )
dim(cp)

## ---- echo=TRUE----------------------------------------------------------
pc.var(cp,suff$total.n)[[1]][1:10]
pc.var(cp,suff$total.n)[[2]][1:10]

## ----echo=TRUE,cache=TRUE------------------------------------------------
load<-get.loadings(cp)
scores<-make.score.img(path.img.list = image.file,
                       path.mask.list = mask.file,
                       loads = load,
                       which.scores = 1:8
)

## ----echo=TRUE-----------------------------------------------------------
#install.packages("fslr")
library(fslr)
ortho2(scores[[1]][[2]]) # gives you the score image of the 1st subject, 2nd PC

## ----echo=TRUE,cache=TRUE------------------------------------------------
fit1<-get.model.fit(score.img.list = scores,
                   path.mask.list = mask.file,
                   path.y.list = y.file,
                   subj.id = subj
  
)
preds<-make.pred.img(score.img.list = scores,
                     path.mask.list = mask.file,
                     fit=fit1,
                     subj.id = subj
  
)
ortho2(preds[[1]]) #prediction image for the first subject

## ----echo=TRUE-----------------------------------------------------------
test.subj<-c("08")
test.image.file<-vector(length = length(test.subj), mode = "list")
test.mask.file<-vector(length = length(test.subj), mode = "list")

for(i in 1:length(test.subj)){

test.image.file[[i]][[1]]<-paste0(dir,"flairTrimmedNormImg_Subject",test.subj[i],".nii.gz") 
test.image.file[[i]][[2]]<-paste0(dir,"t1TrimmedNormImg_Subject",test.subj[i],".nii.gz") 
test.image.file[[i]][[3]]<-paste0(dir,"t2TrimmedNormImg_Subject",test.subj[i],".nii.gz") 
test.image.file[[i]][[4]]<-paste0(dir,"dwiTrimmedNormImg_Subject",test.subj[i],".nii.gz") 
test.mask.file[[i]]<-paste0(dir,"brainmask_Subject",test.subj[i],".nii.gz")

}

## ----echo=TRUE-----------------------------------------------------------
print(test.image.file)
print(test.mask.file)
print(test.subj)

## ----echo=TRUE,cache=TRUE------------------------------------------------
test.scores<-make.score.img(path.img.list = test.image.file,
                            path.mask.list = test.mask.file,
                            loads = load, #training loadings matrix
                            which.scores = 1:8 #make sure this is the same as what your training set used
)

## ----echo=TRUE,cache=TRUE------------------------------------------------
test.preds<-make.pred.img(score.img.list = test.scores,
                          path.mask.list = test.mask.file,
                          fit=fit1, #training fit
                          subj.id = test.subj
)
ortho2(test.preds[[1]])

