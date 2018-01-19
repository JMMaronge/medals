## ---- echo=TRUE----------------------------------------------------------
library(medals)
datadir <- "~/Desktop/ISLES_2015/all_testing"
subj <- c("05","06")

image.file<-vector(length = length(subj), mode = "list")
mask.file<-vector(length = length(subj), mode = "list")
y.file<-vector(length = length(subj), mode = "list")

for(i in 1:length(subj)){
  imgs = file.path(datadir,
                   paste0(c("flair", "t1", "t2", "dwi"),
                          "TrimmedNormImg_Subject",
                          subj[i], ".nii.gz"))
  image.file[[i]][[1]] <- imgs[1]
  image.file[[i]][[2]] <- imgs[2]
  image.file[[i]][[3]] <- imgs[3]
  image.file[[i]][[4]] <- imgs[4]
  mask.file[[i]] <- file.path(datadir, paste0("brainmask_Subject", subj[i], ".nii.gz"))
  y.file[[i]] <- file.path(datadir, 
                           paste0("ymask_Subject", subj[i], ".nii.gz")
  )
}

## ----echo=TRUE-----------------------------------------------------------
print(image.file)
print(mask.file)
print(y.file)
print(subj)

## ------------------------------------------------------------------------
#install.packages("fslr")
library(neurobase)
ortho2(readnii(image.file[[1]][[1]]))

## ---- echo=TRUE----------------------------------------------------------
pc.var(mat=cp$c.mat,n=cp$n)$var[1:8]
pc.var(mat=cp$c.mat,n=cp$n)$cuml_pct[1:8]

## ----echo=TRUE-----------------------------------------------------------
ortho2(scores[[1]][[2]]) # gives you the score image of the 1st subject, 2nd PC

## ----echo=TRUE-----------------------------------------------------------
test.subj<-c("08")
test.image.file<-vector(length = length(test.subj), mode = "list")
test.mask.file<-vector(length = length(test.subj), mode = "list")
test.y.file<-vector(length = length(test.subj), mode = "list")

for(i in 1:length(test.subj)){
  imgs = file.path(datadir,
                   paste0(c("flair", "t1", "t2", "dwi"),
                          "TrimmedNormImg_Subject",
                          test.subj[i], ".nii.gz"))
  test.image.file[[i]][[1]] <- imgs[1]
  test.image.file[[i]][[2]] <- imgs[2]
  test.image.file[[i]][[3]] <- imgs[3]
  test.image.file[[i]][[4]] <- imgs[4]
  test.mask.file[[i]] <- file.path(datadir, 
                                   paste0("brainmask_Subject", test.subj[i], ".nii.gz"))
  test.y.file[[i]] <- file.path(datadir, 
                           paste0("ymask_Subject", test.subj[i], ".nii.gz")
  )
}

## ----echo=TRUE-----------------------------------------------------------
print(test.image.file)
print(test.mask.file)
print(test.subj)

