## ---- echo=TRUE----------------------------------------------------------
library(medals)
datadir <- "~/Desktop/images_ISLES2015/training/"
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

## ----echo=TRUE,cache=TRUE------------------------------------------------
#### library(devtools) ### for install_github()
#### install_github("jmmaronge/medals") ### installing medals
library(medals)
suff <- imaging.suff.stat(
  path.img.list = image.file,
  path.mask.list = mask.file,
  mpower = 2 #### denotes highest order moment wanted
)

## ----echo=TRUE,cache=TRUE------------------------------------------------
cp <- imaging.cp.mat(mean.vec = suff$mean,
                     sd.vec = suff$sd,
                     n.vec = suff$n,
                     cp.list = suff$cp.mats
)
dim(cp)

## ---- echo=TRUE----------------------------------------------------------
pc.var(cp,suff$total.n)$var[1:8]
pc.var(cp,suff$total.n)$cum_pct[1:8]

## ----echo=TRUE,cache=TRUE------------------------------------------------
loadings <- get.loadings(cp)
scores <- make.score.img(
  path.img.list = image.file,
  path.mask.list = mask.file,
  loads = loadings,
  which.scores = 1:8
)

## ----echo=TRUE-----------------------------------------------------------
#install.packages("fslr")
library(fslr)
ortho2(scores[[1]][[2]]) # gives you the score image of the 1st subject, 2nd PC

## ----echo=TRUE,cache=TRUE------------------------------------------------
fit1 <- get.model.fit(
  score.img.list = scores,
  path.mask.list = mask.file,
  path.y.list = y.file,
  # subj.id = subj,
  verbose = TRUE
)
preds <- make.pred.img(
  score.img.list = scores,
  path.mask.list = mask.file,
  fit = fit1,
  # subj.id = subj
  verbose = TRUE
)
ortho2(preds[[1]]) #prediction image for the first subject
ortho2(image.file[[1]][[1]], preds[[1]] > 0.5) #prediction image for the first subject

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

## ----echo=TRUE,cache=TRUE------------------------------------------------
test.scores <-
  scores <- make.score.img(
  path.img.list = test.image.file,
  path.mask.list = test.mask.file,
  loads = loadings,  #training loadings matrix
  which.scores = 1:8  #make sure this is the same as what your training set used
)

## ----echo=TRUE,cache=TRUE------------------------------------------------
test.preds <- preds <- make.pred.img(
  score.img.list = test.scores,
  path.mask.list = test.mask.file,
  fit = fit1, # training fit
  # subj.id = subj
  verbose = TRUE
)
mask = readnii(test.mask.file[[1]])
img = readnii(test.image.file[[1]][[1]]) 
# ortho2(test.preds[[1]]) #prediction image for the first subject
ortho2(img, test.preds[[1]] > 0.5) #binary prediction

# comparison to ground truth
y_img = readnii(test.y.file[[1]]) 
double_ortho(x = y_img, y = test.preds[[1]] > 0.5, 
             NA.x = TRUE, 
             NA.y = TRUE, 
             xyz = xyz(y_img),
             col = "white",
             col.y = "white") #binary prediction

ortho_diff(img, 
           pred = test.preds[[1]] > 0.5, 
          roi = y_img) 

