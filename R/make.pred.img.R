#' A function to make prediction images for MEDALS
#'
#' This function allows you to create prediction images from the MEDALS pipeline. This should be the last step
#' @param score.img.list A list of sublists, where each sublist is a list of images created from the PC scores. This can be the output of make.score.img().This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for score.img.list
#' @param fit A model fit object, either use the default or use the output from get.model.fit().
#' @param verbose Print diagnostic messages
#' @keywords MEDALS, Prediction
#' @export
#' @importFrom stats predict
make.pred.img <- function(score.img.list,
                        path.mask.list,
                        fit = medals::def.fit,
                        # subj.id = NULL,
                        verbose = TRUE
){
  # df.list <- vector(mode = "list", length = length(path.mask.list))
  pred.img.list <- vector(mode = "list", length = length(path.mask.list))

  for (i in 1:length(path.mask.list)) {
    if (verbose) {
      message(paste0("Starting subject ",i))
    }
    mask <- check_nifti(path.mask.list[[i]])

    imgs = score.img.list[[i]]
    n_imgs = length(imgs)
    subj.df = matrix(nrow = sum(mask), ncol = n_imgs)

    for (j in seq(n_imgs)) {
      img = check_nifti(imgs[[j]])
      subj.df[, j] = img[ mask == 1 ]
      rm(list = "img"); gc(); gc()
    }
    subj.df <- data.frame(subj.df, stringsAsFactors = FALSE)
    # subj.df$id <- subj.id[i]
    if (verbose) {
      message("Predicting from Model")
    }
    pred <- predict(fit, subj.df, type = "response")
    rm(list = "subj.df"); gc(); gc();
    if (verbose) {
      message("Making prediction into image")
    }
    pred <- remake_img(vec = pred, img = mask, mask = mask)
    pred.img.list[[i]] <- pred
    rm(list = "pred"); gc(); gc();

    # df.list[[i]] <- subj.df
    # rm(list = "subj.df"); gc(); gc();
  }
  # dat = dplyr::bind_rows(df.list)
  # rm(list = "df.list"); gc(); gc();

  # dat<-do.call("rbind", df.list)
  # dat$pred <- predict(fit, dat, type = "response")
  # for(i in 1:length(subj.id)){
  #   vec<-dat$pred[dat$id==subj.id[i]]
  #   mask<- readnii(path.mask.list[[i]])
  #   pred.tmp<-remake_img(vec,mask,mask)
  #   pred.img.list[[i]]<-pred.tmp
  # }
  return(pred.img.list)
}
#' # @param subj.id A vector of subject identifiers.
