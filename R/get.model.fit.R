#' A function to fit a logistic regression model for MEDALS
#'
#' This function allows you to get a model fit using the PC scores for MEDALS
#' @param score.img.list A list of sublists, where each sublist is a list of images created from the PC scores. This can be the output of make.score.img().This has to contain at least one sublist
#' @param path.mask.list A list of paths to the brain mask for each subject. This should be in the same order as the sublists for score.img.list
#' @param path.y.list A list of paths to the lesion mask for each subject. This should be in the same order as the sublists for score.img.list
#' @param verbose Print diagnostic messages
#' @keywords MEDALS, model fitting
#' @export
#' @importFrom neurobase check_nifti
#' @importFrom dplyr bind_rows
#' @importFrom stats glm
get.model.fit <- function(score.img.list, path.mask.list, path.y.list,
                          # subj.id = NULL,
                          verbose = TRUE){
  df.list <- vector(mode = "list",
                    length = length(path.mask.list))

  for (i in 1:length(path.mask.list)) {

    if (verbose) {
      message(paste0("Starting subject ",i))
    }
    mask <- check_nifti(path.mask.list[[i]])
    y.img <- check_nifti(path.y.list[[i]])

    imgs = score.img.list[[i]]
    n_imgs = length(imgs)
    subj.df = matrix(nrow = sum(mask), ncol = n_imgs)

    for (j in seq(n_imgs)) {
      img = check_nifti(imgs[[j]])
      subj.df[, j] = img[ mask == 1 ]
      rm(list = "img"); gc(); gc()
    }
    subj.df <- data.frame(subj.df, stringsAsFactors = FALSE)
    subj.df$y <- y.img[ mask == 1 ]
    # subj.df$id <- subj.id[i]
    df.list[[i]] <- subj.df
    rm(list = "subj.df"); gc(); gc();
  }
  dat = dplyr::bind_rows(df.list)
  rm(list = "df.list"); gc(); gc();

  # dat <- do.call("rbind", df.list)
  if (verbose) {
    message("Running GLM")
  }
  # dat$id = NULL
  fit <- glm(formula = y ~ ., data = dat, family = "binomial",
             control = list(trace = verbose))
  fit <- strip.model(fit)
  return(fit)
}
#' #@param subj.id A vector of subject identifiers.
