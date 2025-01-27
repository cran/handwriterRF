## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(handwriter)
library(handwriterRF)

## ----profiles, eval=FALSE-----------------------------------------------------
#  profiles <- handwriter::get_writer_profiles(
#    input_dir = "path/to/training/samples/folder",
#    measure = "rates",
#    num_cores = 1,
#    template = handwriter::templateK40,
#    output_dir = "path/to/output/folder"
#  )

## ----single-rf, eval=FALSE----------------------------------------------------
#  rf <- train_rf(
#    df = profiles,
#    ntrees = 200,
#    distance_measures = c("abs", "man", "euc", "max", "cos"),
#    output_dir = "path/to/output/folder",
#    downsample_diff_pairs = TRUE
#  )

## ----multiple-rfs, eval=FALSE-------------------------------------------------
#  for (i in 1:10) {
#    rf <- train_rf(
#      df = profiles,
#      ntrees = 200,
#      distance_measures = c("abs", "man"),
#      output_dir = "path/to/output/folder",
#      run_number = i,
#      downsample_diff_pairs = TRUE
#    )
#  }
#  

## ----ref-profiles, eval=FALSE-------------------------------------------------
#  ref_profiles <- handwriter::get_writer_profiles(
#    input_dir = "path/to/ref/samples/folder",
#    measure = "rates",
#    num_cores = 1,
#    template = handwriter::templateK40,
#    output_dir = "path/to/output/folder"
#  )
#  
#  rscores <- get_ref_scores(rforest = rf,
#                            df = ref_profiles)

## ----plot, out.width="75%", dpi=300-------------------------------------------
plot_scores(scores = ref_scores)

## ----plot-obs, out.width="75%", dpi=300---------------------------------------
plot_scores(scores = ref_scores,
            obs_score = 0.2)

## ----plot-own, eval=FALSE-----------------------------------------------------
#  plot_scores(scores = rscores,
#              obs_score = 0.2)

## ----compare, message=FALSE---------------------------------------------------
sample1 <- system.file("extdata", "docs", "w0238_s01_pWOZ_r02.png", package = "handwriterRF")
sample2 <- system.file("extdata", "docs", "w0238_s01_pWOZ_r03.png", package = "handwriterRF")

df <- compare_documents(
  sample1, 
  sample2, 
  score_only = FALSE,
  project_dir = "~/Desktop/test"
)
df

## -----------------------------------------------------------------------------
plot_scores(scores = ref_scores, obs_score = df$score)

## ----new-compare, eval=FALSE--------------------------------------------------
#  df_new <- compare_documents(
#    sample1,
#    sample2,
#    score_only = FALSE,
#    rforest = rf,
#    reference_scores = rscores
#  )
#  df_new
#  
#  plot_scores(scores = rscores, obs_score = df_new$score)

