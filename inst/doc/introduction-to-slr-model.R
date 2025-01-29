## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5
)

## ----profiles-facet-----------------------------------------------------------
library(dplyr)
library(handwriter)
library(handwriterRF)

wps <- train %>% dplyr::filter(writer == "w0003" | writer == "w0005")

handwriter::plot_writer_profiles(wps, color_by = "writer", facets = "writer")

## ----profiles-combined--------------------------------------------------------
plot_writer_profiles(wps, color_by = "writer")

## ----ref-scores---------------------------------------------------------------
plot_scores(ref_scores)

## ----example-profiles---------------------------------------------------------
test_samples <- test[test$writer == "w0002",][1:2,]

handwriter::plot_writer_profiles(test_samples)

## ----example-score------------------------------------------------------------
score <- compare_writer_profiles(test_samples)
score

## ----plot-score---------------------------------------------------------------
plot_scores(ref_scores, obs_score = score$score)

## ----slr----------------------------------------------------------------------
slr <- compare_writer_profiles(test_samples, score_only = FALSE)
slr

