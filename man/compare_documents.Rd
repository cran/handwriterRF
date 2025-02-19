% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{compare_documents}
\alias{compare_documents}
\title{Compare Documents}
\usage{
compare_documents(
  sample1,
  sample2,
  score_only = TRUE,
  rforest = NULL,
  project_dir = NULL,
  reference_scores = NULL
)
}
\arguments{
\item{sample1}{A filepath to a handwritten document scanned and saved as a
PNG file.}

\item{sample2}{A filepath to a handwritten document scanned and saved as a
PNG file.}

\item{score_only}{TRUE returns only the similarity score. FALSE returns the
similarity score and a score-based likelihood ratio for that score,
calculated using \code{reference_scores}.}

\item{rforest}{Optional. A random forest created with \code{\link[ranger:ranger]{ranger::ranger()}}.
If a random forest is not supplied, \code{random_forest} will be used.}

\item{project_dir}{Optional. A folder in which to save helper files and a CSV
file with the results. If no project directory is supplied. Helper files
will be saved to tempdir() > comparison but deleted before the function
terminates. A CSV file with the results will not be saved, but a dataframe
of the results will be returned.}

\item{reference_scores}{Optional. A list of same writer and different writer
similarity scores used for reference to calculate a score-based likelihood
ratio. If reference scores are not supplied, \code{ref_scores} will be used only
if \code{score_only} is FALSE. If score only is true, reference scores are
unnecessary because a score-based likelihood ratio will not be calculated.
If reference scores are supplied, \code{score_only} will automatically be set to
FALSE.}
}
\value{
A dataframe
}
\description{
Compare two handwritten documents to predict whether they were written by the
same person. Use either a similarity score or a score-based likelihood ratio
as a comparison method.
}
\examples{
\donttest{
# Compare two documents from the same writer with a similarity score
s1 <- system.file(file.path("extdata", "docs", "w0005_s01_pLND_r03.png"),
  package = "handwriterRF"
)
s2 <- system.file(file.path("extdata", "docs", "w0005_s02_pWOZ_r02.png"),
  package = "handwriterRF"
)
compare_documents(s1, s2, score_only = TRUE)

# Compare two documents from the same writer with a score-based
# likelihood ratio
s1 <- system.file(file.path("extdata", "docs", "w0005_s01_pLND_r03.png"),
  package = "handwriterRF"
)
s2 <- system.file(file.path("extdata", "docs", "w0005_s02_pWOZ_r02.png"),
  package = "handwriterRF"
)
compare_documents(s1, s2, score_only = FALSE)
}

}
