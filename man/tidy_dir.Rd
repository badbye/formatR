% Please edit documentation in R/tidy.R
\name{tidy_dir}
\alias{tidy_dir}
\alias{tidy_file}
\title{Format all R scripts under a directory, or specified R scripts}
\usage{
tidy_dir(path = ".", recursive = FALSE, ...)

tidy_file(file, ...)
}
\arguments{
\item{path}{the directory}

\item{recursive}{whether to recursively look for R scripts under \code{path}}

\item{...}{other arguments to be passed to \code{\link{tidy_source}}}

\item{file}{a vector of filenames}
}
\value{
Invisible \code{NULL}.
}
\description{
\code{tidy_dir()} first looks for all the R scripts under a directory (using
the pattern \code{"[.][RrSsQq]$"}), then uses \code{\link{tidy_source}} to
tidy these scripts. The original scripts will be overwritten with reformatted
code if reformatting was successful. You may need to back up the original
directory first if you do not fully understand the tricks used by
\code{\link{tidy_source}}. \code{tidy_file()} formats specified R scripts.
}
\examples{
library(formatR)

path = tempdir()
file.copy(system.file("demo", package = "base"), path, recursive = TRUE)
tidy_dir(path, recursive = TRUE)
}
\seealso{
\code{\link{tidy_source}}
}
\author{
Yihui Xie (\code{tidy_dir}) and Ed Lee (\code{tidy_file})
}
