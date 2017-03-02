% Please edit documentation in R/deparse.R
\name{find_cutoff}
\alias{find_cutoff}
\title{Find the possible positions for cutoff}
\usage{
find_cutoff(chars, default.indent = 2)
}
\description{
The positions to cutoff could be after comma or left bracket.
The pipe operator `%>%` is also supported.
Should use the function: sourcetools::tokenize_string
}
