% Please edit documentation in R/deparse.R
\name{deparse2}
\alias{deparse2}
\alias{deparse1}
\alias{deparse0}
\title{Deparse Code}
\usage{
deparse2(exp, max.width = getOption("width", 80), force.cut = getOption(
                                                                        "force.cut", 
    FALSE), default.indent = getOption("default.indent", 2), 
    ...)

deparse1(x, ...)

deparse0(x, ...)
}
\arguments{
\item{exp}{A single expression}

\item{max.width}{The maxmium width. Will try to make the length of each line
less than the max.width. It will throw a waring once one line is longer.}

\item{force.cut}{Remove the indent to make the lines(that are longer than
max.width) shorter.}

\item{default.indent}{Number of spaces in the default indent setting}
}
\value{
A string of format R code
}
\description{
\code{\link{deparse2}} only parse a single code block, while
\code{\link{deparse0}} can parse multiple code blocks.
More specificly, given a chunk of code, \code{\link{deparse0}} split it into
pieces(This feature is using the \code{\link{highr:::group_rc}} function). In which each
piece of code is a code block.
Then it calls \code{\link{deparse2}} to format all the pieces.
}
\examples{
x = 'old.priors <- db.query(paste("select prior_id from pfts_priors where pft_id =", parent.pft.id, ";"), con = con)'
deparse2(x)
}
