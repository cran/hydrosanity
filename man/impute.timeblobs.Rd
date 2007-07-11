\name{impute.timeblobs}
\alias{impute.timeblobs}
\alias{imputeGaps.timeblobs}
\alias{quick.disaccumulate.timeblob}
\alias{unimputeGaps.timeblobs}
\title{Fill Gaps in Time Series}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
impute.timeblobs(blob.list, which.impute = names(blob.list), timelim = NULL, extend = F, withinTimeframe = NA, method = c("distance", "correlation", "constant"), constant = c("mean", "zero", "extend"), trim = 0)

imputeGaps.timeblobs(blob.list, which.impute = names(blob.list), type = c("disaccumulated", "imputed"), fallBackToConstantDisaccum = T, maxGapLength = Inf, internalGapsOnly = F, ...)

quick.disaccumulate.timeblob(blob)

unimputeGaps.timeblobs(blob.list, timelim = NULL, type = c("imputed", "disaccumulated"))
}
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{which.impute}{ ~~Describe \code{which.impute} here~~ }
  \item{timelim}{ ~~Describe \code{timelim} here~~ }
  \item{extend}{ ~~Describe \code{extend} here~~ }
  \item{withinTimeframe}{ ~~Describe \code{withinTimeframe} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{constant}{ ~~Describe \code{constant} here~~ }
  \item{trim}{ ~~Describe \code{trim} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
  \item{fallBackToConstantDisaccum}{ ~~Describe \code{fallBackToConstantDisaccum} here~~ }
  \item{maxGapLength}{ ~~Describe \code{maxGapLength} here~~ }
  \item{internalGapsOnly}{ ~~Describe \code{internalGapsOnly} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
  \item{blob}{ ~~Describe \code{blob} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
	
}
\keyword{ ts }
