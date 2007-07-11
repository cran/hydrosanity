\name{read.timeblob}
\alias{read.timeblob}
\title{Read Timeblob from a Text File}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.timeblob(file, skip = 1, sep = ",", sitename = NULL, dataname = "Data", dataCol = 2, qualCol = 3, extraCols = c(), extraNames = paste("Extra", extraCols), readTimesFromFile = T, timeCol = 1, timeFormat = "\%d \%b \%Y", startTime = NA, tz = "GMT", timeSeqBy = "days", ...)
}
\arguments{
  \item{file}{ ~~Describe \code{file} here~~ }
  \item{skip}{ ~~Describe \code{skip} here~~ }
  \item{sep}{ ~~Describe \code{sep} here~~ }
  \item{sitename}{ ~~Describe \code{sitename} here~~ }
  \item{dataname}{ ~~Describe \code{dataname} here~~ }
  \item{dataCol}{ ~~Describe \code{dataCol} here~~ }
  \item{qualCol}{ ~~Describe \code{qualCol} here~~ }
  \item{extraCols}{ ~~Describe \code{extraCols} here~~ }
  \item{extraNames}{ ~~Describe \code{extraNames} here~~ }
  \item{readTimesFromFile}{ ~~Describe \code{readTimesFromFile} here~~ }
  \item{timeCol}{ ~~Describe \code{timeCol} here~~ }
  \item{timeFormat}{ ~~Describe \code{timeFormat} here~~ }
  \item{startTime}{ ~~Describe \code{startTime} here~~ }
  \item{tz}{ ~~Describe \code{tz} here~~ }
  \item{timeSeqBy}{ ~~Describe \code{timeSeqBy} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
  if readTimesFromFile == FALSE then
startTime should be able to be interpreted as a POSIXt, or else be a list like
startTime=list(year=1,month=2,day=3,etc) where these give column numbers to read the start time from the first line in file

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
