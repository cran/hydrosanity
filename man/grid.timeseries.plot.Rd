\name{grid.timeseries.plot}
\alias{grid.timeseries.plot}
\alias{grid.timeseries.plot.superpose}
\alias{grid.timeseries.steps}
\title{Plot Time Series}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grid.timeseries.plot(blob.list, xlim = NULL, ylim = NULL, sameScales = T, 
                     logScale = F, qualTimeline = F, colMap = NULL, 
		     barThickness = unit(0.5,"lines"), auto.key = T, maxLabelChars = 20, 
		     pad = unit(1,"lines"), between = unit(0,"lines"), superPos = 1, 
		     newScale = T, main = NULL, sub = T, newpage = (superPos==1), 
		     nSuperpose = 1, gp=gpar(col=rep(trellis.par.get("superpose.line")$col, len=superPos)[superPos], 
		       alpha=rep(trellis.par.get("superpose.line")$alpha, len=superPos)[superPos],
		       lty=rep(trellis.par.get("superpose.line")$lty, len=superPos)[superPos],
		       lwd=rep(trellis.par.get("superpose.line")$lwd, len=superPos)[superPos]))

grid.timeseries.plot.superpose(superpose.blob.list, allSameScales = F, xlim = NULL, ylim = NULL, logScale = F, sub = T, ...)

grid.timeseries.steps(blob, logScale = F, name = "timeseries", gp = NULL, vp = NULL)
}
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{sameScales}{ ~~Describe \code{sameScales} here~~ }
  \item{logScale}{ ~~Describe \code{logScale} here~~ }
  \item{qualTimeline}{ ~~Describe \code{qualTimeline} here~~ }
  \item{colMap}{ ~~Describe \code{colMap} here~~ }
  \item{barThickness}{ ~~Describe \code{thickness} here~~ }
  \item{auto.key}{ ~~Describe \code{auto.key} here~~ }
  \item{maxLabelChars}{ ~~Describe \code{maxLabelChars} here~~ }
  \item{pad}{ ~~Describe \code{pad} here~~ }
  \item{between}{ ~~Describe \code{between} here~~ }
  \item{superPos}{ ~~Describe \code{superPos} here~~ }
  \item{newScale}{ ~~Describe \code{newScale} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{sub}{ ~~Describe \code{sub} here~~ }
  \item{newpage}{ ~~Describe \code{newpage} here~~ }
  \item{nSuperpose}{ ~~Describe \code{nSuperpose} here~~ }
  \item{gp}{ ~~Describe \code{gp} here~~ }
  \item{superpose.blob.list}{ ~~Describe \code{superpose.blob.list} here~~ }
  \item{allSameScales}{ ~~Describe \code{allSameScales} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{name}{ ~~Describe \code{blob} here~~ }
  \item{vp}{ ~~Describe \code{blob} here~~ }
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
