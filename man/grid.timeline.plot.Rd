\name{grid.timeline.plot}
\alias{grid.timeline.plot}
\alias{grid.timeline.bar}
\alias{timelineColMapDefault}
\title{Plot Timeline of Data and its Quality}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grid.timeline.plot(blob.list, xscale = NULL, colMap = NULL, barThickness = unit(1.2,"lines"), auto.key = T, maxLabelChars = 20, pad = unit(1,"lines"), grill = T, main = NULL, sub = T, newpage = T)

grid.timeline.bar <- function(blob, colMap = NULL, name = "timeline.bar", vp = NULL)

timelineColMapDefault(colMap = list(good="black", suspect=trellis.par.get("superpose.polygon")$col[1], poor=trellis.par.get("superpose.polygon")$col[2], disaccumulated=trellis.par.get("superpose.polygon")$col[3], imputed=trellis.par.get("superpose.polygon")$col[4]))
}
\arguments{
  \item{blob.list}{ ~~Describe \code{blob.list} here~~ }
  \item{xscale}{ ~~Describe \code{xscale} here~~ }
  \item{colMap}{ ~~Describe \code{colMap} here~~ }
  \item{barThickness}{ ~~Describe \code{thickness} here~~ }
  \item{auto.key}{ ~~Describe \code{auto.key} here~~ }
  \item{maxLabelChars}{ ~~Describe \code{maxLabelChars} here~~ }
  \item{pad}{ ~~Describe \code{pad} here~~ }
  \item{grill}{ ~~Describe \code{grill} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{sub}{ ~~Describe \code{sub} here~~ }
  \item{newpage}{ ~~Describe \code{newpage} here~~ }
  \item{blob}{ ~~Describe \code{blob} here~~ }
  \item{name}{ ~~Describe \code{name} here~~ }
  \item{vp}{ ~~Describe \code{vp} here~~ }
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
