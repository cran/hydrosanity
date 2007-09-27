\name{panel.levelplot.interp}
\alias{panel.levelplot.mosaic}
\alias{panel.levelplot.interp}
\alias{panel.contourplot.interp}
\alias{panel.worldmap}
\alias{panel.rivers}
\alias{panel.cities}
\alias{panel.worldmap}
\alias{prepanel.extend.10}
\title{ Panel functions for spatial layers }
\description{
	Functions for plot-time spatial interpolation, which can be used 
	as panel functions for levelplot. Also some generally useful
	spatial layers, assuming a latitude-longitude coordinate system.
}
\usage{
panel.levelplot.mosaic <- function(x, y, z, subscripts = T, 
             at = seq(min(z, na.rm = T), max(z, na.rm = T), length = 100), 
	     col.regions = regions$col)

panel.levelplot.interp(x, y, z, subscripts = T, xo.length = 40, yo.length = xo.length, 
             linear = T, extrap = F, contour = F, region = T, at, ...)

panel.contourplot.interp(..., contour = T, region = F)

panel.worldmap(col = "black", ...)
panel.rivers(col = "blue", lty = "longdash", ...)
panel.cities(pch = 15, col = "black", ...)

prepanel.extend.10(...)
}
\arguments{
  \item{x, y, z}{ x and y coordinates and z value to be interpolated. }
  \item{subscripts}{ used by Lattice for conditioning. }
  \item{at}{ \code{z} values at which to indicate changes. May be omitted, see details below. }
  \item{col.regions}{ a vector of colours representing levels. See \code{\link{levelplot}}. }
  \item{xo.length, yo.length}{ resolution of interpolated surface.
  The plot region is divided into \code{xo.length * yo.length} cells. }
  \item{linear}{ use bicubic rather than linear interpolation. }
  \item{extrap}{ for \code{linear=F}, use spatial extrapolation outside 
  the convex hull of the data. }
  \item{contour}{ draw contour lines (passed to \code{panel.levelplot}. }
  \item{region}{ draw shaded image (passed to \code{panel.levelplot}. }
  \item{\dots}{ further arguments passed to \code{panel.levelplot}. }
  \item{col, lty, pch}{ passed on to the usual drawing functions. }
}
\details{
	For \code{panel.levelplot.interp}, if the \code{at} argument is missing it is taken as \code{pretty(z)} 
	if \code{contour=T} and a 100-point sequence between the ranges of \code{z} otherwise.
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link[lattice]{levelplot}}, \code{\link[tripack]{voronoi.mosaic}}, \code{\link[akima]{interp}}, \code{\link[maps]{map}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
\keyword{aplot}
\keyword{dplot}

