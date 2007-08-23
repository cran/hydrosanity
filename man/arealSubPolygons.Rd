\name{arealSubPolygons}
\alias{arealSubPolygons}
\title{ Compute sub-regions closest to each point }
\description{
  Compute the Voronoi mosaic of a set of points, and intersect it with a given
  polygon. This gives the sub-regions of the polygon for which each point is the
  closest.
}
\usage{
arealSubPolygons(x, y = NULL, IDs = row.names(x), boundary, min.area.pct = 0.5)
}
\arguments{
  \item{x}{ x coordinate of points, or a list containing \code{x} and \code{y}. }
  \item{y}{ y coordinate of points. }
  \item{IDs}{ identification strings for each point. }
  \item{boundary}{ polygon coordinates (coerced to "gpc.poly" class). }
  \item{min.area.pct}{ minimum percentage area of the polygon to be allocated to any one site.
	  Sites which would have less area than this will be excluded. }
}
\details{
  Voronoi mosaic, AKA Thiessen polygons, AKA Dirichlet tesselation.
}
\value{
	An object of class \code{SpatialPolygons} with ID slots from the original \code{IDs} argument.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link[tripack]{voronoi.mosaic}}, \code{\link[gpclib]{gpc.poly-class}}, \code{\link[sp]{SpatialPolygons}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
\keyword{ spatial }

