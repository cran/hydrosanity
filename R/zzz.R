
.onAttach <- function(libname, pkgname) {
	packageStartupMessage(
	"Hydrosanity: an interface for exploring hydrological time series. ", "\n",
	"Version ", VERSION, ". ", COPYRIGHT, "\n",
	"Type \"hydrosanity()\" to start the graphical user interface.")
}

