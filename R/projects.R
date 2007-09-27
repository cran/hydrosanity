## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

.hs_on_menu_new_activate <- function(action, window) hydrosanity()
.hs_on_menu_open_activate <- function(action, window) openProject()
.hs_on_menu_save_activate <- function(action, window) saveProject()
.hs_on_menu_saveas_activate <- function(action, window) saveProject(saveAs=T)

openProject <- function(filename=NULL) {
	freezeGUI()
	on.exit(thawGUI())
	
	if (is.null(filename)) {
		ff <- c("Hydrosanity projects (.hydrosanity)", "*.hydrosanity")
		filename <- choose.files(caption="Open project", filters=ff, multi=F)
		StateEnv$win$present()
		if (filename=="") return()
		# reset interface
		hydrosanity()
		StateEnv$win$setSensitive(F)
	}
	
	load(filename, .GlobalEnv)
	hsp$modified <<- F
	hsp$projectFile <<- filename
	StateEnv$win['title'] <- paste("Hydrosanity:", get.stem(filename))
	
	if (!is.null(hsp$core_log)) { # this was in version 0.8.64
		hsp$log <<- hsp$core_log
		hsp$core_log <<- NULL
	}
	setTextview(theWidget("log_textview"), hsp$log)
	addLogSeparator()
	hsp$log <<- NULL
	
	if (is.null(hsp$version) ||
		package_version(hsp$version) < package_version("0.5")) {
		errorDialog("Project file version not supported (check hsp$version)")
		stop("Project file version not supported (check hsp$version)")
	} else
	if (package_version(hsp$version) < package_version("0.6")) {
		# rename a Qual factor level
		for (i in seq(along=hsp$data)) {
			oldLevels <- levels(hsp$data[[i]]$Qual)
			oldLevels[oldLevels == "maybe"] <- "suspect"
			levels(hsp$data[[i]]$Qual) <<- oldLevels
		}
		addLogComment("NOTE: converted project for hydrosanity >= 0.6")
	}
	if (package_version(hsp$version) < package_version("0.8")) {
		# add "sitename" attribute
		for (i in seq(along=hsp$data)) {
			if (is.null(attr(hsp$data[[i]], "sitename"))) {
				attr(hsp$data[[i]], "sitename") <<- names(hsp$data)[i]
			}
		}
		# convert times to "GMT" timezone
		for (i in seq(along=hsp$data)) {
			tmp <- as.POSIXlt(hsp$data[[i]]$Time)
			attr(tmp, "tzone") <- "GMT"
			hsp$data[[i]]$Time <<- as.POSIXct(tmp)
		}
		if (!is.null(hsp$timePeriod)) {
			tmp <- as.POSIXlt(hsp$timePeriod)
			attr(tmp, "tzone") <- "GMT"
			hsp$timePeriod <<- as.POSIXct(tmp)
		}
		addLogComment("NOTE: converted project for hydrosanity >= 0.8")
	}
	hsp$version <<- NULL
	
	# switch to first page and trigger update
	theWidget("notebook")$setCurrentPage(1)
	datasetModificationUpdate()
	
	setIsImportMode(FALSE)
	
	# restore GUI state (only a few bits)
	
	# TODO: if (!is.null(hsp$selection))
	
	setStatusBar("Loaded project from ", dQuote(filename))
}

saveProject <- function(filename=NULL, saveAs=F) {
	freezeGUI()
	on.exit(thawGUI())
	
	if (is.null(filename)) {
		ff <- c("Hydrosanity projects (.hydrosanity)", "*.hydrosanity")
		filename <- hsp$projectFile
		if (is.null(filename)) filename <- ""
		if (saveAs==T || filename=="") {
			filename <- choose.file.save(filename, 
				caption="Save project", filters=ff)
			StateEnv$win$present()
			if (is.na(filename)) return()
		}
	}
	
	if (get.extension(filename) != "hydrosanity") {
		filename <- paste(filename, ".hydrosanity", sep='')
	}
	
	hsp$log <<- getTextviewText(theWidget("log_textview"))
	hsp$version <<- VERSION
	save(hsp, file=filename, compress=TRUE)
	hsp$log <<- NULL
	hsp$version <<- NULL
	hsp$modified <<- F
	hsp$projectFile <<- filename
	StateEnv$win['title'] <- paste("Hydrosanity:", get.stem(filename))
	
	setStatusBar("Project saved to ", dQuote(filename))
}


