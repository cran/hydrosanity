## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateRainPage <- function() {
	StateEnv$update$rain <- F
	StateEnv$win$present()
}

.hs_on_rain_view_surface_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	if (length(selNames) < 4) {
		errorDialog("Need at least 4 items for spatial interpolation.")
		return()
	}
	nBlobs <- length(selNames)
	
	doOverall <- theWidget("rain_surface_overall_radiobutton")$getActive()
	doQuarters <- theWidget("rain_surface_quarters_radiobutton")$getActive()
	doMonths <- theWidget("rain_surface_months_radiobutton")$getActive()
	doAnnual <- theWidget("rain_surface_annual_radiobutton")$getActive()
	doSplines <- theWidget("rain_spline_radiobutton")$getActive()
	doExtrap <- theWidget("rain_spline_extrapolation_checkbutton")$getActive()
	showPoints <- theWidget("rain_showpoints_checkbutton")$getActive()
	showCounts <- theWidget("rain_showcounts_checkbutton")$getActive()
	doElevContours <- theWidget("rain_elevation_contours_checkbutton")$getActive()
	
	myType <- if (doOverall) { "overall" } else
		if (doAnnual) { "annual" } else
		if (doQuarters) { "quarters" } else
		if (doMonths) { "months" }
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	
	if (any(!ok)) {
		errorDialog(paste("Some selected items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". De-select them, or try 'edit metadata' in the 'Dataset' tab."))
		return()
	}
	
	addLogComment("Generate rainfall map")
	
	tmpObjs <- c('tmp.names')
	
	guiDo(call=bquote({
		tmp.names <- .(selNames)
	}))
	
	tmpObjs <- c(tmpObjs, 'tmp.locs')
	guiDo({
		tmp.locs <- sapply(hsp$data[tmp.names], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	})
	
	grid.call <- call('spaceTimeField')
	grid.call[[2]] <- quote(hsp$data[tmp.names])
	grid.call$timelim <- if (!is.null(hsp$timePeriod)) { quote(hsp$timePeriod) }
	grid.call$type <- myType
	grid.call$start.month <- hsp$startMonth
	grid.call$linear <- !doSplines
	grid.call$extrap <- if (doExtrap) { T }
	grid.call$countSurface <- if (showCounts) { T }
	
	tmpObjs <- c(tmpObjs, 'tmp.grid')
	grid.assign.call <- quote(tmp.grid <- foo)
	grid.assign.call[[3]] <- grid.call
	guiDo(call=grid.assign.call)
	
	plot.call <- switch(myType,
		overall=quote(
			levelplot(z ~ x * y, tmp.grid, aspect="iso")
		),
		annual=quote(
			levelplot(z ~ x * y | year, tmp.grid, aspect="iso",
				as.table=T)
		),
		quarters=quote(
			levelplot(z ~ x * y | quarter, tmp.grid, aspect="iso",
				as.table=T)
		),
		months=quote(
			levelplot(z ~ x * y | month, tmp.grid, aspect="iso",
				as.table=T)
		)
	)
	plot.call$panel <- quote(panel.geo)
	if (showPoints) {
		plot.call$points.xy <- quote(tmp.locs)
	}
	
	if (!is.null(hsp$catchment)) {
		plot.call$catchment.poly <- quote(hsp$catchment)
	}
	
	plot.call$prepanel <- quote(prepanel.extend.10)
	if (!is.null(hsp$region)) {
		plot.call$xlim <- quote(hsp$region$xlim)
		plot.call$ylim <- quote(hsp$region$ylim)
	}
	
	# hydrosanity caption
	# TODO
	
	id.call <- call('panel.identify')
	id.call$x <- tmp.locs
	id.call$labels <- rownames(tmp.locs)
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(playwith(plot.call=plot.call, name="rainfall map", 
		extra.buttons=hydrosanityButtons[c('setregion')], 
		identify.call=id.call, 
		eval.args="^hsp$", invert=T, restore.on.close=StateEnv$win), 
		doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated rainfall map")
}

