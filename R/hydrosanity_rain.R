## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateRainPage <- function() {
	StateEnv$update$rain <- F
	StateEnv$win$present()
}

.hs_on_rain_view_surface_button_clicked <- function(button) {
	freezeGUI(echo.to.log=F)
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) < 2) {
		errorDialog("First, select some items to display (more than one).")
		return()
	}
	nBlobs <- length(selNames)
	
	doRaw <- theWidget("rain_surface_raw_radiobutton")$getActive()
	doAnnual <- theWidget("rain_surface_annual_radiobutton")$getActive()
	doQuarters <- theWidget("rain_surface_quarters_radiobutton")$getActive()
	doMonths <- theWidget("rain_surface_months_radiobutton")$getActive()
	doOverall <- theWidget("rain_surface_overall_radiobutton")$getActive()
	
	myType <- if (doOverall) "overall" else
		if (doAnnual) "annual" else
		if (doQuarters) "quarters" else
		if (doMonths) "months" else
		if (doRaw) "raw"
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	
	if (any(!ok)) {
		errorDialog(paste("Some selected items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". De-select them, or try 'edit metadata' in the 'Dataset' tab."))
		return()
	}
	
	addLogComment("Generate rainfall mosaic plot")
	
	tmpObjs <- c('tmp.names')
	
	guiDo(call=bquote({
		tmp.names <- .(selNames)
	}))
	
	tmpObjs <- c(tmpObjs, 'tmp.locs')
	guiDo({
		tmp.locs <- sapply(hsp$data[tmp.names], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	})
	
	tmpObjs <- c(tmpObjs, 'tmp.data')
	guiDo(tmp.data <- lapply(hsp$data[tmp.names], window, 
		hsp$timePeriod[1], hsp$timePeriod[2]))
	
	if (doRaw) {
		tmpObjs <- c(tmpObjs, 'tmp.range')
		if (any(sapply(tmp.data, function(x) 
			any(x$AccumSteps > 1, na.rm=T) ))) {
			if (is.null(questionDialog("It is recommended that you ",
				"disaccumulate the data first, as it makes this ",
				"plot easier to understand. You can do this from ",
				"the Impute tab. Continue anyway?"))) {
					StateEnv$win$present()
					return()
			}
			StateEnv$win$present()
		}
		guiDo({
			tmp.data <- sync.timeblobs(tmp.data)
			tmp.range <- range(unlist(tmp.data[-1]), finite=T)
		})
		
		plot.call <- quote(levelplot(data ~ x * y | which, data={
				foo <- tmp.data[ii <- cur.index+seq(4)-1, -1]
				row.names(foo) <- format(tmp.data$Time[ii])
				dat <- do.call(make.groups, as.data.frame(t(foo)))
				dat[c('x','y')] <- tmp.locs
				dat
			}))
		plot.call$col.regions <- quote(sqrtPalette())
		plot.call$at <- quote(seq(tmp.range[1], tmp.range[2], length=100))
		#plot.call$gui.step <- 4
	}
	
	if (doOverall) {
		guiDo({
			tmp.data <- lapply(tmp.data, quick.disaccumulate.timeblob)
			tmp.data <- lapply.timeblob.data(tmp.data, mean, na.rm=T)
			tmp.data <- cbind(tmp.locs, z=unlist(tmp.data))
		})
		plot.call <- quote(levelplot(z ~ x * y, tmp.data))
	}
	if (doAnnual) {
		tmpObjs <- c(tmpObjs, 'tmp.goo')
		guiDo({
			tmp.data <- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, 
				by="years", start.month=hsp$yearStart, fun.qual="omit"))
			tmp.data$Time <- factor(as.POSIXlt(tmp.data$Time)$year+1900)
			tmp.goo <- melt(tmp.data, id="Time", variable_name="site")
			tmp.goo <- cast(tmp.goo, site ~ Time)
			tmp.goo <- cbind(tmp.goo, tmp.locs)
			tmp.goo <- melt(tmp.goo, id=c("site","x","y"), variable_name="Time")
		})
		plot.call <- quote(levelplot(value ~ x * y | Time, tmp.goo))
		if (nrow(tmp.data) > 9) plot.call$layout <- c(0, 9)
	}
	if (doQuarters) {
		guiDo({
			tmp.data <- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, 
				by="3 months", start.month=hsp$yearStart, fun.qual="omit"))
			tmp.data$Season <- waterQuarters(tmp.data$Time, start.month=hsp$yearStart)
		})
	}
	if (doMonths) {
		guiDo({
			tmp.data <- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, 
				by="months", fun.qual="omit"))
			tmp.data$Season <- factor(months(tmp.data$Time, abbreviate=TRUE),
				levels=c("Jan","Feb","Mar","Apr","May","Jun",
				"Jul","Aug","Sep","Oct","Nov","Dec"), ordered=T)
		})
	}
	if (doQuarters || doMonths) {
		tmpObjs <- c(tmpObjs, 'tmp.goo')
		guiDo({
			tmp.goo <- melt(tmp.data[-1], id="Season", variable_name="site")
			tmp.goo <- cast(tmp.goo, site ~ Season, fun.aggregate=mean, na.rm=T)
			tmp.goo <- cbind(tmp.goo, tmp.locs)
			tmp.goo <- melt(tmp.goo, id=c("site","x","y"), variable_name="Season")
		})
		plot.call <- quote(levelplot(value ~ x * y | Season, tmp.goo))
	}
	
	plot.call$aspect <- "iso"
	plot.call$as.table <- if (!doOverall) T
	
	plot.call$panel <- function(x, y, z, subscripts, at, col.regions=regions$col) {
		x <- x[subscripts]
		y <- y[subscripts]
		z <- z[subscripts]
		if (sum(!is.na(z)) >= 1) {
			regions <- trellis.par.get("regions")
			panel.levelplot.mosaic(x, y, z, at=at, 
				col.regions=col.regions)
			if (FALSE) panel.levelplot.interp(x, y, z, at=at, 
				col.regions=col.regions, linear=F, extrap=T)
			if (FALSE) panel.contourplot.interp(x, y, z, 
				at=sqrtPretty(at), linear=F, extrap=T)
		}
		panel.worldmap()
		panel.rivers()
		panel.cities()
		if (!is.null(hsp$catchment))
			sp.polygons(hsp$catchment)
		panel.points(x, y, pch=ifelse(is.na(z), 4, 21))
		#if (FALSE) panel.text(x, y, labels=row.names(points.xy)) TODO
	}
	
	if (length(hsp$region)) {
		plot.call$xlim <- quote(hsp$region$xlim)
		plot.call$ylim <- quote(hsp$region$ylim)
	} else {
		plot.call$prepanel <- quote(prepanel.extend.10)
	}
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	playwith(plot.call=plot.call, name="rainfall mosaic", 
		time.vector=if (doRaw) tmp.data$Time,
		time.mode.page.incr=4,
		labels=rownames(tmp.locs),
		eval.args="^hsp$", invert.match=T, on.close=restoreHS)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated rainfall mosiac plot")
}

.hs_on_rain_view_grids_button_clicked <- function(button) {
	freezeGUI(echo.to.log=F)
	on.exit(thawGUI())
	
	filePattern <- theWidget("rain_grid_files_pattern_entry")$getText()
	fromYear <- theWidget("rain_grids_from_year_spinbutton")$getValue()
	toYear <- theWidget("rain_grids_to_year_spinbutton")$getValue()
	
	monthsVary <- any(grep("%#?[mb]", filePattern))
	yearsVary <- any(grep("%#?[Yy]", filePattern))
	
	addLogComment("Generate plot of areal rainfall grids")
	
	tmpObjs <- c()
	
	guiDo(library(rgdal))
	
	if (!yearsVary && !monthsVary) {
		tmpObjs <- c(tmpObjs, "tmp.grid")
		guiDo(call=bquote(
			tmp.grid <- readGDAL_fixed(.(filePattern))
		))
		plot.call <- quote(
			levelplot(band1 ~ x * y, data=as(tmp.grid, "data.frame"))
		)
	}
	
	if (yearsVary || monthsVary) {
		tmpObjs <- c(tmpObjs, "tmp.times")
		
		guiDo(call=bquote({
			tmp.times <- seq(ISOdate(.(fromYear),1,1,0), 
				ISOdate(.(toYear),1,1,0)-1, 
				by=.(if (monthsVary) "months" else "years"))
		}))
		
		plot.call <- bquote(
			spplot(readGDAL_fixed(format(tmp.times[gui.index], .(filePattern))), 
				formula=band1~x*y | format(tmp.times[gui.index], 
				.(if (monthsVary) "%Y" else "%Y %b")))
		)
		
		plot.call <- bquote(
			levelplot(band1 ~ x * y | format(cur.time, 
				.(if (monthsVary) "%Y %b" else "%Y")), 
				data={
					fname <- .(filePattern)
					myGrid <- readGDAL_fixed(format(cur.time, fname))
					as(myGrid, "data.frame")
				})
		)
	}
	
	plot.call$aspect <- "iso"
	
	plot.call$panel <- function(...) {
		panel.levelplot(...)
		panel.worldmap()
		panel.rivers()
		panel.cities()
		if (!is.null(hsp$catchment))
			sp.polygons(hsp$catchment)
	}
	
	if (!is.null(hsp$region)) {
		plot.call$xlim <- quote(hsp$region$xlim)
		plot.call$ylim <- quote(hsp$region$ylim)
	}
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	playwith(plot.call=plot.call, title="Areal rainfall grids", 
		time.vector=if (yearsVary || monthsVary) tmp.times,
		labels=NA,
		eval.args="^hsp$", invert.match=T, on.close=restoreHS)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated plot of areal rainfall grids")
}

.hs_on_rain_grids_make_areal_button_clicked <- function(button) {
	freezeGUI()
	on.exit(thawGUI())
	
	filePattern <- theWidget("rain_grid_files_pattern_entry")$getText()
	fromYear <- theWidget("rain_grids_from_year_spinbutton")$getValue()
	toYear <- theWidget("rain_grids_to_year_spinbutton")$getValue()
	monthsVary <- any(grep("%#?[mb]", filePattern))
	yearsVary <- any(grep("%#?[Yy]", filePattern))
	
	addLogComment("Generate areal rainfall from grids")
	
	tmpObjs <- c("tmp.times")
		
	if (!yearsVary && !monthsVary) {
		guiDo(call=bquote({
			tmp.times <- ISOdate(.(fromYear),1,1,0)
		}))
	} else {
		guiDo(call=bquote({
			tmp.times <- seq(ISOdate(.(fromYear),1,1,0), 
				ISOdate(.(toYear+1),1,1,0)-1, 
				by=.(if (monthsVary) "months" else "years"))
		}))
	}
	
	tmpObjs <- c(tmpObjs, "tmp.data", "tmp.fname", "myGrid")
	
	guiDo(call=bquote({
		tmp.fname <- .(filePattern)
	}))
	
	guiDo({
		tmp.data <- 0
		for (i in seq_along(tmp.times)) {
			myGrid <- try(readGDAL_fixed(format(tmp.times[i], tmp.fname)))
			if (inherits(myGrid, "try-error")) tmp.data[i] <- NA
			else tmp.data[i] <- overlay(as(myGrid, "SpatialPointsDataFrame"),
				hsp$catchment, fn=mean)[,1]
		}
	})
	
	guiDo(hsp$data[["areal.grids"]] <- timeblob(Time=tmp.times, Data=tmp.data, role="AREAL"))
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated areal rainfall from grids")
	
	datasetModificationUpdate()
}

.hs_on_rain_view_mosaic_button_clicked <- function(button) {
	freezeGUI(echo.to.log=F)
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) < 2) {
		errorDialog("First, select some items to display (more than one).")
		return()
	}
	nBlobs <- length(selNames)
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	
	if (any(!ok)) {
		errorDialog(paste("Some selected items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". De-select them, or try 'edit metadata' in the 'Dataset' tab."))
		return()
	}
	
	if (is.null(hsp$catchment)) {
		errorDialog("This function requires a catchment polygon (import it from 'Scope' tab).")
		return()
	}
	
	addLogComment("Generate Thiessen polygons")
	
	tmpObjs <- c('tmp.names')
	
	guiDo(call=bquote({
		tmp.names <- .(selNames)
	}))
	
	tmpObjs <- c(tmpObjs, 'tmp.locs')
	guiDo({
		tmp.locs <- sapply(hsp$data[tmp.names], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	})
	
	guiDo({
		tmp.poly <- hsp$catchment@polygons[[1]]@Polygons[[1]]
		tmp.subPolys <- arealSubPolygons(tmp.locs, boundary=coordinates(tmp.poly))
	})
	
	plot.call <- quote(xyplot(y ~ x, tmp.locs, aspect="iso"))
	plot.call$labels <- quote(tmp.names)
	plot.call$polys <- quote(tmp.subPolys)
	plot.call$panel <- function(x, y, labels, polys, ...) {
		panel.worldmap()
		panel.rivers()
		panel.cities()
		sp.polygons(polys)
		if (FALSE) panel.points(x, y)
		panel.text(x, y, labels, ...)
		centroids <- getSpPPolygonsLabptSlots(polys)
		polySites <- match(getSpPPolygonsIDSlots(polys), labels)
		panel.segments(x0=x[polySites], y0=y[polySites],
			x1=centroids[,1], y1=centroids[,2], lty=2, col="red")
	}
	
	if (!is.null(hsp$region)) {
		plot.call$xlim <- quote(hsp$region$xlim)
		plot.call$ylim <- quote(hsp$region$ylim)
	} else {
		plot.call$prepanel <- quote(prepanel.extend.10)
	}
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	playwith(plot.call=plot.call, title="Thiessen polygons", 
		eval.args="^hsp$", invert.match=T, on.close=restoreHS)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated Thiessen polygons plot")
}

.hs_on_rain_mosaic_make_areal_button_clicked <- function(button) {
	freezeGUI()
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) < 2) {
		errorDialog("First, select some items to work with (more than one).")
		return()
	}
	nBlobs <- length(selNames)
	
	doByWeighting <- theWidget("rain_area_by_weighting_radiobutton")$getActive()
	doByGrids <- theWidget("rain_areal_by_grids_radiobutton")$getActive()
	filePattern <- theWidget("rain_grid_files_pattern_entry")$getText()
	fromYear <- theWidget("rain_grids_from_year_spinbutton")$getValue()
	toYear <- theWidget("rain_grids_to_year_spinbutton")$getValue()
	monthsVary <- any(grep("%#?[mb]", filePattern))
	yearsVary <- any(grep("%#?[Yy]", filePattern))
	
	if (!yearsVary) { toYear <- fromYear }
	
	if (doByGrids && !yearsVary && !monthsVary) {
		errorDialog("Only one grid was specified. This is not supported (yet).")
		return()
	}
	
	loc <- lapply(hsp$data[selNames], attr, "location.xy")
	ok <- (sapply(loc, length) == 2)
	
	if (any(!ok)) {
		errorDialog(paste("Some selected items do not have a valid 'location.xy' attribute:",
			paste(selNames[!ok], collapse=", "),
			". De-select them, or try 'edit metadata' in the 'Dataset' tab."))
		return()
	}
	
	if (is.null(hsp$catchment)) {
		errorDialog("This function requires a catchment polygon (import it from 'Scope' tab).")
		return()
	}
	
	addLogComment("Generate areal rainfall from Thiessen polygons")
	
	tmpObjs <- c('tmp.names')
	
	guiDo(call=bquote(
		tmp.names <- .(selNames)
	))
	
	tmpObjs <- c(tmpObjs, 'tmp.locs')
	guiDo({
		tmp.locs <- sapply(hsp$data[tmp.names], attr, "location.xy")
		tmp.locs <- data.frame(x=tmp.locs[1,], y=tmp.locs[2,])
	})
	
	guiDo({
		tmp.poly <- hsp$catchment@polygons[[1]]@Polygons[[1]]
		tmp.subPolys <- arealSubPolygons(tmp.locs, boundary=coordinates(tmp.poly))
		tmp.subAreas <- sapply(tmp.subPolys@polygons, getPolygonAreaSlot)
		tmp.areaFrac <- tmp.subAreas / tmp.poly@area
		
		tmp.polyNames <- getSpPPolygonsIDSlots(tmp.subPolys)
		tmp.data <- lapply(hsp$data[tmp.polyNames], window, hsp$timePeriod[1], hsp$timePeriod[2])
		tmp.data <- lapply(tmp.data, quick.disaccumulate.timeblob)
		tmp.sync <- sync.timeblobs(tmp.data)
		tmp.synctime <- tmp.sync$Time
		tmp.sync <- tmp.sync[-1]
	})
	
	if (doByGrids) {
		addToLog("# Read in grids")
		
		tmpObjs <- c(tmpObjs, "tmp.times")
		
		myBy <- if (monthsVary) "months" else "years"
		guiDo(call=bquote({
			tmp.gridtime <- seq(ISOdate(.(fromYear),1,1,0), 
				ISOdate(.(toYear+1),1,1,0)-1, 
				by=.(myBy))
		}))
		
		tmpObjs <- c(tmpObjs, "tmp.data", "tmp.fname", "myGrid")
		
		guiDo(call=bquote({
			tmp.by <- .(myBy)
			tmp.fname <- .(filePattern)
		}))
		
		guiDo({
			tmp.gridvals <- list()
			for (i in seq_along(tmp.gridtime)) {
				myGrid <- try(readGDAL_fixed(format(tmp.gridtime[i], tmp.fname)))
				tmp.gridvals[[i]] <- if (inherits(myGrid, "try-error")) NA
					else overlay(as(myGrid, "SpatialPointsDataFrame"),
						tmp.subPolys, fn=mean)[,1]
			}
			# columns are time steps, rows are sub-polygon regions
			tmp.gridvals <- as.data.frame(tmp.gridvals)
			# transpose, so columns are regions, rows are time steps
			tmp.gridvals <- as.data.frame(t(tmp.gridvals))
			# make it a timeblob
			tmp.gridblob <- timeblob(Time=tmp.gridtime, Data=tmp.gridvals)
			
			# now aggregate gauge data to monthly and sync it
			tmp.monthdata <- lapply(tmp.data, aggregate, by=tmp.by)
			tmp.monthdata <- syncTo.timeblobs(tmp.monthdata, blob=tmp.gridblob)[-1]
			tmp.scale <- tmp.monthdata
			
			# scale each month of the gauge data so month sum equal to grid value
			for (i in seq_along(tmp.monthdata)) {
				# replace zeros with -1 (arbitrary) to avoid Inf
				tmp.monthdata[[i]] <- ifelse(tmp.monthdata[[i]] == 0, 
					-1, tmp.monthdata[[i]])
				tmp.scale[[i]] <- tmp.gridvals[[i]] / tmp.monthdata[[i]]
			}
			
			# apply scale factors to original data
			mySyncIndices <- matchtimes.timeblob(tmp.gridblob, times=tmp.synctime)
			for (i in seq_along(tmp.sync)) {
				tmp.sync[[i]] <- tmp.sync[[i]] * tmp.scale[[i]][mySyncIndices]
			}
		})
	}
	
	# compute areal (area-weighted) time series
	guiDo({
		tmp.areal <- apply(tmp.sync, ROWS<-1, weighted.mean, w=tmp.areaFrac)
		hsp$data[["areal.weighted"]] <- timeblob(Time=tmp.synctime, Data=tmp.areal, role="AREAL")
	})
	
	# TODO: remove tmps
	
	setStatusBar("Generated areal rainfall by weighting ", length(tmp.subAreas), " gauges.")
	
	datasetModificationUpdate()
}


## NON-ACTIONS, just interface bits and pieces

.hs_on_rain_choose_grid_file_button_clicked <- function(button) {
	filename <- try(file.choose(), silent=T)
	StateEnv$win$present()
	if (inherits(filename, "try-error")) return()
	theWidget("rain_grid_files_pattern_entry")$setText(filename)
}

