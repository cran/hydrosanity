## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateRainPage <- function() {
	StateEnv$update$rain <- F
	StateEnv$win$present()
}

.hs_on_rain_view_surface_button_clicked <- function(button) {
	freezeGUI(use.core.log=F)
	on.exit(thawGUI())
	
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
	doRaw <- theWidget("rain_surface_raw_radiobutton")$getActive()
	doLinear <- theWidget("rain_linear_radiobutton")$getActive()
	doExtrap <- theWidget("rain_spline_extrap_radiobutton")$getActive()
	
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
	
	tmpObjs <- c(tmpObjs, 'tmp.data')
	guiDo(tmp.data <- lapply(hsp$data[tmp.names], window, 
		hsp$timePeriod[1], hsp$timePeriod[2]))
	
	if (doRaw) {
		tmpObjs <- c(tmpObjs, 'tmp.range')
		guiDo({
			# disaccumulate?
			tmp.data <- sync.timeblobs(tmp.data)
			tmp.range <- range(unlist(tmp.data[-1]), finite=T)
		})
		
		plot.call <- quote(levelplot(data ~ x * y | which, data={
				foo <- tmp.data[ii <- gui.index+seq(4)-1, -1]
				row.names(foo) <- format(tmp.data$Time[ii])
				dat <- do.call(make.groups, as.data.frame(t(foo)))
				dat[c('x','y')] <- tmp.locs
				dat
			}))
		plot.call$col.regions <- quote(sqrtPalette())
		plot.call$at <- quote(seq(tmp.range[1], tmp.range[2], length=100))
		plot.call$gui.step <- 4
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
				by="years", start.month=hsp$startMonth, fun.qual="omit"))
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
				by="3 months", start.month=hsp$startMonth, fun.qual="omit"))
			tmp.data$Season <- waterQuarters(tmp.data$Time, start.month=hsp$startMonth)
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
	
	plot.call$panel <- function(x, y, z, subscripts, at, col.regions=regions$col,
		linear=F, extrap=F) {
		x <- x[subscripts]
		y <- y[subscripts]
		z <- z[subscripts]
		if (sum(!is.na(z)) >= 4) {
			regions <- trellis.par.get("regions")
			panel.levelplot.interp(x, y, z, at=at, 
				col.regions=col.regions, linear=linear, extrap=extrap)
			if (FALSE) panel.contourplot.interp(x, y, z, 
				at=sqrtPretty(at), linear=linear, extrap=extrap)
		}
		panel.worldmap()
		panel.rivers()
		panel.cities()
		if (!is.null(hsp$catchment))
			sp.polygons(hsp$catchment)
		panel.points(x, y, pch=ifelse(is.na(z), 4, 21))
		#if (FALSE) panel.text(x, y, labels=row.names(points.xy))
	}
	plot.call$linear <- doLinear
	plot.call$extrap <- if (doExtrap) T
	
	if (!is.null(hsp$region)) {
		plot.call$xlim <- quote(hsp$region$xlim)
		plot.call$ylim <- quote(hsp$region$ylim)
	} else {
		plot.call$prepanel <- quote(prepanel.extend.10)
	}
	
	myExtras <- if (doRaw) list("index")
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(playwith(plot.call=plot.call, name="rainfall map", 
		extra.buttons=myExtras, 
		labels=rownames(tmp.locs),
		eval.args="^hsp$", invert=T, restore.on.close=StateEnv$win), 
		doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated rainfall map")
}

.hs_on_rain_view_mosaic_button_clicked <- function(button) {
	freezeGUI(use.core.log=F)
	on.exit(thawGUI())
	
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
	
	addLogComment("Generate Voronoi mosaic")
	
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
	guiDo(playwith(plot.call=plot.call, name="Voronoi mosaic", 
		extra.buttons=NULL,
		eval.args="^hsp$", invert=T, restore.on.close=StateEnv$win), 
		doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated Voronoi mosaic plot")
}

.hs_on_rain_make_areal_button_clicked <- function(button) {
	freezeGUI()
	on.exit(thawGUI())
	
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
	
	doByVoronoi <- theWidget("rain_area_byvoronoi_radiobutton")$getActive()
	doBySurface <- theWidget("rain_areal_bysurface_radiobutton")$getActive()
	
	if (doBySurface) {
		errorDialog("Sorry, this does not work yet.")
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
	
	addLogComment("Generate Voronoi mosaic")
	
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
	})
	
	if (doBySurface) {
		#grid1 <- readFltRaster("G:/Projects/Tuross/surface_files/rainfall_surface/year2000/rainGrid_20001.flt")
		#overlay(as(grid1, "SpatialPointsDataFrame"), tmp.subPolys, fn=mean)
	}
	
	guiDo({
		tmp.polyNames <- getSpPPolygonsIDSlots(tmp.subPolys)
		tmp.data <- lapply(hsp$data[tmp.polyNames], window, hsp$timePeriod[1], hsp$timePeriod[2])
		tmp.data <- lapply(tmp.data, quick.disaccumulate.timeblob)
		tmp.data <- sync.timeblobs(tmp.data)
		tmp.time <- tmp.data$Time
		tmp.areal <- apply(tmp.data[-1], ROWS<-1, weighted.mean, w=tmp.areaFrac)
		hsp$data[["areal"]] <- timeblob(Time=tmp.time, Data=tmp.areal)
	})
	
	setStatusBar("Generated areal rainfall series from ", length(tmp.subAreas), " gauges.")
	
	datasetModificationUpdate()
}

