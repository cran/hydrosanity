## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL



grid.timeline.plot <- function(blob.list, xscale=NULL, colMap=NULL, barThickness=unit(1.2,"lines"), auto.key=T, maxLabelChars=20, pad=unit(1,"lines"), grill=T, main=NULL, sub=T, newpage=T) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		xscale <- timelim.timeblobs(blob.list)
	} else {
		xscale <- as.POSIXct(xscale)
		if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window, xscale[1], xscale[2], inclusive=T)
	}
	# setup
	nBlobs <- length(blob.list)
	ylabs <- sapply(names(blob.list), toString, width=maxLabelChars)
	maxlab <- ylabs[ which.max(nchar(ylabs, "width")) ]
	theKey <- NULL
	keyHeight <- unit(0, "npc")
	if (auto.key && !identical(colMap, NA)) {
		if (is.null(colMap)) { colMap <- timelineColMapDefault() }
		usedLevels <- unique(unlist(lapply(blob.list, 
			function(x) { levels(x$Qual[,drop=T]) })))
		usedIdx <- sort(match(usedLevels, names(colMap)))
		keyList <- list(text=list(names(colMap)[usedIdx]),
			rectangles=list(col=unlist(colMap, use.names=F)[usedIdx],
			size=4), columns=length(usedIdx), between.columns=2,
			between=1)
		if (length(usedIdx) > 0) {
			theKey <- draw.key(keyList)
			keyHeight <- grobHeight(theKey)
		}
	}
	mainHeight <- unit(0, "npc")
	subHeight <- unit(0, "npc")
	if (!is.null(main) && !identical(main, F)) {
		if (is.character(main)) { main <- textGrob(main) }
		mainHeight <- grobHeight(mainHeight)
	}
	if (!is.null(sub) && !identical(sub, F)) {
		if (identical(sub, T)) {
			mySync <- sync.timeblobs(blob.list)
			dataPoints <- sum(is.na(unlist(mySync[-1]))==F)
			sub <- hydrosanity.caption(
				timelim.timeblobs(blob.list),
				by=attr(mySync, "timestep"), n=dataPoints, series=nBlobs)
		}
		if (is.character(sub)) { sub <- textGrob(sub) }
		subHeight <- grobHeight(sub)
	}
	if (newpage) { grid.newpage() }
	# layout for plot
	pushViewport(viewport(name="titles.layout",
		layout=grid.layout(5, 1,
			heights=unit.c(pad, mainHeight, keyHeight, 
					unit(1,"null"), subHeight))))
	pushViewport(viewport(name="timeline.plot.layout",
		layout.pos.row=4, 
		layout=grid.layout(3, 3,
			widths=unit.c(stringWidth(maxlab)+pad, unit(1,"null"), pad),
			heights=unit.c(pad, unit(1,"null"), unit(3, "lines")))))
	# overall plot viewport, and layout for timeline bars
	pushViewport(viewport(name="time.vp", 
		layout.pos.col=2, layout.pos.row=2, xscale=xscale,
		layout=grid.layout(nBlobs*2+1, 1,
			heights=unit.c(unit(1,"null"), 
				rep(unit.c(barThickness, unit(1,"null")), nBlobs)))))
	# draw axis and grill
	grid.lines(y=unit(0,"npc"))
	axisGrob <- grid.xaxis.POSIXt(name="timeline.xaxis")
	if (grill) {
		grillX <- axisGrob$at[axisGrob$label != ""]
		grid.segments(grillX, unit(0,"npc"), grillX, unit(1,"npc"),
			default.units="native", gp=gpar(col="grey"))
	}
	for (k in 1:nBlobs) {
		# draw timeline bar number k
		pushViewport(viewport(name=paste("timeline.bar",k,".vp",sep=''),
			layout.pos.row=k*2, xscale=xscale, clip="on"))
		grid.timeline.bar(blob.list[[k]], colMap=colMap,
			name=paste("timeline.bar",k,sep=''))
		# draw label number k
		pushViewport(viewport(clip="off"))
		grid.text(ylabs[k], x=0, just="right",
			name=paste("label",k,sep=''))
		upViewport()
		upViewport()
	}
	# come back up
	upViewport(2)
	# draw titles
	if (!is.null(main) && !identical(main, F)) {
		pushViewport(viewport(name="main.vp", layout.pos.row=2))
		grid.draw(main)
		upViewport()
	}
	if (!is.null(sub) && !identical(sub, F)) {
		pushViewport(viewport(name="sub.vp", layout.pos.row=5))
		grid.draw(sub)
		upViewport()
	}
	if (!is.null(theKey)) {
		pushViewport(viewport(name="key.vp", layout.pos.row=3))
		grid.draw(theKey)
		upViewport()
	}
	upViewport(1)
}

timelineColMapDefault <- function(colMap=list(
		good="black", 
		suspect=trellis.par.get("superpose.polygon")$col[1], 
		poor=trellis.par.get("superpose.polygon")$col[2], 
		disaccumulated=trellis.par.get("superpose.polygon")$col[3], 
		imputed=trellis.par.get("superpose.polygon")$col[4])) {
	if (is.null(colMap)) {
		eval(formals(timelineColMapDefault)$colMap)
	} else { colMap }
}

#if colMap=NA then ignore quality codes, just plot non-NA values as black
grid.timeline.bar <- function(blob, colMap=NULL, name="timeline.bar", vp=NULL) {
	# check types
	if (is.null(colMap)) { colMap <- timelineColMapDefault() }
	if (!is.timeblob(blob)) { stop("blob must be a timeblob") }
	if (nrow(blob)==0) { return() }
	thisNA <- is.na(blob$Data)
	# default, if not plotting Qual codes
	thisCol <- rep(as.factor("black"), nrow(blob))
	if (!is.na(colMap) && !is.null(blob$Qual)) {
		thisCol <- applyColourMap(blob$Qual, colMap)
	}
	# vector of integer codes for each colour as well as NA
	colIntCodes <- as.integer(thisCol)
	colIntCodes[thisNA] <- -1 # unique code for NA
	# find indices (time steps) where colour (or is.na) changes
	# i.e. divide into blocks of continuous colour (for efficient plotting)
	blockStartIndex <- which(c(1,diff(colIntCodes))!=0)
	nBlock <- length(blockStartIndex)
	blockStart <- as.numeric(blob$Time[blockStartIndex])
	blockEnd <- c(blockStart[-1], end(blob))
	blockWidth <- blockEnd - blockStart
	# get subset of colours and convert to character strings
	blockCol <- levels(thisCol)[ thisCol[blockStartIndex] ]
	# set colour to NA where data is NA
	blockCol[thisNA[blockStartIndex]] <- NA
	# draw it
	grid.rect(x=blockStart, width=blockWidth, just="left",
		default.units="native", gp=gpar(fill=blockCol, col=NA),
		name=name, vp=vp)
}


grid.timeseries.plot.superpose <- function(superpose.blob.list, allSameScales=F, xscale=NULL, yscale=NULL, logScale=F, sub=T, ...) {
	# check types
	if (!identical(class(superpose.blob.list),"list")) {
		stop("'superpose.blob.list' must be a list of lists of timeblobs")
	}
	for (i in seq(along=superpose.blob.list)) {
		if (!identical(class(superpose.blob.list[[i]]),"list")) {
			stop("'superpose.blob.list' must be a list of lists of timeblobs")
		}
	}
	if (is.null(xscale)) {
		xscale <- min(lapply(superpose.blob.list, start.timeblobs))
		xscale[2] <- max(lapply(superpose.blob.list, end.timeblobs))
	} else {
		xscale <- as.POSIXct(xscale)
		if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
		for (i in seq(along=superpose.blob.list)) {
			superpose.blob.list[[i]] <- lapply(superpose.blob.list[[i]],
				window, xscale[1], xscale[2], inclusive=T)
		}
	}
	# make common yscale
	if (is.null(yscale) && allSameScales
		&& sum(sapply(c(unlist(superpose.blob.list, recursive=F)), nrow) == 0)) {
		# no data for any series
		yscale <- c(1, 10)
	} else
	if (is.null(yscale) && allSameScales) {
		allRanges <- sapply.timeblob.data(
			c(unlist(superpose.blob.list, recursive=F)), 
			range, finite=T)
		yscale <- range(allRanges[is.finite(allRanges)])
		if (logScale && (yscale[1] <= 0)) {
			# limit by minimum non-zero value (for log scale)
			allMins <- sapply.timeblob.data(
				c(unlist(superpose.blob.list, recursive=F)), 
				function(x){ min(x[x>0], na.rm=T) })
			yscale[1] <- min(allMins[is.finite(allMins)])
		}
		if (!logScale) { yscale[1] <- 0 }
	}
	# make caption
	if (identical(sub, T)) {
		mySync <- sync.timeblobs(c(unlist(superpose.blob.list, recursive=F)))
		dataPoints <- sum(is.na(unlist(mySync[-1]))==F)
		sub <- hydrosanity.caption(range(mySync$Time),
			by=attr(mySync, "timestep"), n=dataPoints, series=ncol(mySync)-1)
	}
	# call grid.timeseries.plot for each superposed layer
	for (layer in seq(along=superpose.blob.list)) {
		this.args <- list(...)
		this.args$blob.list <- superpose.blob.list[[layer]]
		this.args$superPos <- layer
		this.args$nSuperpose <- length(superpose.blob.list)
		this.args$xscale <- xscale
		this.args$yscale <- yscale
		this.args$logScale <- logScale
		if (!is.null(this.args$yscale)) { this.args$newScale <- F }
		this.args$sub <- sub
		do.call(grid.timeseries.plot, this.args)
	}
}


grid.timeseries.plot <- function(blob.list, xscale=NULL, yscale=NULL, sameScales=T, logScale=F, qualTimeline=F, colMap=NULL, barThickness=unit(0.5,"lines"), auto.key=T, maxLabelChars=20, pad=unit(1,"lines"), between=unit(0,"lines"), superPos=1, newScale=T, main=NULL, sub=T, newpage=(superPos==1), nSuperpose=1, gp=gpar(col=rep(trellis.par.get("superpose.line")$col, len=superPos)[superPos], lty=rep(trellis.par.get("superpose.line")$lty, len=superPos)[superPos])) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(xscale)) {
		if (superPos == 1) {
			xscale <- timelim.timeblobs(blob.list)
		} else {
			depth <- downViewport("time.vp")
			xscale <- as.POSIXct.numeric(convertX(unit(c(0,1), "npc"), "native"))
			upViewport(depth)
		}
	} else {
		xscale <- as.POSIXct(xscale)
		if (any(is.na(xscale))) { stop("'xscale' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window, xscale[1], xscale[2], inclusive=T)
	}
	if (!is.null(yscale) && !is.numeric(yscale)) { stop("'yscale' must be numeric") }
	# setup
	nBlobs <- length(blob.list)
	ylabs <- sapply(names(blob.list), toString, width=maxLabelChars)
	theKey <- NULL
	keyHeight <- unit(0, "npc")
	if (auto.key && qualTimeline && (superPos==1)) {
		if (is.null(colMap)) { colMap <- timelineColMapDefault() }
		usedLevels <- unique(unlist(lapply(blob.list, 
			function(x) { levels(x$Qual[,drop=T]) })))
		keyLabels <- "data"
		keyCols <- "black"
		if ((length(usedLevels) > 0) && !identical(colMap, NA)) {
			usedIdx <- sort(match(usedLevels, names(colMap)))
			if (length(usedIdx) > 0) {
				keyLabels <- names(colMap)[usedIdx]
				keyCols <- unlist(colMap, use.names=F)[usedIdx]
			}
		}
		if (any(unlist(lapply.timeblob.data(blob.list, is.na)))) {
			keyLabels <- c(keyLabels, "missing")
			keyCols <- c(keyCols, "transparent")
		}
		keyList <- list(text=list(keyLabels),
			rectangles=list(col=keyCols, size=4), 
			columns=length(keyLabels), between.columns=2,
			between=1)
		theKey <- draw.key(keyList)
		keyHeight <- grobHeight(theKey)
	}
	mainHeight <- unit(0, "npc")
	subHeight <- unit(0, "npc")
	if (!is.null(main) && !identical(main, F)) {
		if (is.character(main)) { main <- textGrob(main) }
		mainHeight <- grobHeight(mainHeight)
	}
	if (!is.null(sub) && !identical(sub, F)) {
		if (identical(sub, T)) {
			mySync <- sync.timeblobs(blob.list)
			dataPoints <- sum(is.na(unlist(mySync[-1]))==F)
			sub <- hydrosanity.caption(timelim.timeblobs(blob.list),
				by=attr(mySync, "timestep"), n=dataPoints, series=nBlobs)
		}
		if (is.character(sub)) { sub <- textGrob(sub) }
		subHeight <- grobHeight(sub)
	}
	if (newpage) { grid.newpage() }
	# layout for plot
	if (!qualTimeline) { barThickness <- unit(0,"mm") }
	yLabSpace <- unit(1.5, "lines")
	yAxisWidth <- unit(3, "lines")
	yAxesSpace <- yAxisWidth
	if (newScale) { yAxesSpace <- yAxisWidth * nSuperpose }
	if (superPos == 1) {
		pushViewport(viewport(name="titles.layout",
		layout=grid.layout(5, 1,
			heights=unit.c(pad, mainHeight, keyHeight, 
					unit(1,"null"), subHeight))))
		pushViewport(viewport(name="timeseries.plot.layout",
			layout.pos.row=4, 
			layout=grid.layout(3, 3,
			widths=unit.c(yLabSpace+yAxesSpace, unit(1,"null"), pad),
			heights=unit.c(pad, unit(1,"null"), unit(3, "lines")))))
		# overall plot viewport, and layout for timeseries plots
		pushViewport(viewport(name="time.vp", 
			layout.pos.col=2, layout.pos.row=2, xscale=xscale,
			layout=grid.layout(nBlobs*3, 1,
			heights=rep(unit.c(between, unit(1,"null"), barThickness), nBlobs))))
		# draw time axis with labels at bottom of plot
		grid.xaxis.POSIXt(name="timeseries.xaxis")
		grid.lines(y=0)
	}
	# calculate common yscale
	if (is.null(yscale) && sameScales
		&& (sum(sapply(blob.list, nrow) == 0))) {
		# no data for any series
		yscale <- c(1, 10)
	} else
	if (is.null(yscale) && sameScales) {
		allRanges <- sapply.timeblob.data(blob.list, range, finite=T)
		yscale <- range(allRanges[is.finite(allRanges)])
		if (logScale && (yscale[1] <= 0)) {
			# limit by minimum non-zero value (for log scale)
			allMins <- sapply.timeblob.data(blob.list, 
				function(x){ min(x[x>0], na.rm=T) })
			yscale[1] <- min(allMins[is.finite(allMins)])
		}
		if (!logScale) { yscale[1] <- 0 }
	}
	# plot each timeblob in the list
	for (k in 1:nBlobs) {
		# allow skipping for superposed series
		if (is.null(blob.list[[k]])) { next }
		# set up vertical scale for timeseries number k
		myYScale <- yscale
		if (is.null(yscale) && nrow(blob.list[[k]])==0) {
			# no data for this series
			myYScale <- c(1, 10)
		} else
		if (is.null(yscale)) {
			myYScale <- range(blob.list[[k]]$Data, finite=T)
			# need to ensure yscale > 0 for log scale
			if (logScale) {
				if (myYScale[1] <= 0) {
					x <- blob.list[[k]]$Data
					myYScale[1] <- min(x[x>0], na.rm=T)
				}
			}
			if (!logScale) { myYScale[1] <- 0 }
		}
		if (logScale) {
			myYScale <- log10(myYScale)
		}
		# extend yscale for clarity
		myYScale <- extendrange(myYScale)
		# do plot
		if (superPos == 1) {
			# create viewport for timeseries number k
			pushViewport(viewport(
				name=paste("timeseries",k,".vp",sep=''),
				layout.pos.row=k*3-1, 
				xscale=xscale, yscale=myYScale, clip="on"))
		} else {
			# navigate down to where timeseries number k was plotted
			downViewport(paste("timeseries",k,".vp",sep=''))
			if (newScale==T) {
				# push a new viewport to change scales
				pushViewport(viewport(
					name=paste("timeseries",k,"/",superPos,".vp",sep=''),
					xscale=xscale, yscale=myYScale, clip="on"))
			}
		}
		grid.timeseries.steps(blob.list[[k]], logScale=logScale,
			gp=gp, name=paste("timeseries",k,sep=''))
		# draw frame and axes
		if (superPos == 1) {
			pushViewport(viewport(xscale=xscale, yscale=myYScale, 
				clip="off"))
			grid.rect()
			if (logScale) {
				grid.yaxis.log(name="timeseries.yaxis")
				grid.yaxis.log(main=F, label=F)
			} else {
				grid.yaxis(name="timeseries.yaxis")
				grid.yaxis(main=F, label=F)
			}
			# draw label number k
			grid.text(ylabs[k], x=-1*yAxesSpace-unit(1,"lines"), 
				rot=90, name=paste("label",k,sep=''))
			# draw timeline bar and x-axis
			if (qualTimeline) {
				pushViewport(viewport(y=0, height=barThickness, 
					just="top", xscale=xscale, clip="off"))
				if (nBlobs <= 4) { grid.xaxis.POSIXt(label=F) }
				grid.lines(y=0)
				pushViewport(viewport(xscale=xscale, clip="on"))
				grid.timeline.bar(blob.list[[k]], colMap=colMap,
					name=paste("timeline.bar",k,sep=''))
				upViewport(2)
			} else {
				if (nBlobs <= 4) { grid.xaxis.POSIXt(label=F) }
			}
		}
		if ((superPos != 1) && newScale) {
			pushViewport(viewport(x=-1*yAxisWidth*(superPos-1), 
				just="left", yscale=myYScale, clip="off", gp=gp))
			do.call(ifelse(logScale,'grid.yaxis.log','grid.yaxis'),
				list(
				name=paste("timeseries.yaxis.",superPos,sep=''),
				edits=gEdit(gPath="major", y=unit(c(0,1),"npc"))
				))
		}
		# come back up
		if (superPos == 1) { upViewport() } # axes
		if ((superPos != 1) && newScale) { upViewport(2) } # axes and scale
		upViewport() # from timeseriesk.vp
	}
	# come back up
	upViewport(2)
	# draw titles
	if (superPos == 1) {
		if (!is.null(main) && !identical(main, F)) {
			pushViewport(viewport(name="main.vp", layout.pos.row=2))
			grid.draw(main)
			upViewport()
		}
		if (!is.null(sub) && !identical(sub, F)) {
			pushViewport(viewport(name="sub.vp", layout.pos.row=5))
			grid.draw(sub)
			upViewport()
		}
		if (!is.null(theKey)) {
			pushViewport(viewport(name="key.vp", layout.pos.row=3))
			grid.draw(theKey)
			upViewport()
		}
	}
	upViewport(1)
}


grid.timeseries.steps <- function(blob, logScale=F, name="timeseries", gp=NULL, vp=NULL) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	if (nrow(blob)==0) { return() }
	# draw as steps (note: plot type="s" fails to draw horiz line preceding NA points)
	iDates <- seq(1, nrow(blob))
	iVals <- iDates
	iDates <- c(rep(iDates,each=2)[-1], NA)
	iVals <- rep(iVals,each=2)
	myData <- blob$Data
	if (logScale) {
		myData <- log10(myData)
		# set log(0)==-Inf to a finite value off bottom of plot
		yscale <- as.numeric(convertY(unit(c(0,1), "npc"), "native"))
		myData[na.omit(blob$Data==0)] <- yscale[1] - diff(yscale)
	}
	grid.lines(x=blob$Time[iDates], y=myData[iVals],
		default.units="native", name=name, gp=gp, vp=vp)
}


hydrosanity.caption <- function(timelim, by, n, series=NA, x=unit(1,"npc")-unit(1,"mm"), y=unit(1,"mm"), just=c("right","bottom"), gp=gpar(fontsize=7, col=grey(0.5))) {
	timelim <- as.POSIXct(timelim)
	if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
	if (!identical(by, "irregular") && !is.na(series)) {
		allN <- (length(seq(timelim[1], timelim[2], by=by))-1) * series
		pctN <- round(100*(n/allN), digits=0)
		if (pctN < 100) {
			n <- sprintf('%s=%s%%', n, pctN)
		}
	}
	vRSimple <- paste(R.version$major, R.version$minor, sep='.')
	seriesPrefix <- if (!is.na(series) && (series > 1)) { series } else { '' }
	textGrob(sprintf(
		"Data: %s to %s by %s (%sN=%s). Hydrosanity %s, R %s",
		timelim[1], timelim[2], by, seriesPrefix, n, VERSION, vRSimple),
		x=x, y=y, just=just, gp=gp, name="hydrosanity.caption")
}


# returns factor with levels as colours (i.e. colour name/spec strings)
# any unspecified levels (or NAs in qualityCodes) gets first colour in colMap
applyColourMap <- function(qualityCodes, colMap) {
	# check types
	if (!is.factor(qualityCodes)) { 
		warning(paste("'qualityCodes' must be a factor (skipping)"))
		return(rep(factor("black"), length(qualityCodes)))
	}
	if (!is.list(colMap)) { stop("'colMap' must be a list") }
	if (length(intersect(names(colMap),levels(qualityCodes)))==0) {
		warning(paste("levels of 'qualityCodes' do not match 'colMap' (skipping)"))
		return(rep(factor("black"), length(qualityCodes)))
	}
	# need to invert colMap list
	colLevelMap <- as.list(names(colMap))
	names(colLevelMap) <- unlist(colMap)
	# do the conversion into colours
	thisCol <- qualityCodes
	levels(thisCol) <- colLevelMap
	# handle any unspecified levels (or NAs in qualityCodes)
	thisCol[is.na(thisCol)] <- colMap[[1]]
	return(thisCol)
}

monthNames <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

### AXIS STUFF ###

# logLim should be given in log10 scale
logAxisComponents <- function(logLim, label=T) {
	if (length(logLim) != 2) { stop("'logLim' must be of length 2") }
	lim <- logLim
	# integer log powers should always be labelled
	at <- seq(ceiling(min(lim)), max(lim))
	hasLabel <- rep(T, length(at))
	mags <- seq(floor(min(lim)), max(lim))
	if (diff(range(lim)) < 3) {
		# make it a linear sequence log-transformed (gradient effect)
		newAt <- log10(
			sequence(rep(9,length(mags))) * 
				rep(10^mags, each=9) )
		newAt <- newAt[(min(lim) <= newAt) & (newAt <= max(lim))]
		# draw labels for the previous sequence (integer log powers)
		hasLabel <- rep(F, length(newAt))
		hasLabel[match(at, newAt)] <- T
		at <- newAt
	}
	if ((diff(range(lim)) < 1.1)) {
		# label all ticks
		hasLabel[] <- T
	}
	if ((diff(range(lim)) < 0.5)) {
		# go into more detail: ticks between sub-orders of magnitude
		newAt <- log10(
			0.1*(9+sequence(rep(90,length(mags)))) * 
				rep(10^mags, each=90) )
		newAt <- newAt[(min(lim) <= newAt) & (newAt <= max(lim))]
		# keep labels only for the previous sequence
		hasLabel <- rep(F, length(newAt))
		hasLabel[match(at, newAt)] <- T
		at <- newAt
	}
	if ((diff(range(lim)) < 0.35)) {
		# label all ticks
		hasLabel[] <- T
	}
	if ((diff(range(lim)) < 0.05)) {
		# go into more detail: ticks between sub-sub-orders of magnitude
		newAt <- log10(
			0.01*(99+sequence(rep(900,length(mags)))) * 
				rep(10^mags, each=900) )
		newAt <- newAt[(min(lim) <= newAt) & (newAt <= max(lim))]
		# keep labels only for the previous sequence
		hasLabel <- rep(F, length(newAt))
		hasLabel[match(at, newAt)] <- T
		at <- newAt
	}
	if ((diff(range(lim)) < 0.035)) {
		# label all ticks
		hasLabel[] <- T
	}
	makeLabels <- function(x) {
		# use sapply rather than format directly, so not common format
		newLabels <- sapply(10^x, format, digits=2, scientific=1)
		# large powers of 10 are better presented with an exponent
		simplePowers <- (x >= 5) & (x == round(x))
		newLabels[simplePowers] <- paste("10^",x[simplePowers],sep='')
		newLabels
	}
	# blank labels
	atLabels <- rep("", length(at))
	if (label) {
		atLabels[hasLabel] <- makeLabels(at[hasLabel])
	}
	return(list(at=at, label=atLabels))
}

grid.yaxis.log <- function(logLim=as.numeric(convertY(unit(c(0,1), "npc"), "native")), label=T, draw=T, name=NULL, ...) {
	axisStuff <- logAxisComponents(logLim, label=label)
	if (label==F) { axisStuff$label <- F }
	tmp <- yaxisGrob(at=axisStuff$at, label=axisStuff$label, name=name, ...)
	if (label) {
		tmp <- editGrob(tmp, gPath("labels"), check.overlap=F)
	}
	if (draw) { grid.draw(tmp) }
	tmp
}

grid.xaxis.log <- function(logLim=as.numeric(convertX(unit(c(0,1), "npc"), "native")), label=T, draw=T, name=NULL, ...) {
	axisStuff <- logAxisComponents(logLim, label=label)
	if (label==F) { axisStuff$label <- F }
	tmp <- xaxisGrob(at=axisStuff$at, label=axisStuff$label, name=name, ...)
	if (label) {
		tmp <- editGrob(tmp, gPath("labels"), check.overlap=F)
	}
	if (draw) { grid.draw(tmp) }
	tmp
}

lattice.y.sqrt <- function(lim, ...) {
	arglist <- list(...)
	tmp <- yscale.components.default(lim, ...)
	##tmp$left$labels$labels <- format(tmp$left$labels$at ^ 2)
	tmp$left$ticks$at <- sqrt(pretty(pmax(0,tmp$num.limit) ^ 2))
	tmp$left$labels$at <- tmp$left$ticks$at
	tmp$left$labels$labels <- format(tmp$left$labels$at ^ 2)
	return(tmp)
}

lattice.x.sqrt <- function(lim, ...) {
	arglist <- list(...)
	tmp <- xscale.components.default(lim, ...)
	##tmp$bottom$labels$labels <- format(tmp$bottom$labels$at ^ 2)
	tmp$bottom$ticks$at <- sqrt(pretty(pmax(0,tmp$num.limit) ^ 2))
	tmp$bottom$labels$at <- tmp$bottom$ticks$at
	tmp$bottom$labels$labels <- format(tmp$bottom$labels$at ^ 2)
	return(tmp)
}


lattice.y.prettylog <- function(lim, ...) {
	arglist <- list(...)
	have.log <- (!is.null(arglist$logsc)) && (!identical(arglist$logsc, F))
	tmp <- yscale.components.default(lim, ...)
	if (have.log) {
		axisStuff <- logAxisComponents(lim)
		tmp$left$ticks$at <- axisStuff$at
		tmp$left$labels$at <- axisStuff$at
		tmp$left$labels$labels <- axisStuff$label
		tmp$left$labels$check.overlap <- F
	}
	return(tmp)
}

lattice.x.prettylog <- function(lim, ...) {
	arglist <- list(...)
	have.log <- (!is.null(arglist$logsc)) && (!identical(arglist$logsc, F))
	tmp <- xscale.components.default(lim, ...)
	if (have.log) {
		axisStuff <- logAxisComponents(lim)
		tmp$bottom$ticks$at <- axisStuff$at
		tmp$bottom$labels$at <- axisStuff$at
		tmp$bottom$labels$labels <- axisStuff$label
		tmp$bottom$labels$check.overlap <- F
	}
	return(tmp)
}

# lim should be POSIXct or numeric (see as.numeric.POSIXct)
timeAxisComponents <- function(lim, label=T, tz="GMT") {
	if (length(lim) != 2) { stop("'lim' must be of length 2") }
	if (is.numeric(lim)) {
		class(lim) <- c("POSIXt", "POSIXct")
		attr(lim, "tzone") <- tz
	}
	timelim <- as.POSIXct(lim)
	lim <- as.numeric(timelim)
	startTime <- min(timelim)
	# utility functions for making pretty times
	truncMonth <- function(thisPOSIXt) {
		zz <- as.POSIXlt(thisPOSIXt)
		zz$mday <- 1
		zz$hour <- zz$min <- zz$sec <- 0
		zz$isdst <- -1
		zz
	}
	truncYear <- function(thisPOSIXt) {
		zz <- as.POSIXlt(thisPOSIXt)
		zz$mday <- 1
		zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
		zz$isdst <- -1
		zz
	}
	truncDecade <- function(thisPOSIXt) {
		zz <- as.POSIXlt(truncYear(thisPOSIXt))
		zz$year <- (zz$year %/% 10) * 10
		zz
	}
	trunc.century <- function(thisPOSIXt) {
		zz <- as.POSIXlt(truncYear(thisPOSIXt))
		zz$year <- (zz$year %/% 100) * 100
		zz
	}
	# work out time sequence and formatting, depending on the time scale
	# each of tickSpec and labelSpec defines a time sequence and formatting
	tickSpec <- list(by="1 hour", format="%m-%d %H:%M", 
		from=trunc(startTime, units="hours"))
	labelSpec <- tickSpec
	if (diff(range(lim)) > 8 * 60*60) { # 8 hours
		labelSpec$by <- "3 hours"
		labelSpec$from <- trunc(startTime, units="days")
	}
	if (diff(range(lim)) > 24 * 60*60) { # 24 hours
		tickSpec <- labelSpec
		labelSpec$by <- "12 hours"
	}
	if (diff(range(lim)) > 2 * 24*60*60) { # 2 days
		tickSpec <- labelSpec
		labelSpec$by <- "1 DSTday"
		labelSpec$format <- "%Y-%m-%d"
	}
	if (diff(range(lim)) > 3 * 24*60*60) { # 3 days
		# only put ticks at day labels (no sub-ticks)
		tickSpec <- labelSpec
	}
	if (diff(range(lim)) > 7 * 24*60*60) { # 7 days
		tickSpec <- labelSpec
		labelSpec$by <- "3 DSTdays"
		# ignore up to 3 days of previous month to find start date
		labelSpec$from <- truncMonth(startTime + 4*24*60*60)
	}
	if (diff(range(lim)) > 18 * 24*60*60) { # 18 days
		labelSpec$by <- "7 DSTdays"
		# ignore up to 7 days of previous month to find start date
		labelSpec$from <- truncMonth(startTime + 7*24*60*60)
		# leave ticks going by 1 day
	}
	if (diff(range(lim)) > 1.1 * 30*24*60*60) { # 1 month
		labelSpec$by <- "1 month"
		# leave ticks going by 1 day
	}
	if (diff(range(lim)) > 1.85 * 30*24*60*60) { # 2 months
		labelSpec$format <- "%Y-%m"
		# only put ticks at month labels (no sub-ticks)
		tickSpec <- labelSpec
	}
	if (diff(range(lim)) > 6 * 30*24*60*60) { # 6 months
		tickSpec <- labelSpec
		labelSpec$by <- "3 months"
		labelSpec$format <- "%Y-%b"
		labelSpec$from <- truncYear(startTime)
	}
	if (diff(range(lim)) > 2 * 365.25*24*60*60) { # 2 years
		tickSpec <- labelSpec
		labelSpec$by <- "1 year"
		labelSpec$format <- "%Y"
	}
	if (diff(range(lim)) > 3.5 * 365.25*24*60*60) { # 3.5 years
		# only put ticks at year labels (no sub-ticks)
		tickSpec <- labelSpec
	}
	if (diff(range(lim)) > 8 * 365.25*24*60*60) { # 8 years
		tickSpec <- labelSpec
		labelSpec$by <- "2 years"
		labelSpec$from <- truncDecade(startTime)
	}
	if (diff(range(lim)) > 12 * 365.25*24*60*60) { # 12 years
		labelSpec$by <- "5 years"
		# leave ticks going by 1 year
	}
	if (diff(range(lim)) > 30 * 365.25*24*60*60) { # 30 years
		tickSpec <- labelSpec
		labelSpec$by <- "10 years"
	}
	if (diff(range(lim)) > 60 * 365.25*24*60*60) { # 60 years
		# drop 5-year ticks
		tickSpec <- labelSpec
	}
	if (diff(range(lim)) > 100 * 365.25*24*60*60) { # 100 years
		labelSpec$by <- "20 years"
		labelSpec$from <- trunc.century(startTime)
	}
	# make sequence of axis ticks
	at <- seq(tickSpec$from, max(timelim), by=tickSpec$by)
	at <- at[(min(timelim) <= at) & (at <= max(timelim))]
	# blank labels
	atLabels <- rep("", length(at))
	if (label) {
		labelAt <- seq(labelSpec$from, max(timelim), by=labelSpec$by)
		labelIdx <- c(na.omit(match(as.numeric(labelAt), as.numeric(at))))
		atLabels[labelIdx] <- format(at[labelIdx], labelSpec$format)
	}
	return(list(at=at, label=atLabels))
}

grid.xaxis.POSIXt <- function(lim=as.numeric(convertX(unit(c(0,1), "npc"), "native")), label=T, draw=T, name=NULL, ...) {
	axisStuff <- timeAxisComponents(lim, label=label)
	if (label==F) { axisStuff$label <- F }
	tmp <- xaxisGrob(at=axisStuff$at, label=axisStuff$label, name=name, ...)
	if (label) {
		tmp <- editGrob(tmp, gPath=gPath("labels"), check.overlap=F)
	}
	if (draw) { grid.draw(tmp) }
	tmp
}

# prepanel.default.qqmath fails to take range(x, finite=T)
prepanel.qqmath.fix <- function(x, ...) {
	tmp <- lattice:::prepanel.default.qqmath(x, ...)
	tmp$ylim <- range(x, finite=T)
	tmp
}

## this function by Deepayan Sarkar, posted on R-help mailing list
myYlabGrob <-
   function(..., main.ylab = "") ## ...is lab1, lab2, etc
{
   ## you can add arguments to textGrob for more control
   ## in the next line
   labs <- lapply(list(...), textGrob, rot=90)
   main.ylab <- textGrob(main.ylab, rot = 90)
   nlabs <- length(labs)
   lab.heights <-
       lapply(labs,
              function(lab) unit(1, "grobheight",
                                 data=list(lab)))
   unit1 <- unit(1.2, "grobheight", data = list(main.ylab))
   unit2 <- do.call(max, lab.heights)
   lab.layout <-
       grid.layout(ncol = 2, nrow = nlabs,
                   heights = unit(1, "null"),
                   widths = unit.c(unit1, unit2),
                   respect = TRUE)
   lab.gf <- frameGrob(layout=lab.layout)
   for (i in seq_len(nlabs))
   {
       lab.gf <- placeGrob(lab.gf, labs[[i]], row = i, col = 2)
   }
   lab.gf <- placeGrob(lab.gf, main.ylab, col = 1)
   lab.gf
}

