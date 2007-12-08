## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateExplorePage <- function() {
	StateEnv$update$explore <- F
	StateEnv$win$present()
}

.hs_on_explore_timeseries_button_clicked <- function(button) {
	freezeGUI(echo.to.log=F)
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(selNames)
	doCommonScale <- theWidget("explore_timeseries_commonscale_radiobutton")$getActive()
	doSuperpose <- theWidget("explore_timeseries_superpose_radiobutton")$getActive()
	doQual <- theWidget("explore_timeseries_qual_checkbutton")$getActive()
	doRawData <- theWidget("explore_timeseries_rawdata_checkbutton")$getActive()
	doAggr1 <- theWidget("explore_timeseries_aggr1_checkbutton")$getActive()
	doAggr2 <- theWidget("explore_timeseries_aggr2_checkbutton")$getActive()
	aggr1By <- theWidget("explore_timeseries_aggr1_comboboxentry")$getActiveText()
	aggr2By <- theWidget("explore_timeseries_aggr2_comboboxentry")$getActiveText()
	doSmooth <- theWidget("explore_timeseries_smooth_checkbutton")$getActive()
	nTrans <- (doRawData + doAggr1 + doAggr2)
	if (nTrans == 0) { return() }
	if (nBlobs * nTrans == 1) { doSuperpose <- F }
	
	addLogComment("Generate timeseries plot")
	
	tmpObjs <- c()
	
	rawdata.call <- if (nBlobs == length(hsp$data)) {
		quote(hsp$data)
	} else {
		bquote(hsp$data[.(selNames)])
	}
	
	# note: we cannot apply time series window to the data here;
	#       need to keep whole dataset to allow interaction (zoom out, etc)
	
	# compute and store aggregated series
	if (doAggr1) {
		tmpObjs <- c(tmpObjs, 'tmp.aggr1')
		aggr.call <- bquote(
			tmp.aggr1 <- lapply(.(rawdata.call), aggregate.timeblob,
				by=.(aggr1By)))
		if (any(grep("( month|year)", aggr1By))) {
			aggr.call[[3]]$start.month <- hsp$yearStart
		}
		if (doSmooth) {
			aggr.call <- bquote(
				tmp.aggr1 <- lapply(.(rawdata.call), 
					smooth.timeblob, by=.(aggr1By)))
		}
		guiDo(call=aggr.call)
	}
	if (doAggr2) {
		tmpObjs <- c(tmpObjs, 'tmp.aggr2')
		aggr.call <- bquote(
			tmp.aggr2 <- lapply(.(rawdata.call), aggregate.timeblob,
				by=.(aggr2By)))
		if (any(grep("( month|year)", aggr2By))) {
			aggr.call[[3]]$start.month <- hsp$yearStart
		}
		if (doSmooth) {
			aggr.call <- bquote(
				tmp.aggr2 <- lapply(.(rawdata.call), 
					smooth.timeblob, by=.(aggr2By)))
		}
		guiDo(call=aggr.call)
	}
	
	# each item in list.call makes a timeseries plot, each superposed.
	# (so each item should specify a list of timeblobs)
	list.call <- call('list')
	if (doSuperpose) {
		# superpose blobs and aggregates all in one panel
		bigList <- list()
		if (doRawData) {
			bigList <- c(bigList, 
				lapply(selNames, function(name) {
					bquote(hsp$data[.(name)])
				})
			)
		}
		if (doAggr1) {
			if (nBlobs == 1) { # simplify code if only one
				bigList <- c(bigList, quote(tmp.aggr1))
			} else {
				bigList <- c(bigList, 
					lapply(selNames, function(name) {
						bquote(tmp.aggr1[.(name)])
					})
				)
			}
		}
		if (doAggr2) {
			if (nBlobs == 1) { # simplify code if only one
				bigList <- c(bigList, quote(tmp.aggr2))
			} else {
				bigList <- c(bigList, 
					lapply(selNames, function(name) {
						bquote(tmp.aggr2[.(name)])
					})
				)
			}
		}
		list.call <- as.call(c(quote(list), bigList))
	} else {
		# do multiple plots
		if (nBlobs == 1) {
			# multiple panels for transforms (all in one layer)
			if (nTrans == 1) {
				list.call[[2]] <- if (doRawData) { rawdata.call } else
					if (doAggr1) { quote(tmp.aggr1) } else
					if (doAggr2) { quote(tmp.aggr2) }
			} else {
				list.call[[2]] <- as.call(c(quote(c),
					if (doRawData) { rawdata.call },
					if (doAggr1) { quote(tmp.aggr1) },
					if (doAggr2) { quote(tmp.aggr2) }
				))
			}
		} else {
			# multiple panels for blobs, but superpose transforms
			if (doRawData) {
				list.call[[2]] <- rawdata.call
			}
			if (doAggr1) {
				i <- length(list.call)+1
				list.call[[i]] <- quote(tmp.aggr1)
			}
			if (doAggr2) {
				i <- length(list.call)+1
				list.call[[i]] <- quote(tmp.aggr2)
			}
		}
	}
	
	# plot specifications
	if (length(list.call[-1]) == 1) {
		plot.call <- call('grid.timeseries.plot')
		plot.call[[2]] <- list.call[[2]]
	} else {
		plot.call <- call('grid.timeseries.plot.superpose')
		plot.call[[2]] <- list.call
	}
	
	# plot scales and annotation specifications
	plot.call$xlim <- quote(hsp$timePeriod)
	plot.call$sameScales <- if (doCommonScale) { T } else { F }
	plot.call$allSameScales <- if (doCommonScale && doSuperpose #&& !doSmooth
		&& (length(list.call[-1]) > 1)) { T }
	plot.call$qualTimeline <- if (doQual) {
		if (doSuperpose && (nBlobs > 1)) { F } else { T }
	}
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	playwith(plot.call=plot.call, title="timeseries", 
		viewport="time.vp", time.mode=TRUE,
		# TODO: log scale button
		bottom=list(setPeriodTool),
		eval.args="^hsp$", invert.match=T, on.close=restoreHS)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	setStatusBar("Generated timeseries plot")
}

.hs_on_explore_cdf_button_clicked <- function(button) {
	freezeGUI(echo.to.log=F)
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(selNames)
	doNormal <- theWidget("explore_cdf_normal_radiobutton")$getActive()
	doUniform <- theWidget("explore_cdf_uniform_radiobutton")$getActive()
	doCDF <- doNormal || doUniform
	doBoxPlot <- theWidget("explore_cdf_bwplot_radiobutton")$getActive()
	doStripPlot <- theWidget("explore_cdf_stripplot_radiobutton")$getActive()
	doViolinPlot <- theWidget("explore_cdf_violinplot_radiobutton")$getActive()
	doRawData <- theWidget("explore_cdf_rawdata_radiobutton")$getActive()
	doAggr1 <- theWidget("explore_cdf_aggr1_radiobutton")$getActive()
	doAggr2 <- theWidget("explore_cdf_aggr2_radiobutton")$getActive()
	aggr1By <- theWidget("explore_cdf_aggr1_comboboxentry")$getActiveText()
	aggr2By <- theWidget("explore_cdf_aggr2_comboboxentry")$getActiveText()
	
	addLogComment("Generate distribution plot")
	
	tmpObjs <- c('tmp.data')
	
	# initalise data
	if (nBlobs == length(hsp$data)) {
		guiDo(tmp.data <- hsp$data)
	} else {
		guiDo(call=bquote(
			tmp.data <- hsp$data[.(selNames)]
		))
	}
	
	# apply time period window
	if (!is.null(hsp$timePeriod)) {
		guiDo(tmp.data <- lapply(tmp.data, window, 
			hsp$timePeriod[1], hsp$timePeriod[2]))
	}
	
	# compute and store aggregated series
	if (doAggr1 || doAggr2) {
		aggrBy <- if (doAggr1) aggr1By else aggr2By
		aggr.call <- bquote(
			tmp.data <- lapply(tmp.data, aggregate.timeblob, by=.(aggrBy))
		)
		if (any(grep("( month|year)", aggrBy))) {
			aggr.call[[3]]$start.month <- hsp$yearStart
		}
		guiDo(call=aggr.call)
	}
	
	# make.groups
	tmpObjs <- c(tmpObjs, "tmp.groups")
	guiDo(string=sprintf(
		"tmp.groups <- make.groups(%s)",
		paste(sep="", collapse=", ",
			make.names(names(tmp.data)), 
			'=tmp.data[[', shQuote(names(tmp.data)), ']]$Data')
	))
	#guiDo(tmp.groups <- do.call(make.groups, 
	#	lapply(tmp.data, function(x) x$Data )))
	
	# plot specifications
	plotFn <- if (doCDF) 'qqmath' else 
		if (doStripPlot) 'stripplot' else 'bwplot'
	plot.call <- call(plotFn)
	plot.call[[2]] <- if (doCDF) quote(~ data) else quote(data ~ which)
	plot.call$groups <- if (doCDF) quote(which)
	plot.call$data <- quote(tmp.groups)
	plot.call$distribution <- if (doNormal) quote(qnorm) else
		if (doUniform) quote(qunif)
	plot.call$panel <- if (doCDF) {
			function(x, ...) {
				if (FALSE) panel.qqmathline(x[is.finite(x)], ...)
				panel.qqmath(x, ...)
			}
		} else if (doViolinPlot) {
			function(...) {
				panel.violin(varwidth=T, ...)
				panel.stripplot(pch=3, ...)
			}
		}
	plot.call$jitter <- if (doStripPlot) { T }
	
	# plot scales and annotation specifications
	if (doCDF) {
		tmpObjs <- c(tmpObjs, 'tmp.probs')
		plot.call$scales <- quote(list())
		if (doNormal) {
			guiDo(tmp.probs <- c(0.0001, 0.001, 0.01, 0.1, 0.25, 0.5, 
				0.75, 0.9, 0.99, 0.999, 0.9999))
			plot.call$scales$x <- quote(list(
				at=qnorm(1-tmp.probs), labels=tmp.probs * 100))
			plot.call$xlim <- quote(rev(extendrange(qnorm(tmp.probs))))
		} else {
			guiDo(tmp.probs <- seq(0, 1, by=0.1))
			plot.call$scales$x <- quote(list(
				at=(1-tmp.probs), labels=tmp.probs * 100))
			plot.call$xlim <- quote(rev(extendrange(tmp.probs)))
		}
	}
	plot.call$yscale.components <- quote(lattice.y.prettylog)
	plot.call$xlab <- if (doCDF) { "Probability (%)" }
	plot.call$ylab <- attr(hsp$data[[selNames[1]]], "dataname")
	plot.call$auto.key <- T
	
	# hydrosanity caption
	addToLog("## Make hydrosanity caption")
	tmpObjs <- c(tmpObjs, 'tmp.n', 'tmp.caption')
	guiDo(call=bquote({
		tmp.n <- sum(!unlist(lapply.timeblob.data(tmp.data, is.na)))
		tmp.caption <- hydrosanity.caption(
			timelim.timeblobs(tmp.data), 
			by=.(attr(tmp.data[[1]], "timestep")), n=tmp.n, 
			series=.(nBlobs))
	}))
	plot.call$sub <- quote(tmp.caption)
	plot.call$prepanel <- if (doCDF) { quote(prepanel.qqmath.fix) }
	
	idLabels <- unlist(lapply(tmp.data, function(x) {
		format(x$Time, timestepTimeFormat(attr(x, "timestep")))
	}))
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	playwith(plot.call=plot.call, title="distribution", 
		labels=idLabels, 
		eval.args="^hsp$", invert.match=T, on.close=restoreHS)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	setStatusBar("Generated distribution plot")
}

.hs_on_explore_seasonal_button_clicked <- function(button) {
	freezeGUI(echo.to.log=F)
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(selNames)
	doMonths <- theWidget("explore_seasonal_months_radiobutton")$getActive()
	doBoxPlot <- theWidget("explore_seasonal_bwplot_radiobutton")$getActive()
	doStripPlot <- theWidget("explore_seasonal_stripplot_radiobutton")$getActive()
	doViolinPlot <- theWidget("explore_seasonal_violinplot_radiobutton")$getActive()
	doSupStripPlot <- theWidget("explore_seasonal_supstripplot_radiobutton")$getActive()
	doDrawLine <- theWidget("explore_seasonal_drawline_radiobutton")$getActive()
	if (doSupStripPlot && (nBlobs == 1)) {
		doSupStripPlot <- F
		doStripPlot <- T
	}
	
	addLogComment("Generate seasonal plot")
	
	tmpObjs <- c('tmp.data')
	
	# initalise data
	if (nBlobs == length(hsp$data)) {
		guiDo(tmp.data <- hsp$data)
	} else {
		guiDo(call=bquote(
			tmp.data <- hsp$data[.(selNames)]
		))
	}
	
	if (doMonths) {
		guiDo({
			tmp.data <- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, 
				by="months"), timelim=hsp$timePeriod)
			tmp.data$Season <- factor(months(tmp.data$Time, abbreviate=TRUE),
				levels=c("Jan","Feb","Mar","Apr","May","Jun",
				"Jul","Aug","Sep","Oct","Nov","Dec"), ordered=T)
		})
	} else {
		guiDo({
			tmp.data <- sync.timeblobs(lapply(tmp.data, aggregate.timeblob, 
				by="3 months"), timelim=hsp$timePeriod)
			tmp.data$Season <- factor(quarters(tmp.data$Time), ordered=T)
		})
	}
	
	# plot specifications
	plotFn <- if (doStripPlot || doSupStripPlot) {
		'stripplot'
	} else {
		'bwplot'
	}
	plot.call <- call(plotFn)
	plot.call[[2]] <- as.formula(
		paste(paste(make.names(selNames), collapse=" + "), "~ Season")
	)
	plot.call[[3]] <- quote(tmp.data)
	plot.call$outer <- if (!doSupStripPlot && (nBlobs > 1)) { T }
	plot.call$panel <- if (doDrawLine && !doSupStripPlot) {
		if (doStripPlot) {
			function(...) {
				panel.linejoin(..., fun=function(x)mean(x, na.rm=T))
				panel.stripplot(...)
			}
		} else if (doViolinPlot) {
			function(...) {
				panel.linejoin(..., fun=function(x)mean(x, na.rm=T))
				panel.violin(varwidth=T, ...)
				panel.stripplot(pch=3, ...)
			}
		} else {
			function(...) {
				panel.linejoin(..., fun=function(x)mean(x, na.rm=T))
				panel.bwplot(...)
			}
		}
	} else if (doDrawLine && doSupStripPlot) {
		quote(panel.superpose)
	} else if (doViolinPlot) {
		function(...) {
			panel.violin(varwidth=T, ...)
			panel.stripplot(pch=3, ...)
		}
	}
	plot.call$panel.groups <- if (doDrawLine && doSupStripPlot) {
		function(...) {
			panel.linejoin(..., fun=function(x){mean(x, na.rm=T)})
			panel.stripplot(...)
		}
	}
	plot.call$layout <- if (!doSupStripPlot && doMonths && (nBlobs > 1)) {
		c(1, nBlobs)
	}
	#plot.call$jitter <- if (doStripPlot || doSupStripPlot) { T }
	
	# plot scales and annotation specifications
	plot.call$ylab <- attr(hsp$data[[selNames[1]]], "dataname")
	plot.call$yscale.components <- quote(lattice.y.prettylog)
	plot.call$auto.key <- if (doSupStripPlot) { T }
	
	# hydrosanity caption
	addToLog("## Make hydrosanity caption")
	tmpObjs <- c(tmpObjs, 'tmp.n', 'tmp.caption')
	guiDo(call=bquote({
		tmp.n <- sum(sapply(tmp.data[2:.(nBlobs+1)], is.na)==F)
		tmp.caption <- hydrosanity.caption(
			range(tmp.data$Time), # TODO: should add timestep to this
			by=.(attr(tmp.data, "timestep")), n=tmp.n, 
			series=.(nBlobs))
	}))
	plot.call$sub <- quote(tmp.caption)
	
	idLabels <- rep(format(tmp.data$Time, "%Y"), nBlobs)
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	playwith(plot.call=plot.call, title="seasonality", 
		labels=idLabels, 
		eval.args="^hsp$", invert.match=T, on.close=restoreHS)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	setStatusBar("Generated seasonal plot")
}

