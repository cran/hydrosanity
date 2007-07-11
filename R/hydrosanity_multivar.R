## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateMultivarPage <- function() {
	
	role <- sapply(hsp$data, attr, "role")
	
	flow_combo <- theWidget("multivar_flowblob_combobox")
	oldSel <- flow_combo$getActive()
	flow_combo$getModel()$clear()
	for (x in names(hsp$data)[role=="FLOW"]) {
		flow_combo$appendText(x)
	}
	if (oldSel == -1) { oldSel <- 0 }
	flow_combo$setActive(oldSel)
	
	StateEnv$update$multivar <- F
	StateEnv$win$present()
}

.hs_on_multivar_relationplot_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(selNames)
	lagSpec <- theWidget("multivar_relationplot_lag_comboboxentry")$getActiveText()
	doRises <- theWidget("multivar_relationplot_flowrises_checkbutton")$getActive()
	doSmooth <- theWidget("multivar_smooth_checkbutton")$getActive()
	smoothSpan <- theWidget("multivar_smooth_span_spinbutton")$getValue()
	doRawData <- theWidget("multivar_relationplot_rawdata_radiobutton")$getActive()
	doAggr1 <- theWidget("multivar_relationplot_aggr1_radiobutton")$getActive()
	doAggr2 <- theWidget("multivar_relationplot_aggr2_radiobutton")$getActive()
	aggr1By <- theWidget("multivar_relationplot_aggr1_comboboxentry")$getActiveText()
	aggr2By <- theWidget("multivar_relationplot_aggr2_comboboxentry")$getActiveText()
	flowName <- theWidget("multivar_flowblob_combobox")$getActiveText()
	if (is.null(flowName)) {
		errorDialog("No flow item was selected.")
		return()
	}
	selNames <- c(flowName, selNames)
	
	addLogComment("Generate multivariate rainfall-runoff relationship plot")
	
	tmpObjs <- c('tmp.data')
	
	guiDo(call=bquote(
		tmp.data <- lapply(hsp$data[.(selNames)], window, hsp$timePeriod[1], hsp$timePeriod[2])
	))
	
	if (doRises) {
		guiDo(tmp.data[[1]]$Data <- rises(tmp.data[[1]]$Data))
	}
	
	# compute and store aggregated series
	if (doAggr1 || doAggr2) {
		aggrBy <- if (doAggr1) { aggr1By } else { aggr2By }
		aggr.call <- bquote(
			tmp.data <- lapply(tmp.data, aggregate.timeblob, by=.(aggrBy))
		)
		if (any(grep("( month|year)", aggrBy))) {
			aggr.call[[3]]$start.month <- hsp$startMonth
		}
		guiDo(call=aggr.call)
	}
	
	guiDo(tmp.data <- syncTo.timeblobs(tmp.data, blob=tmp.data[[1]]))
	
	plot.call <- call('xyplot')
	plot.call[[2]] <- as.formula(
		paste(make.names(flowName), "~",  paste(make.names(selNames[-1]), collapse=" + "))
	)
	plot.call[[3]] <- quote(tmp.data)
	plot.call$outer <- T
	if (doSmooth) {
		plot.call$type <- c("p", "smooth")
		plot.call$span <- smoothSpan
	}
	plot.call$xscale.components <- quote(lattice.x.prettylog)
	plot.call$yscale.components <- quote(lattice.y.prettylog)
	
	# hydrosanity caption
	addToLog("## Make hydrosanity caption")
	tmpObjs <- c(tmpObjs, 'tmp.n', 'tmp.caption')
	guiDo(call=bquote({
		tmp.n <- sum(unlist(lapply(tmp.data[-1], function(x) !is.na(x) & !is.na(tmp.data[1]))))
		tmp.caption <- hydrosanity.caption(
			timelim.timeblobs(tmp.data),
			by=.(attr(tmp.data, "timestep")), n=tmp.n, series=.(ncol(tmp.data)-1))
	}))
	plot.call$sub <- quote(tmp.caption)
	
	idLabels <- format(tmp.data$Time, 
		timestepTimeFormat(attr(tmp.data, "timestep")))
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(plotAndPlay(plot.call=plot.call, name="rainfall-runoff", 
		extra.buttons=plotAndPlayButtons[c('zero', 'logscale')],
		trans.scales=c("x","y"),
		labels=idLabels, eval.args="^tmp",
		restore.on.close=StateEnv$win), doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated multivariate rainfall-runoff relationship plot")
}


