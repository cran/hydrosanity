## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateImputePage <- function() {
	StateEnv$update$impute <- F
	StateEnv$win$present()
}

.hs_on_impute_view_error_scatter_button_clicked <- function(button) {
	freezeGUI(use.core.log=F)
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	doRawData <- theWidget("impute_rawdata_radiobutton")$getActive()
	doAggr1 <- theWidget("impute_aggr1_radiobutton")$getActive()
	doAggr2 <- theWidget("impute_aggr2_radiobutton")$getActive()
	aggr1By <- theWidget("impute_aggr1_comboboxentry")$getActiveText()
	aggr2By <- theWidget("impute_aggr2_comboboxentry")$getActiveText()
	
	doByDistance <- theWidget("impute_missing_distance_radiobutton")$getActive()
	doByCorrelation <- theWidget("impute_missing_correlation_radiobutton")$getActive()
	doByConstant <- theWidget("impute_missing_constant_radiobutton")$getActive()
	constTypeIdx <- theWidget("impute_missing_constant_combobox")$getActive() + 1
	constType <- switch(constTypeIdx, "mean", "mean", "zero", "extend")
	doTrim <- (constTypeIdx == 2)
	imputeMethod <- c(
		if (doByDistance) { "distance" },
		if (doByCorrelation) { "correlation" },
		if (doByConstant) { "constant" })
	
	addLogComment("View imputed vs actual values scatterplot")
	
	roles <- sapply(hsp$data, attr, "role")
	if (length(unique(roles[selNames])) > 1) {
		errorDialog("All selected items must have same role (e.g RAIN / FLOW).")
		return()
	}
	
	tmpObjs <- c("tmp.vars", "tmp.predictors")
	sameRole <- (roles == roles[selNames[1]])
	guiDo(call=bquote({
		tmp.vars <- .(selNames)
		tmp.predictors <- .(names(hsp$data)[sameRole])
	}))
	
	impute.call <- call('impute.timeblobs')
	impute.call[[2]] <- quote(hsp$data[tmp.predictors])
	impute.call$which.impute <- quote(tmp.vars)
	impute.call$timelim <- if (!is.null(hsp$timePeriod)) { quote(hsp$timePeriod) }
	impute.call$extend <- T
	impute.call$method <- imputeMethod
	impute.call$constant <- if (doByConstant) { constType }
	impute.call$trim <- if (doTrim) { 0.01 }
	
	impute.assign.call <- quote(tmp.data <- foo)
	impute.assign.call[[3]] <- impute.call
	
	guiDo(call=impute.assign.call)
	
	# compute and store aggregated series
	if (doAggr1 || doAggr2) {
		aggrBy <- if (doAggr1) { aggr1By } else { aggr2By }
		aggr.call <- bquote(
			tmp.data <- lapply(tmp.data, aggregate.timeblob, by=.(aggrBy), fun.qual="omit")
		)
		if (any(grep("( month|year)", aggrBy))) {
			aggr.call[[3]]$start.month <- hsp$startMonth
		}
		guiDo(call=aggr.call)
	}
	
	# prepare data for plot
	guiDo(tmp.groups <- do.call(make.groups, 
		lapply(tmp.data, function(x) x$Data )))
	guiDo(tmp.groups$imputed <- unlist(lapply(tmp.data, function(x) x$Imputed )))

	plot.call <- quote(xyplot(data ~ imputed | which, tmp.groups, aspect="iso",
		panel=function(...) {
			panel.abline(a=0, b=1,
				col.line=trellis.par.get("superpose.line")$col[2],
				lty=trellis.par.get("superpose.line")$lty[2])
			panel.xyplot(...)
		}
	))
	
	idLabels <- unlist(lapply(tmp.data, function(x) {
		format(x$Time, timestepTimeFormat(attr(x, "timestep")))
	}))
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(playwith(plot.call=plot.call, name="imputed vs actual", 
		extra.buttons=list("zero","logscale"),
		trans.scales=c("x","y"), labels=idLabels, 
		eval.args="^hsp$", invert=T, restore.on.close=StateEnv$win), 
		doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	setStatusBar("Generated imputed vs actual values scatterplot")
}

.hs_on_impute_missingatrandom_button_clicked <- function(button) {
	freezeGUI(use.core.log=F)
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	doByDistance <- theWidget("impute_missing_distance_radiobutton")$getActive()
	doByCorrelation <- theWidget("impute_missing_correlation_radiobutton")$getActive()
	doByConstant <- theWidget("impute_missing_constant_radiobutton")$getActive()
	imputeMethod <- c(
		if (doByDistance) { "distance" },
		if (doByCorrelation) { "correlation" },
		if (doByConstant) { "constant" })
	if (doByConstant) {
		errorDialog("This does not make sense with method 'constant'.")
		return()
	}
	
	addLogComment("View distribtions of observed vs missing (imputed in gaps)")
	
	roles <- sapply(hsp$data, attr, "role")
	if (length(unique(roles[selNames])) > 1) {
		errorDialog("All selected items must have same role (e.g RAIN / FLOW).")
		return()
	}
	
	tmpObjs <- c("tmp.vars", "tmp.predictors")
	sameRole <- (roles == roles[selNames[1]])
	guiDo(call=bquote({
		tmp.vars <- .(selNames)
		tmp.predictors <- .(names(hsp$data)[sameRole])
	}))
	
	impute.call <- call('impute.timeblobs')
	impute.call[[2]] <- quote(hsp$data[tmp.predictors])
	impute.call$which.impute <- quote(tmp.vars)
	impute.call$timelim <- if (!is.null(hsp$timePeriod)) { quote(hsp$timePeriod) }
	impute.call$extend <- T
	impute.call$method <- imputeMethod
	
	impute.assign.call <- quote(tmp.data <- foo)
	impute.assign.call[[3]] <- impute.call
	
	guiDo(call=impute.assign.call)
	
	guiDo(tmp.data <- lapply(tmp.data, function(x) {
		x$is.observed <- factor(!is.na(x$Data), labels=c("missing", "not missing"))
		#x$Data[is.na(x$Data)] <- x$Imputed[is.na(x$Data)]
		x
	}))
	
	# prepare data for plot
	guiDo(tmp.groups <- do.call(make.groups, 
		lapply(tmp.data, function(x) x$Imputed )))
	guiDo(tmp.groups$is.observed <- unlist(lapply(tmp.data, function(x) x$is.observed )))
	
	#plot.call <- quote(bwplot(data ~ is.observed | which, tmp.groups))
	plot.call <- quote(qqmath( ~ data | which, tmp.groups, groups=is.observed,
		prepanel=prepanel.qqmath.fix))
	
	addToLog(paste(deparse(plot.call), collapse="\n"))
	guiDo(playwith(plot.call=plot.call, name="distributions of observed vs missing", 
		extra.buttons=list("zero","logscale"),
		nav.scales=c("x", "y"), trans.scales=c("y"),
		eval.args="^hsp$", invert=T, restore.on.close=StateEnv$win), 
		doLog=F)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
}

.hs_on_impute_missing_button_clicked <- function(button, justDisaccumulate=F) {
	freezeGUI()
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	maxGapLength <- theWidget("impute_missing_gaps_comboboxentry")$getActiveText()
	doInternalGapsOnly <- theWidget("impute_missing_gaps_internal_checkbutton")$getActive()
	doLocally <- theWidget("impute_locally_checkbutton")$getActive()
	doByDistance <- theWidget("impute_missing_distance_radiobutton")$getActive()
	doByCorrelation <- theWidget("impute_missing_correlation_radiobutton")$getActive()
	doByConstant <- theWidget("impute_missing_constant_radiobutton")$getActive()
	constTypeIdx <- theWidget("impute_missing_constant_combobox")$getActive() + 1
	constType <- switch(constTypeIdx, "mean", "mean", "zero", "extend")
	doTrim <- (constTypeIdx == 2)
	imputeMethod <- c(
		if (doByDistance) { "distance" },
		if (doByCorrelation) { "correlation" },
		if (doByConstant) { "constant" })
	
	addLogComment("Impute missing values")
	
	roles <- sapply(hsp$data, attr, "role")
	if (length(unique(roles[selNames])) > 1) {
		errorDialog("All selected items must have same role (e.g RAIN / FLOW).")
		return()
	}
	
	tmpObjs <- c("tmp.vars", "tmp.predictors")
	sameRole <- (roles == roles[selNames[1]])
	guiDo(call=bquote({
		tmp.vars <- .(selNames)
		tmp.predictors <- .(names(hsp$data)[sameRole])
	}))
	
	impute.call <- call('imputeGaps.timeblobs')
	impute.call[[2]] <- quote(hsp$data[tmp.predictors])
	impute.call$which.impute <- quote(tmp.vars)
	impute.call$timelim <- if (!is.null(hsp$timePeriod)) { quote(hsp$timePeriod) }
	impute.call$extend <- if (!doInternalGapsOnly) { T }
	impute.call$type <- if (justDisaccumulate) { "disaccumulate" }
	impute.call$method <- imputeMethod
	impute.call$constant <- if (doByConstant) { constType }
	impute.call$trim <- if (doTrim) { 0.01 }
	
	impute.assign.call <- quote(hsp$data[tmp.vars] <- foo)
	impute.assign.call[[3]] <- impute.call
	
	guiDo(call=impute.assign.call)
	
	if (length(tmpObjs) > 0) {
		guiDo(call=bquote(rm(list=.(tmpObjs))))
	}
	
	datasetModificationUpdate()
}

.hs_on_impute_disaccumulate_button_clicked <- function(button) {
	.hs_on_impute_missing_button_clicked(button, justDisaccumulate=T)
}

.hs_on_impute_undo_imputed_button_clicked <- function(button) {
	freezeGUI()
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	addLogComment("Revert imputed values")
	
	guiDo(call=bquote({
		tmp.vars <- .(selNames)
		hsp$data[tmp.vars] <- unimputeGaps.timeblobs(hsp$data[tmp.vars], 
			timelim=hsp$timePeriod, type="imputed")
		rm(tmp.vars)
	}))
	
	datasetModificationUpdate()
}

.hs_on_impute_undo_accumulated_button_clicked <- function(button) {
	freezeGUI()
	on.exit(thawGUI())
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	if (length(selNames) == 0) {
		errorDialog("No items selected.")
		return()
	}
	
	addLogComment("Revert disaccumulated values")
	
	guiDo(call=bquote({
		tmp.vars <- .(selNames)
		hsp$data[tmp.vars] <- unimputeGaps.timeblobs(hsp$data[tmp.vars], 
			timelim=hsp$timePeriod, type="disaccumulated")
		rm(tmp.vars)
	}))
	
	datasetModificationUpdate()
}

.hs_on_impute_calculate_gaps_button_clicked <- function(button) {
	freezeGUI()
	on.exit(thawGUI())
	
	TXV <- theWidget("impute_textview")
	setTextview(TXV, "")
	
	selNames <- iconViewGetSelectedNames(theWidget("selection_iconview"))
	
	for (x in selNames) {
		gapInfo <- gaps(window(hsp$data[[x]], hsp$timePeriod[1], 
			hsp$timePeriod[2], extend=T)$Data, internal.only=T)
		myHeader <- sprintf("%s gap length counts in specified period (total gaps: %i)",
			x, length(gapInfo$length))
		#mySumm <- paste(capture.output(
		#	print(table(gapInfo$length, dnn=myHeader))
		#), collapse='\n')
		gapTable <- table(gapInfo$length)
		mySumm <- paste(sep='', 
			myHeader, "\n  ",
			paste(sep='', collapse=', ',
				gapTable, 'x[', dimnames(gapTable)[[1]], ']')
		)
		if (length(gapInfo$length) == 0) {
			mySumm <- paste(x, "has no gaps in the specified time period.")
		}
		addTextview(TXV, mySumm, "\n")
	}
}

