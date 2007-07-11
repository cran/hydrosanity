## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateImportPage <- function() {
	ratio_item_combo <- theWidget("import_transform_ratio_item_combobox")
	ratio_item_combo$getModel()$clear()
	for (x in names(hsp$data)) {
		ratio_item_combo$appendText(x)
	}
	ratio_item_combo$setActive(0)
	
	# generate summary table
	
	dfID <- dfName <- dfData <- dfStart <- dfEnd <- dfLength <- dfFreq <- 
		dfLoc <- dfQual <- dfExtra <- dfRole <- character(length(hsp$data))
	
	for (i in seq(along=hsp$data)) {
		myLength <- end(hsp$data[[i]]) - start(hsp$data[[i]])
		myAvgFreq <- myLength / nrow(hsp$data[[i]])
		
		dfID[i] <- names(hsp$data)[i]
		dfName[i] <- attr(hsp$data[[i]], "sitename")
		dfData[i] <- attr(hsp$data[[i]], "dataname")
		dfStart[i] <- format(start(hsp$data[[i]]))
		dfEnd[i] <- format(end(hsp$data[[i]]))
		dfLength[i] <- as.byString(myLength, digits=2, explicit=T)
		#dfFreq[i] <- as.byString(myAvgFreq, digits=2)
		dfFreq[i] <- attr(hsp$data[[i]], "timestep")
		
		dfLoc[i] <- 'NA'
		myLoc <- attr(hsp$data[[i]], "location.xy")
		myElev <- attr(hsp$data[[i]], "elevation")
		if (!is.null(myLoc)) {
			myLoc <- format(round(myLoc, digits=2))
			if (is.null(myElev)) { myElev <- 'NA' } else {
				myElev <- format(round(myElev, digits=2))
			}
			dfLoc[i] <- paste('(', myLoc[1], ', ', myLoc[2], 
				', ', myElev, ')', sep='')
		}
		
		dfQual[i] <- class(hsp$data[[i]]$Qual)[1]
		if (is.factor(hsp$data[[i]]$Qual) || is.numeric(hsp$data[[i]]$Qual)) {
			levelsFn <- if (is.factor(hsp$data[[i]]$Qual))
			{ levels } else { unique }
			dfQual[i] <- paste(' (', toString( paste(
				levelsFn(hsp$data[[i]]$Qual),
			collapse="/"), width=30), ')', sep='')
		}
		dfExtra[i] <- ""
		if (ncol(hsp$data[[i]]) >= 4) {
		for (xcol in seq(4, ncol(hsp$data[[i]]))) {
			dfExtra[i] <- paste(dfExtra[i],
				if(xcol > 4)', ',
				names(hsp$data[[i]])[xcol],
				if (is.factor(hsp$data[[i]][[xcol]])) {
					paste(' (', toString( paste(
						levels(hsp$data[[i]][[xcol]]),
					collapse="/"), width=30), ')', sep='')
				},
				sep=''
			)
		}
		}
		
		myRole <- attr(hsp$data[[i]], "role")
		if (is.null(myRole)) { myRole <- "" }
		dfRole[i] <- myRole
	}
	
	dfModel <- rGtkDataFrame(data.frame(
		ID=dfID,
		Name=dfName,
		Start=dfStart,
		End=dfEnd,
		Length=dfLength,
		Timestep=dfFreq,
		Location_X.Y.Z=dfLoc,
		Qual=dfQual,
		Extra_data=dfExtra,
		Role=dfRole,
		Data=dfData,
		stringsAsFactors=F)
		)
	myTreeView <- theWidget("import_summary_treeview")
	myTreeView$setModel(dfModel)
	myTreeView$columnsAutosize()
	
	StateEnv$update$import <- F
	StateEnv$win$present()
}

## ACTIONS

.hs_on_import_displayfile_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	filenames <- choose.files(multi=T)
	StateEnv$win$present()
	if (length(filenames)==0) { return() }
	file.show(filenames)
}

.hs_on_import_viewtable_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	thisIndex <- blobIndices[1]
	blobName <- names(hsp$data)[thisIndex]
	if (length(blobIndices) > 1) {
		infoDialog("Only the first selected item (",
			blobName, ") will be shown.")
	}
	tmp <- hsp$data[[thisIndex]][,-1]
	row.names(tmp) <- make.unique(format(hsp$data[[thisIndex]]$Time))
	tmp2 <- edit(tmp, title=blobName)
	mostattributes(tmp2) <- attributes(tmp)
	if (!identical(tmp, tmp2)) {
		addLogComment("Edited data object ", dQuote(blobName), " interactively.")
		hsp$data[[thisIndex]][,-1] <<- tmp2
		setStatusBar("Edited data object ", dQuote(blobName))
		datasetModificationUpdate()
	}
}

.hs_on_import_file_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	filenames <- choose.files()
	StateEnv$win$present()
	if (length(filenames)==0) { return() }
	
	addLogComment("Import data from file")
	
	wasEmpty <- (length(hsp$data) == 0)
	
	import.string <- rep("", length(filenames))
	dataName <- rep("", length(filenames))
	
	## Fix filename for MS - otherwise eval/parse strip the \\.
	for (i in seq(along=filenames)) {
		filenames[i] <- gsub("\\\\", "/", filenames[i])
		dataName[i] <- get.stem(filenames[i])
	}
	
	myOptionString <- theWidget("import_options_entry")$getText()
	if (nchar(myOptionString) > 0) {
		myOptionString <- paste(",", myOptionString)
	}
	
	if (theWidget("import_known_format_radio")$getActive()) {
		kfIndex <- theWidget("import_known_format_combobox")$getActive()+1
		importSpec <- TIMESERIES.FORMATS[[kfIndex]]
		importFn <- importSpec[1]
		# user may have changed options in GUI, so use myOptionString
		for (i in seq(along=filenames)) {
			import.string[i] <- sprintf(
				'hsp$data[["%s"]] <- %s("%s"%s)', 
				dataName[i], importFn, filenames[i], myOptionString)
		}
	}
	else if (theWidget("import_file_with_time_radio")$getActive()) {
		myTimeCol <- theWidget("import_time_column_spinbutton")$getValue()
		myTimeFormat <- theWidget("import_time_format_comboboxentry")$getActiveText()
		for (i in seq(along=filenames)) {
			import.string[i] <- sprintf(
				'hsp$data[["%s"]] <- read.timeblob("%s", timeCol=%i, timeFormat="%s"%s)',
				dataName[i], filenames[i], myTimeCol, myTimeFormat, myOptionString)
		}
	}
	else if (theWidget("import_file_seq_radio")$getActive()) {
		myStartTime <- theWidget("import_time_start_entry")$getText()
		myTimeSeqBy <- theWidget("import_time_step_comboboxentry")$getActiveText()
		for (i in seq(along=filenames)) {
			import.string[i] <- sprintf(
				'hsp$data[["%s"]] <- read.timeblob("%s", startTime="%i", timeSeqBy="%s"%s)',
				dataName[i], filenames[i], myStartTime, myTimeSeqBy, myOptionString)
		}
	}
	
	for (i in seq(along=filenames)) {
		result <- guiDo(string=import.string[i])
		setStatusBar("Imported file ", dQuote(basename(filenames[i])),
			" to hsp$data[[", dQuote(dataName[i]), "]]")
		# mark as rain/flow/etc
		setDataRole(dataName[i], doLogComment=F)
		# update table etc: inefficient, but gives user more feedback
		updateImportPage()
	}
	
	if (wasEmpty) {
		# basic check included in transcipt once, not used in GUI
		addLogComment("Display a simple summary (structure) of the data.")
		addToLog('str(hsp$data)')
	}
	
	setIsImportMode(FALSE)
	
	datasetModificationUpdate()
}

.hs_on_import_summary_treeview_id_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobID <- names(hsp$data)[blobIndex]
	if (new.text == blobID) { return() }
	if (new.text == "") { return() }
	addLogComment("Set ID for object ", dQuote(blobID))
	guiDo(call=bquote(
		names(hsp$data)[.(blobIndex)] <- .(new.text)
	))
	
	setStatusBar("Set ID for object ", dQuote(blobID), " to ", dQuote(new.text))
	datasetModificationUpdate()
}

.hs_on_import_summary_treeview_sitename_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobID <- names(hsp$data)[blobIndex]
	blobName <- attr(hsp$data[[blobIndex]], "sitename")
	if (new.text == blobName) { return() }
	if (new.text == "") { return() }
	addLogComment("Set sitename for object ", dQuote(blobID))
	guiDo(call=bquote(
		attr(hsp$data[[.(blobID)]], "sitename") <- .(new.text)
	))
	
	setStatusBar("Set sitename for object ", dQuote(blobID), " to ", dQuote(new.text))
	datasetModificationUpdate()
}

.hs_on_import_summary_treeview_dataname_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobID <- names(hsp$data)[blobIndex]
	blobDataName <- attr(hsp$data[[blobIndex]], "dataname")
	if (new.text == blobDataName) { return() }
	if (new.text == "") { return() }
	addLogComment("Set dataname for object ", dQuote(blobID))
	guiDo(call=bquote(
		attr(hsp$data[[.(blobID)]], "dataname") <- .(new.text)
	))
	
	setStatusBar("Set dataname for object ", dQuote(blobID), " to ", dQuote(new.text))
	datasetModificationUpdate()
}

.hs_on_import_summary_treeview_role_edited <- function(cell, path.string, new.text, user.data) {
	blobIndex <- as.numeric(path.string)+1
	blobName <- names(hsp$data)[blobIndex]
	if (attr(hsp$data[[blobIndex]], "role") == new.text) { return() }
	setDataRole(blobName, new.text)
	datasetModificationUpdate()
}

.hs_on_import_edit_metadata_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	
	dfID <- dfName <- dfData <- dfRole <- character(length(blobIndices))
	dfLocX <- dfLocY <- dfElev <- numeric(length(blobIndices))
	
	for (k in seq(along=blobIndices)) {
		# 'i' indexes hsp$data; 'k' indexes metadata (subset)
		i <- blobIndices[k]
		dfID[k] <- names(hsp$data)[i]
		dfName[k] <- attr(hsp$data[[i]], "sitename")
		dfData[k] <- attr(hsp$data[[i]], "dataname")
		dfRole[k] <- attr(hsp$data[[i]], "role")
		if (is.null(dfRole[k])) { dfRole[k] <- "" }
		myLoc <- attr(hsp$data[[i]], "location.xy")
		if (is.null(myLoc)) { myLoc <- c(NA, NA) }
		dfLocX[k] <- myLoc[1]
		dfLocY[k] <- myLoc[2]
		myElev <- attr(hsp$data[[i]], "elevation")
		if (is.null(myElev)) { myElev <- NA }
		dfElev[k] <- myElev
	}
	#dfRole <- factor(dfRole, levels=c("RAIN", "FLOW", "OTHER"))
	
	metadata <- data.frame(
		SiteID=dfID,
		SiteName=dfName,
		DataName=dfData,
		Role=dfRole,
		X_Long=dfLocX,
		Y_Lat=dfLocY,
		Z_Elev=dfElev,
		check.names=F,
		stringsAsFactors=F
	)
	tmp.meta <<- metadata
	newMeta <- guiDo(editAsText(metadata), doLog=F)
	if (identical(metadata, newMeta)) {
		return()
	}
	
	# TODO: check that number of rows is the same...
	
	maybeUpdate <- function(assign.expr, envir=parent.frame()) {
		expr <- substitute(assign.expr)
		oldval <- eval(expr[[2]], envir=envir)
		newval <- eval(expr[[3]], envir=envir)
		if (is.null(newval)) { return() }
		if (any(is.na(newval))) { return() }
		if (any(newval == "")) { return() }
		if (identical(oldval, newval)) { return() }
		# assign as literal value
		expr[[3]] <- newval
		guiDo(call=expr)
		return(TRUE)
	}
	
	addLogComment(paste("Edit metadata for", length(blobIndices), "objects"))
	for (k in seq(along=blobIndices)) {
		# 'i' indexes hsp$data; 'k' indexes metadata (subset)
		i <- blobIndices[k]
		maybeUpdate(names(hsp$data)[i] <- newMeta$ItemName[k])
		maybeUpdate(attr(hsp$data[[i]], "sitename") <- newMeta$SiteName[k])
		maybeUpdate(attr(hsp$data[[i]], "dataname") <- newMeta$DataName[k])
		maybeUpdate(attr(hsp$data[[i]], "role") <- newMeta$Role[k])
		maybeUpdate(attr(hsp$data[[i]], "location.xy") <- 
			c(newMeta$X_Long[k], newMeta$Y_Lat[k]))
		maybeUpdate(attr(hsp$data[[i]], "elevation") <- newMeta$Z_Elev[k])
	}
	
	setStatusBar(paste("Edited metadata for", length(blobIndices), "objects"))
	datasetModificationUpdate()
}

.hs_on_import_remove_blob_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	
	blobNames <- names(hsp$data)[blobIndices]
	if (is.null(questionDialog("Remove item(s) ", 
		paste(blobNames,collapse=', '), "?"))) {
		return()
	}
	addLogComment("Remove data object(s)")
	guiDo(call=bquote(hsp$data[.(blobNames)] <- NULL))
	
	setStatusBar('Removed data object(s) ', paste(dQuote(blobNames),collapse=', '))
	datasetModificationUpdate()
}

.hs_on_import_extract_extra_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	
	blobNames <- names(hsp$data)[blobIndices]
	
	for (x in blobNames) {
		addLogComment("Extract extra columns into new item(s)")
		myCols <- ncol(hsp$data[[x]])
		myExtras <- names(hsp$data[[x]])[-(1:3)]
		for (extraName in myExtras) {
			if (!is.numeric(hsp$data[[x]][[extraName]])) { next }
			newBlobName <- paste(x,make.names(extraName),sep='_')
			guiDo(call=bquote({
				hsp$data[[.(newBlobName)]] <- hsp$data[[.(x)]]
				hsp$data[[.(newBlobName)]][4:.(myCols)] <- NULL
				hsp$data[[.(newBlobName)]]$Data <- hsp$data[[.(x)]][[.(extraName)]]
				hsp$data[[.(x)]][[.(extraName)]] <- NULL
			}))
		}
		setStatusBar('Extracted extra columns of item ', dQuote(x))
	}
	datasetModificationUpdate()
}

.hs_on_import_makefactor_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	factorCmdRaw <- theWidget("import_makefactor_comboboxentry")$getActiveText()
	
	addLogComment("Convert quality codes")
	factor_fn.string <- paste(sep="\n",
		'tmp.factor <- function(x) {',
			'x2 <- {', factorCmdRaw, '}',
			'factor(x2, ordered=T, exclude=NULL)',
		'}')
	guiDo(string=factor_fn.string)
	
	for (i in blobIndices) {
		blobName <- names(hsp$data)[i]
		guiDo(call=bquote(
			hsp$data[[.(blobName)]]$Qual <- tmp.factor(hsp$data[[.(blobName)]]$Qual)
		))
		setStatusBar("Converted quality codes of object ", dQuote(blobName))
	}
	guiDo(rm(tmp.factor))
	datasetModificationUpdate()
}

.hs_on_import_set_accums_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(blobIndices)
	blobNames <- names(hsp$data)[blobIndices]
	
	maxGapLengthAccum <- theWidget("import_accum_gaps_comboboxentry")$getActiveText()
	
	addLogComment("Set multiple accumulations")
	
	for (x in blobNames) {
		maxGapStepsAccum <- round(
			as.numeric.byString(maxGapLengthAccum)
			/ as.numeric.byString(attr(hsp$data[[x]], "timestep")))
		guiDo(call=bquote(
			tmp.gapInfo <- gaps(hsp$data[[.(x)]]$Data, internal.only=T, 
				max.length=.(maxGapStepsAccum))
		))
		if (nrow(tmp.gapInfo) > 0) {
			guiDo(call=bquote({
				hsp$data[[.(x)]]$AccumSteps <- as.integer(1)
				with(tmp.gapInfo, 
					hsp$data[[.(x)]]$AccumSteps[start + length] <-
						length + 1
				)
			}))
		} else {
			addToLog("# No gaps fit the criteria.")
		}
	}
	guiDo(rm(tmp.gapInfo))
	datasetModificationUpdate()
}

.hs_on_export_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(blobIndices)
	blobNames <- names(hsp$data)[blobIndices]
	dataString <- paste('hsp$data[',
		paste(dQuote(blobNames),collapse=", "), ']', sep='')
	
	justTimePeriod <- theWidget("export_timeperiod_radiobutton")$getActive()
	oneFile <- theWidget("export_onefile_radiobutton")$getActive()
	csvFile <- theWidget("export_csv_radiobutton")$getActive()
	timeFormat <- theWidget("export_time_format_comboboxentry")$getActiveText()
	myOptionString <- theWidget("export_options_entry")$getText()
	if (is.null(hsp$timePeriod)) { justTimePeriod <- F }
	if (nBlobs == 1) { oneFile <- F }
	if (nchar(myOptionString) > 0) {
		myOptionString <- paste(",", myOptionString)
	}
	myOptionString <- paste(', row.names=F', myOptionString, sep='')
	if (!csvFile) {
		myOptionString <- paste(', sep="\t"', myOptionString, sep='')
	}
	
	exportFn <- if (csvFile) { "write.csv" } else { "write.table" }
	ext <- if (csvFile) { "csv" } else { "txt" }
	
	defaultName <- blobNames[1]
	if (!oneFile && (nBlobs > 1)) {
		defaultName <- "%NAME%"
	}
	if (justTimePeriod) {
		periodString <- paste(format(hsp$timePeriod, "%y"), collapse='-')
		defaultName <- paste(defaultName, periodString, sep='_')
	}
	defaultName <- paste(defaultName, ext, sep='.')
	filename <- choose.file.save(defaultName, caption="Export data", 
		filters=Filters[c("txt","All"),])
	StateEnv$win$present()
	if (is.na(filename)) { return() }
	
	## Fix filename for MS - otherwise eval/parse strip the \\.
	filename <- gsub("\\\\", "/", filename)
	
	if (get.extension(filename) != ext) {
		filename <- paste(filename, ext, sep='.')
	}
	
	addLogComment("Export data to file")
	
	if (oneFile) {
		guiDo(call=bquote({
			tmp.data <- sync.timeblobs(hsp$data[.(blobNames)], 
				timelim=.(if (justTimePeriod) { quote(hsp$timePeriod) }))
			tmp.data$Time <- format(tmp.data$Time, format=.(timeFormat))
		}))
		export.string <- sprintf(
			'%s(tmp.data, %s%s)', 
			exportFn, dQuote(filename), myOptionString)
		guiDo(string=export.string)
		setStatusBar("Exported data to ", dQuote(filename))
			
	} else {
		for (i in seq(along=blobNames)) {
			x <- blobNames[i]
			myFilename <- filename
			if (nBlobs > 1) {
				# use blob name to identify each file
				myFilename <- sub('%NAME%', x, myFilename)
				if (identical(filename, myFilename)) {
					# or just put a number in the filename
					myFilename <- paste(sep='',
						substr(filename,1,nchar(filename)-4),
						'_', i, '.', ext)
				}
			}
			if (justTimePeriod) {
				guiDo(call=bquote(
					tmp.data <- window(hsp$data[[.(x)]],
						hsp$timePeriod[1], hsp$timePeriod[2])
				))
			} else {
				guiDo(call=bquote(tmp.data <- hsp$data[[.(x)]]))
			}
			guiDo(call=bquote(
				tmp.data$Time <- format(tmp.data$Time, format=.(timeFormat))
			))
			export.string <- sprintf(
				'%s(tmp.data, %s%s)', 
				exportFn, dQuote(myFilename), myOptionString)
			guiDo(string=export.string)
			setStatusBar("Exported data item ", dQuote(x), " to ", 
				dQuote(myFilename))
		}
	}
	
	guiDo(rm(tmp.data))
}

.hs_on_import_transform_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(blobIndices)
	blobNames <- names(hsp$data)[blobIndices]
	
	timestepString <- theWidget("import_transform_timestep_comboboxentry")$getActiveText()
	aggrFunIdx <- theWidget("import_transform_aggrfun_combobox")$getActive() + 1
	qualFunIdx <- theWidget("import_transform_qualfun_combobox")$getActive() + 1
	doReplace <- theWidget("import_transform_replace_checkbutton")$getActive()
	
	aggrFun <- switch(aggrFunIdx, quote(mean), quote(median), quote(sum), quote(max))
	qualFunName <- switch(qualFunIdx, 'worst', 'median', 'mode')
	
	addLogComment("Resample to different timestep")
	
	for (x in blobNames) {
		newName <- x
		if (!doReplace) {
			newName <- paste(x, sub(' ','',timestepString), sep='_')
		}
		aggr.call <- bquote(
			hsp$data[[.(newName)]] <- aggregate.timeblob(
				hsp$data[[.(x)]], by=.(timestepString))
		)
		if (aggrFunIdx > 1) { aggr.call[[3]]$FUN <- aggrFun }
		if (qualFunIdx > 1) { aggr.call[[3]]$fun.qual <- qualFunName }
		if (any(grep("( month|year)", timestepString))) {
			aggr.call[[3]]$start.month <- hsp$startMonth
		}
		guiDo(call=aggr.call)
		setStatusBar("Resampled object ", dQuote(x))
	}
	
	datasetModificationUpdate()
}

.hs_on_import_transform_ratio_button_clicked <- function(button) {
	StateEnv$win$setSensitive(F)
	on.exit(StateEnv$win$setSensitive(T))
	setStatusBar("")
	
	blobIndices <- treeViewGetSelectedIndices(theWidget("import_summary_treeview"))
	if (length(blobIndices)==0) {
		errorDialog("No items selected.")
		return()
	}
	nBlobs <- length(blobIndices)
	blobNames <- names(hsp$data)[blobIndices]
	
	timestepString <- theWidget("import_transform_ratio_timestep_comboboxentry")$getActiveText()
	denomIndex <- theWidget("import_transform_ratio_item_combobox")$getActive() + 1
	denomName <- names(hsp$data)[denomIndex]
	
	addLogComment("Take ratio of time series over ", timestepString)
	
	delta <- as.numeric.byString(attr(hsp$data[[denomName]], "timestep"))
	smoothDelta <- as.numeric.byString(timestepString)
	winSize <- round(smoothDelta / delta)
	guiDo(call=bquote(tmp.filter <- rep(1/.(winSize), .(winSize))))
		
	guiDo(call=bquote({
		tmp.denom <- quick.disaccumulate.timeblob(hsp$data[[.(denomName)]])
		tmp.denom$Data <- filter(tmp.denom$Data, tmp.filter)
		tmp.data <- syncTo.timeblobs(lapply(hsp$data[.(blobNames)], quick.disaccumulate.timeblob), 
			blob=tmp.denom)
		tmp.data[-1] <- lapply(tmp.data[-1], filter, tmp.filter)
	}))
	
	for (i in seq(along=blobNames)) {
		x <- blobNames[i]
		newName <- paste('ratio', x, denomName, sep='_')
		newDataName <- paste(attr(hsp$data[[x]], "dataname"),
			attr(hsp$data[[denomName]], "dataname"), sep=" / ")
		guiDo(call=bquote(
			hsp$data[[.(newName)]] <- timeblob(tmp.data$Time, 
				Data=(tmp.data[[.(i+1)]] / tmp.denom$Data), 
				dataname=.(newDataName))
		))
	}
	
	setStatusBar("Generated ratio of ", nBlobs, " item(s) to ", dQuote(denomName))
	
	datasetModificationUpdate()
}


## NON-ACTIONS, just interface bits and pieces

setDataRole <- function(blobName, role=NULL, doLogComment=T) {
	if (is.null(role)) {
		if (one.step.acf(hsp$data[[blobName]]) > 0.5) {
			role <- "FLOW"
		} else {
			role <- "RAIN"
		}
	}
	
	if (doLogComment) { addLogComment("Set data role") }
	
	guiDo(call=bquote(
		attr(hsp$data[[.(blobName)]], "role") <- .(role)
	))
	setStatusBar("Set data role for object ", dQuote(blobName), " to ",
		dQuote(role))
}

.hs_on_import_file_radio_options_toggled <- function(button) {
	
	newPageIdx <- 0
	if (theWidget("import_known_format_radio")$getActive()) {
		newPageIdx <- 0
		.hs_on_import_known_format_combobox_changed(
			theWidget("import_known_format_combobox"))
	} else {
		theWidget("import_options_expander")$setExpanded(TRUE)
		# TODO: need to check switching from known format or not
		theWidget("import_options_entry")$setText(
			'sep=",", skip=1, dataname="Data", dataCol=2, qualCol=3')
	}
	
	if (theWidget("import_file_with_time_radio")$getActive()) {
		newPageIdx <- 1
	}
	
	if (theWidget("import_file_seq_radio")$getActive()) {
		newPageIdx <- 2
	}
	
	theWidget("import_file_radio_options_notebook")$setCurrentPage(newPageIdx)
}

.hs_on_import_robj_radio_clicked <- function(button) {
	infoDialog(isMarkup=T, paste(sep='',
	'Importing data from R must be done from the R console, using a command like:',
	'\n\n',
	'<tt>hsp$data[["myName"]] &lt;- timeblob(Time=myTime, Data=myData)</tt>',
	'\n\n',
	'and then choose <i>Update</i> from the <i>View</i> menu.',
	'\n\n',
	'See <tt>help(timeblob)</tt> for details.'))
}

.hs_on_import_known_format_combobox_changed <- function(widget) {
	kfIndex <- widget$getActive()+1
	theWidget("import_options_entry")$setText(TIMESERIES.FORMATS[[kfIndex]][2])
}

.hs_on_dataset_expanders_activate <- function(widget) {
	for (x in c("import_import_expander", "import_edit_expander",
	"import_transform_expander", "import_export_expander")) {
		if (theWidget(x) == widget) {
			# skip, will be expanded
		} else {
			theWidget(x)$setExpanded(FALSE)
		}
	}
}


