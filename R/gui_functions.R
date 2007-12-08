## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL
##
## Some of these functions are adapted from Rattle v2.1 by Graham Williams.

theWidget <- function(name) {
	return(StateEnv$GUI$getWidget(name))
}

## Log support

addToLog <- function(..., sep="", end.with="\n") {
	if (isTRUE(StateEnv$echo.to.log))
		addTextview(theWidget("log_textview"), ..., end.with, sep=sep)
	if (isTRUE(StateEnv$echo.to.console))
		message(...)
}

addLogSeparator <- function() {
	addToLog("\n",
		"## ", paste(rep("=", 60), collapse=""), "\n",
		"## Timestamp: ", format(Sys.time()))
}

addLogComment <- function(...) {
	addToLog("\n## ", ...)
}

## PLAYWITH TOOLS

restoreHS <- function(playState) {
	StateEnv$win$present()
	FALSE
}

setPeriodTool <- function(playState) {
	quickTool(playState,
		"Set period", 
		icon = "gtk-yes", 
		f = setperiod_handler)
}

setRegionTool <- function(playState) {
	quickTool(playState,
		"Set region", 
		icon = "gtk-yes", 
		f = setregion_handler)
}

setperiod_handler <- function(widget, playState) {
	addLogComment("Set time period for analysis")
	xlim <- callArg(playState, xlim)
	if (is.null(xlim)) {
		guiDo(hsp$timePeriod <- NULL)
		gmessage(paste("Set time period for analysis to NULL",
			"(i.e. include all data)"))
	} else {
		timelim <- as.POSIXct(xlim)
		myTimeStrings <- format(round(timelim, "days"))
		guiDo(call=bquote(
			hsp$timePeriod <- as.POSIXct(.(myTimeStrings), tz="GMT")
		))
		gmessage(paste("Set time period for analysis:",
			paste(myTimeStrings, collapse=" to ")))
	}
	timeperiodModificationUpdate()
	playState$win$present()
}

setregion_handler <- function(widget, playState) {
	addLogComment("Set region for analysis")
	xlim <- callArg(playState, xlim)
	ylim <- callArg(playState, ylim)
	if (is.null(xlim) || is.null(ylim)) {
		guiDo(hsp$region <- NULL)
		gmessage(paste("Set region for analysis to NULL ",
			"(i.e. include all sites)"))
	} else {
		xlim <- round(xlim, digits=3)
		ylim <- round(ylim, digits=3)
		guiDo(call=bquote(hsp$region <- list(xlim=.(xlim), ylim=.(ylim))))
		gmessage(paste("Set region for analysis:",
			"X=", paste(xlim, collapse=" to "), 
			"Y=", paste(ylim, collapse=" to ")))
	}
	regionModificationUpdate()
	playState$win$present()
}

# TODO: delete these
hydrosanityButtons <- alist(
	setregion=quickTool("Set region", "gtk-yes", f=.hs_map_setregion_event),
	setperiod=quickTool("Set period", "gtk-yes", f=.hs_time_setperiod_event),
	zoomin=quickTool("Zoom in", "gtk-zoom-in", f=.hs_time_zoomin_event),
	zoomout=quickTool("Zoom out", "gtk-zoom-out", f=.hs_time_zoomout_event),
	centre=quickTool("Re-centre", "gtk-jump-to-ltr", f=.hs_time_centre_event),
	logscale=quickTool("Log scale", "gtk-goto-top", f=.hs_time_logscale_event, isToggle=T),
	layers=quickTool("Layers...", "gtk-index", f=.hs_layers_event)
)

as.POSIXct.numeric <- function(x) {
	structure(as.numeric(x), class = c("POSIXt", "POSIXct"))
}

.hs_map_setregion_event <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get current scale setting
	myX <- eval(tmp.state$call$xlim, tmp.state$env)
	myY <- eval(tmp.state$call$ylim, tmp.state$env)
	addLogComment("Set region for analysis")
	if (is.null(myX) || is.null(myY)) {
		guiDo(hsp$region <- NULL)
		infoDialog("Set region for analysis to NULL ",
			"(i.e. include all sites)")
	} else {
		myX <- round(myX, digits=3)
		myY <- round(myY, digits=3)
		guiDo(call=bquote(hsp$region <- list(xlim=.(myX), ylim=.(myY))))
		infoDialog("Set region for analysis: X=",
			paste(myX, collapse=" to "), " Y=", paste(myY, collapse=" to "))
	}
	regionModificationUpdate()
	tmp.state$win$present()
}

.hs_time_setperiod_event <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get current scale setting
	myXScale <- eval(tmp.state$call$xscale, tmp.state$env)
	addLogComment("Set time period for analysis")
	if (is.null(myXScale)) {
		guiDo(hsp$timePeriod <- NULL)
		infoDialog("Set time period for analysis to NULL ",
			"(i.e. include all data)")
	} else {
		timelim <- as.POSIXct(myXScale)
		myTimeStrings <- format(round(timelim, "days"))
		guiDo(call=bquote(
			hsp$timePeriod <- as.POSIXct(.(myTimeStrings), tz="GMT")
		))
		infoDialog("Set time period for analysis: ",
			paste(myTimeStrings, collapse=" to "))
	}
	timeperiodModificationUpdate()
	tmp.state$win$present()
}

.hs_time_centre_event <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	# get new scales interactively
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("Viewport 'time.vp' not found")
		return()
	}
	xscale <- as.numeric(convertX(unit(0:1, "npc"), "native"))
	# get new centre point
	plotAndPlaySetPrompt("Click to re-centre the plot")
	clickLoc <- grid.locator()
	if (is.null(clickLoc)) {
		upViewport(depth)
		return()
	}
	xscale.new <- as.numeric(clickLoc$x) + diff(xscale) * c(-0.5, 0.5)
	xscale.new <- as.POSIXct(xscale.new)
	# update state
	tmp.state$call$xscale <- xscale.new
	plotAndPlaySetCurrState(tmp.state)
	plotAndPlayUpdate()
}

.hs_time_zoomin_event <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	# set up masking
	maskGrob <- rectGrob(gp=gpar(col="grey", 
		fill=rgb(0.5,0.5,0.5, alpha=0.5)), name="tmp.mask")
	# get new scales interactively
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("Viewport 'time.vp' not found")
		return()
	}
	xscale <- convertX(unit(0:1, "npc"), "native")
	# get start time
	plotAndPlaySetPrompt("Click at the start of the window (to zoom in to)")
	clickLoc <- grid.locator()
	if (is.null(clickLoc)) {
		upViewport(depth)
		return()
	}
	xscale.new <- as.POSIXct.numeric(clickLoc$x)
	grid.draw(editGrob(maskGrob, x=unit(0,"npc"), 
		width=(clickLoc$x - xscale[1]), just="left"))
	# get end time
	plotAndPlaySetPrompt("OK, now click at the end of the window")
	clickLoc <- grid.locator()
	if (is.null(clickLoc)) {
		grid.remove("tmp.mask", grep=T, global=T, strict=T)
		upViewport(depth)
		return()
	}
	xscale.new[2] <- as.POSIXct.numeric(clickLoc$x)
	grid.draw(editGrob(maskGrob, x=unit(1,"npc"),
		width=(xscale[2] - clickLoc$x), just="right"))
	# update state
	tmp.state$call$xscale <- xscale.new
	plotAndPlaySetCurrState(tmp.state)
	plotAndPlayUpdate()
}

.hs_time_zoomout_event <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get new scales interactively
	depth <- try(downViewport("time.vp"), silent=T)
	if (inherits(depth, "try-error")) {
		errorDialog("Viewport 'time.vp' not found")
		return()
	}
	xscale <- as.numeric(convertX(unit(0:1, "npc"), "native"))
	xscale <- xscale + diff(xscale) * c(-0.5, 0.5)
	xscale <- as.POSIXct(xscale)
	# update state
	tmp.state$call$xscale <- xscale
	plotAndPlaySetCurrState(tmp.state)
	plotAndPlayUpdate()
}

.hs_time_logscale_event <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get new log scale setting
	logScale <- widget$getActive()
	# update state
	tmp.state$call$logScale <- logScale
	plotAndPlaySetCurrState(tmp.state)
	plotAndPlayUpdate()
}

.hs_layers_event <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# show window with layer options
	c("add.cities", "add.rivers", "z", "")
	# update state
	tmp.state$call$add.cities <- add.cities
	plotAndPlaySetCurrState(tmp.state)
	plotAndPlayUpdate()
}


## TREE VIEW AND ICON VIEW HELPERS

insertTreeViewTextColumns <- function(treeView, colNames=colnames(treeView$getModel()), editors=NULL, combo=NULL) {
	for (i in seq(along=colNames)) {
		renderer <- gtkCellRendererText()
		#renderer$set(xalign = 1.0) # right-align
		if (!is.null(combo[[ colNames[i] ]])) {
			renderer <- gtkCellRendererCombo()
			renderer$set(model = rGtkDataFrame(data.frame(combo[[ colNames[i] ]])), text_column = 0, has_entry = F)
		}
		if (!is.null(editors[[ colNames[i] ]])) {
			renderer$set(editable = T)
			gSignalConnect(renderer, "edited", editors[[ colNames[i] ]])
		}
		treeView$insertColumnWithAttributes(
			-1, gsub('_', ' ', colNames[i]), renderer, text=i-1)
	}
	invisible(NULL)
}

# note these indices are in the R convention (first element is #1)
treeViewGetSelectedIndices <- function(treeView) {
	selPaths <- treeView$getSelection()$getSelectedRows()$retval
	if (length(selPaths)==0) { return(NULL) }
	indices <- sapply(selPaths, function(x) x$getIndices()) + 1
}

setupIconView <- function(iconView, itemNames=names(hsp$data)) {
	
	flowPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_FLOW.png"))$retval
	rainPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_RAIN.png"))$retval
	arealPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_AREAL.png"))$retval
	otherPixbuf <- gdkPixbufNewFromFile(getpackagefile("icon_OTHER.png"))$retval
	# (or NULL)
	
	# these columns are: item name, display text, icon
	list_store <- gtkListStore("character", "character", "GdkPixbuf")
	
	for (x in itemNames) {
		
		iter <- list_store$append()$iter
		list_store$set(iter, 0, x)
		myName <- paste(x, attr(hsp$data[[x]], "sitename"), sep=": ")
		list_store$set(iter, 1, myName)
		# set icon
		myRole <- attr(hsp$data[[x]], "role")
		if (is.null(myRole)) { myRole <- "OTHER" }
		list_store$set(iter, 2, switch(myRole,
			"FLOW"=flowPixbuf,
			"RAIN"=rainPixbuf,
			"AREAL"=arealPixbuf,
			otherPixbuf)
		)
	}
	selPaths <- NULL
	if (!is.null(iconView$getModel())) {
		selPaths <- iconView$getSelectedItems()
	}
	iconView$setModel(list_store)
	iconView$setTextColumn(1)
	iconView$setPixbufColumn(2)
	iconView$setItemWidth(110)
	if (!is.null(selPaths)) {
		for (p in selPaths) { iconView$selectPath(p) }
	}
	iconView$resizeChildren() #?
	invisible(NULL)
}

iconViewSetSelection <- function(iconView, selection=c("first", "all", "none", "rain")) {
	selection <- match.arg(selection)
	
	if (selection == "first") {
		iconView$selectPath(gtkTreePathNewFromIndices(0))
	}
	if (selection == "all") {
		iconView$selectAll()
	}
	if (selection == "none") {
		iconView$unselectAll()
	}
	if (selection == "rain") {
		iconView$unselectAll()
		for (i in seq_along(hsp$data)) {
			if (attr(hsp$data[[i]], "role") == "RAIN") {
				iconView$selectPath(gtkTreePathNewFromIndices(i-1))
			}
		}
	}
}

iconViewGetSelectedNames <- function(iconView) {
	# it's not enough to get the item indices since user can re-order them!
	selPaths <- iconView$getSelectedItems()
	if (length(selPaths)==0) { return(NULL) }
	# these are returned in reverse order, strangely
	selPaths <- rev(selPaths)
	# get names of items
	# assuming that the names are in column 0 of the model
	dataListModel <- iconView$getModel()
	selNames <- sapply(selPaths, function(x) {
		dataListModel$getValue(dataListModel$getIter(x)$iter, 0)$value
	})
	return(selNames)
}


## Misc

freezeGUI <- function(echo.to.log=T) {
	StateEnv$win$setSensitive(F)
	StateEnv$win$getWindow()$setCursor(gdkCursorNew("watch"))
	StateEnv$echo.to.log <- echo.to.log
	setStatusBar("")
}

thawGUI <- function() {
	StateEnv$win$setSensitive(T)
	StateEnv$win$getWindow()$setCursor(NULL)
	StateEnv$echo.to.log <- T # default
}

setStatusBar <- function(..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  theWidget("statusbar")$pop(1)
  theWidget("statusbar")$push(1, msg)
  while (gtkEventsPending()) gtkMainIteration() # Refresh status and windows
  invisible(NULL)
}


## Some of these functions are based on ones in Rattle v2.1
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2
## Graham.Williams@togaware.com

# generally useful RGtk2 GUI things

guiDo <- function(expr, call, string, doLog=T, doFailureLog=doLog, 
	logFunction=addToLog, doFailureDialog=T, doStop=T, 
	envir=if (doLog) .GlobalEnv else parent.frame(), ...) {
	
	if (missing(expr) + missing(call) + missing(string) != 2)
		stop("give only one of 'expr', 'call' and 'string'")
	if (!missing(expr)) call <- substitute(expr)
	isString <- !missing(string)
	
	# set default log function in case 'addToLog' is not defined
	if (doLog || doFailureLog) {
		if (inherits(try(eval(logFunction), silent=T), "try-error")
		|| !is.function(eval(logFunction))) {
			logFunction <- function(x) cat(x, "\n")
		}
	}
	# log it
	if (doLog) {
		theCall <- if (isString) {
			try(parse(text=string)[[1]], silent=T)
		} else call
		if (isString && inherits(theCall, "try-error")) {
			# syntax error
			logFunction(string)
		} else {
			callPretty <- paste(
				# if the code is in a simple block, omit braces
				if (identical(theCall[[1]], as.symbol("{"))) {
					unlist(lapply(theCall[-1], deparse, ...))
				} else {
					deparse(theCall, ...)
				}, collapse="\n")
			logFunction(callPretty)
		}
	}
	# set up error handler
	handleIt <- function(e) {
		# show error dialog
		if (doFailureDialog) {
			commandText <- if (isString) string else {
				paste(deparse(call, ...), collapse="\n")
			}
			msgText <- conditionMessage(e)
			callText <- deparse(conditionCall(e), width.cutoff=500)[1]
			if (length(msgText)==0) msgText <- ""
			if (length(callText)==0) callText <- ""
			errorDialog(paste(sep='',
				'A command has failed. The error was:',
				'\n\n<span foreground="#aa0000">', 
					pangoEscape(msgText),
				'</span>\n\n',
				'The error occurred in: \n\n<tt>',
					pangoEscape(callText), 
				'</tt>\n\n', 
				'The original command was: \n\n<tt>',
					pangoEscape(commandText), 
				'</tt>\n\n',
				'If this is not your fault, you might want to select ',
				'this text and copy it into a bug report. Please also ',
				'include the output from <tt>sessionInfo()</tt>'),
				isMarkup=T)
		}
		if (doFailureLog) {
			logFunction("# FAILED")
		}
		# propagate the error
		if (doStop) {
			stop(e)
		}
		return(e)
	}
	# evaluate it
	enclos <- if (is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
	if (isString) {
		result <- tryCatch(eval(parse(text=string), envir=envir,
			enclos=enclos), error=handleIt)
	} else {
		result <- tryCatch(eval(call, envir=envir, enclos=enclos), 
			error=handleIt)
	}
	return(result)
}

errorDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="error", ..., isMarkup=isMarkup)
}

infoDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="info", ..., isMarkup=isMarkup)
}

questionDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="question", ..., isMarkup=isMarkup)
}

guiMessageDialog <- function(type="info", ..., isMarkup=F) {
	myString <- paste(sep='', ...)
	myButtons <- switch(type,
		error="close",
		info="ok",
		question="yes-no"
	)
	dialog <- gtkMessageDialogNew(NULL, NULL, type, myButtons, myString)
	if (isMarkup) {
		dialog$setMarkup(myString)
	}
	result <- dialog$run() # make it modal
	dialog$destroy()
	if (result == GtkResponseType["yes"]) {
		return("yes")
	} else {
		return(invisible(NULL))
	}
}

pangoEscape <- function(x) {
	x <- gsub('%', '%%', x)
	x <- gsub('&', '&amp;', x)
	x <- gsub('<', '&lt;', x)
	x <- gsub('>', '&gt;', x)
	#x <- gsub('&&', '&amp;&amp;', x)
	#x <- gsub('& ', '&amp; ', x)
	#x <- gsub('<<', '&lt;&lt;', x)
	#x <- gsub('<-', '&lt;-', x)
	#x <- gsub('< ', '&lt; ', x)
	x
}

guiTextInput <- function(text="", title="Text Input", prompt="", oneLiner=F, 
	accepts.tab=T, wrap.mode=c("none", "char", "word", "word_char"), 
	size=c(600, 320), width.chars=-1, focus.on.ok=!oneLiner) {
	
	wrap.mode <- match.arg(wrap.mode)
	# construct dialog
	editBox <- gtkDialog(title=title, NULL, NULL,
		"OK", GtkResponseType["ok"], "Cancel", GtkResponseType["cancel"],
		show = F)
	editBox$setDefaultResponse(GtkResponseType["ok"])
	if (nchar(prompt) > 0) {
		editBox[["vbox"]]$packStart(gtkLabel(prompt), expand=F, pad=2)
	}
	if (oneLiner) {
		editEntry <- gtkEntry()
		editEntry['activates-default'] <- T
		editEntry['text'] <- text
		editEntry['width-chars'] <- width.chars
		editBox[["vbox"]]$packStart(editEntry, pad=10)
	} else {
		editBox$setDefaultSize(size[1], size[2])
		editTV <- gtkTextView()
		setTextviewMonospace(editTV)
		editTV$setWrapMode(GtkWrapMode[wrap.mode])
		editTV$setAcceptsTab(accepts.tab)
		setTextview(editTV, text)
		scroller <- gtkScrolledWindow()
		scroller$add(editTV)
		scroller$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
		editBox[["vbox"]]$packStart(scroller)
	}
	# put focus on the OK button
	if (focus.on.ok) editBox[["actionArea"]]$getChildren()[[2]]$grabFocus()
	result <- editBox$run() # make it modal
	newTxt <- if (oneLiner) editEntry['text'] else getTextviewText(editTV)
	editBox$destroy()
	if (result != GtkResponseType["ok"]) return(invisible(NULL))
	newTxt
}

## EDIT DATA FRAMES AS TEXT

editAsText <- function(x, title=NULL, edit.row.names=any(row.names(x) != 1:nrow(x))) {
	if (!is.data.frame(x)) stop("'x' must be a data frame")
	if (is.null(title)) {
		title <- paste("Editing", deparse(substitute(x)))
	}
	# make table text block from data frame 'x'
	foo <- capture.output(
		write.table(x, sep="\t", quote=F, row.names=edit.row.names,
		col.names=if (edit.row.names) {NA} else {T})
	)
	tableTxt <- paste(paste(foo, collapse="\n"), "\n", sep='')
	if (edit.row.names) tableTxt <- paste("row.names", tableTxt, sep='')
	# show text box and repeat if there was an error
	readOK <- F
	while (!readOK) {
		newTableTxt <- guiTextInput(text=tableTxt, title=title, 
			prompt=paste("Copy and paste to/from a spreadsheet,",
				"or edit the text here (in tab-separated format).\n",
				"Do not move the columns around,",
				"they must stay in this order."))
		if (is.null(newTableTxt)) return(x)
		# convert table text block back to data frame
		zz <- textConnection(newTableTxt)
		newData <- tryCatch(
			read.delim(file=zz, header=T, colClasses=sapply(x, class),
			row.names=if (edit.row.names) {1} else {NULL}),
			error=function(e)e)
		close(zz)
		# check whether there was an error in reading the table
		if (inherits(newData, "error")) {
			errorDialog("Error reading table: ", conditionMessage(newData))
			tableTxt <- newTableTxt
		} else {
			readOK <- T
		}
	}
	# warn if the number of rows has changed
	if (nrow(newData) != nrow(x)) {
		warning("Number of rows changed from ",
			nrow(x), " to ", nrow(newData))
	} else if (!edit.row.names) {
		# keep original row names 
		row.names(newData) <- attr(x, "row.names")
	}
	# ensure factor levels are the same
	for (i in which(sapply(x, class) == "factor")) {
		newData[[i]] <- factor(newData[[i]], levels=levels(x[[i]]))
	}
	newData
}


## Textview widget support

setTextview <- function(tv, ..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  tv$getBuffer()$setText(msg)
  invisible(NULL)
}

addTextview <- function(tv, ..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  tv.buf <- tv$getBuffer()
  loc <- tv.buf$getEndIter()$iter
  tv.buf$insert(loc, msg)
  invisible(NULL)
}

getTextviewText <- function(tv)
{
  ## Extract text content of specified textview
  log.buf <- tv$getBuffer()
  start <- log.buf$getStartIter()$iter
  end <- log.buf$getEndIter()$iter
  return(log.buf$getText(start, end))
}

setTextviewMonospace <- function(tv)
{
  tv$modifyFont(pangoFontDescriptionFromString("monospace 10"))
  invisible(NULL)
}

Filters <- matrix(c(
	"R or S files (*.R,*.q,*.ssc,*.S)", "*.R;*.q;*.ssc;*.S",
	"Postscript files (*.ps)",          "*.ps",             
	"Encapsulated Postscript (*.eps)",  "*.eps",            
	"PDF files (*.pdf)",                "*.pdf",            
	"Png files (*.png)",                "*.png",            
	"Jpeg files (*.jpeg,*.jpg)",        "*.jpeg;*.jpg",     
	"Text files (*.txt)",               "*.txt",            
	"R images (*.RData,*.rda)",         "*.RData;*.rda",    
	"Zip files (*.zip)",                "*.zip",            
	"SVG files (*.svg)",                "*.svg",            
	"Windows Metafiles (*.wmf,*.emf)",  "*.wmf;*.emf",      
	"xfig files (*.fig)",               "*.fig",            
	"All files (*.*)",                  "*.*"), ncol=2, byrow=T,
	dimnames=list(c('R','ps','eps','pdf','png','jpeg','txt',
	'RData','zip','svg','wmf','fig','All'),NULL))

# returns character string, or NA if cancelled
choose.file.save <- function(default="", caption="Save File", filters=Filters[c("All"),], index=0) {
	dialog <- gtkFileChooserDialog(caption, NULL, "save",
		"gtk-cancel", GtkResponseType["cancel"],
		"gtk-save", GtkResponseType["accept"])
	dialog$setCurrentName(default)
	
	if (length(filters)==2) {
		filters <- matrix(filters, nrow=1, ncol=2)
	}
	
	for (i in seq(1, nrow(filters))) {
		ff <- gtkFileFilterNew()
		ff$setName(filters[i,1])
		for (x in strsplit(filters[i,2], ';')[[1]]) {
			ff$addPattern(x)
		}
		dialog$addFilter(ff)
		if (i == index) dialog$setFilter(ff)
	}
	
	#dialog$setDoOverwriteConfirmation(T) crap, appears behind filechooser
	if (dialog$run() == GtkResponseType["accept"]) {
		filename <- dialog$getFilename()
		if (file.exists(filename)) {
			if (is.null(questionDialog("Replace existing file?"))) {
				filename <- NA
			}
		}
		dialog$destroy()
		return(filename)
	} else {
		dialog$destroy()
		return(NA)
	}
}

get.extension <- function(path)
{
  ## Extract and return the extension part of a filename
  
  parts <- strsplit(path, "\\.")[[1]]
  if (length(parts) > 1)
    last <- parts[length(parts)]
  else
    last <- ""
  last
}

get.stem <- function(path)
{
  parts <- strsplit(basename(path), "\\.")[[1]]
  last <- paste(parts[1:length(parts)-1], collapse=".")
  last
}


