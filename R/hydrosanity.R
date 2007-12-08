## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GUI based on Rattle (c) 2006 Graham.Williams@togaware.com
##

MAJOR <- "0"
MINOR <- "8"
REVISION <- unlist(strsplit("$Revision: 76 $", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- paste("(c) 2007 Felix Andrews <felix@nfrac.org>\n",
	" GUI based on Rattle (c) 2006 Graham.Williams@togaware.com")
WEBSITE <- "http://hydrosanity.googlecode.com/"

## LICENSE
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version. See the file gpl-license.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

TIMESERIES.FORMATS <- list(
	".au BoM daily rainfall"=
		c('read.timeblob',
		  'skip=1, sep=",", dataname="Rain (mm/day)", dataCol=6, qualCol=7, extraCols=c(9), extraNames=c("AccumSteps"), readTimesFromFile=F, startTime=list(year=3,month=4,day=5), timeSeqBy="DSTdays", timeOffset=as.difftime(9-24, units="hours")'),
	".au NSW Pinneena v8 streamflow (ML/day, default time format)"=
		c('read.timeblob',
		  'skip=3, sep=",", dataname="Flow (ML/day)", timeFormat="%H:%M_%d/%m/%Y", na.strings=c(\'""\')')
)

SITELIST.FORMATS <- list(
	".au BoM daily rainfall"='select.sites.BOM.AU'
)

# this stores application (non-project) state information
if (!exists("StateEnv", environment(), inherits=FALSE)) {
	StateEnv <- new.env()
}

hydrosanity <- function(project = NULL) {
	if (!is.null(StateEnv$win)) {
		.hs_on_menu_quit_activate()
	}
	
	StateEnv$update <- list(
		import=F,
		timeperiod=F,
		summary=F,
		explore=F,
		impute=F,
		rain=F,
		multivar=F,
		corr=F
	)
	
	StateEnv$echo.to.console <- T
	StateEnv$echo.to.log <- T
	
	StateEnv$GUI <- gladeXMLNew(getpackagefile("hydrosanity.glade"),
		root="hs_window")
	StateEnv$win <- theWidget("hs_window")
	
	# connect the callbacks (event handlers)
	gladeXMLSignalAutoconnect(StateEnv$GUI)
	gSignalConnect(StateEnv$win, "delete-event", .hs_on_menu_quit_activate)
	
	freezeGUI()
	on.exit(thawGUI())
	
	# set up log page
	addInitialLogMessage()
	
	# create empty project variable
	guiDo(hsp <- list(data=list(), modified=F))
	
	theWidget("welcome_label")$setMarkup(paste(sep='',
		'<span foreground="#660066"><big><b>Welcome to Hydrosanity</b></big></span>', 
		' version ', VERSION, '\n', 
		gsub('[<>]','',COPYRIGHT), '\n', 
		'<tt>', WEBSITE, '</tt>'))
	
	# set up initial GUI state
	theWidget("notebook")$setCurrentPage(0)
	theWidget("import_file_radio_options_notebook")$setShowTabs(FALSE)
	theWidget("import_options_expander")$setExpanded(FALSE)
	theWidget("import_edit_expander")$setExpanded(FALSE)
	setIsImportMode(TRUE)
	
	known_format_combo <- theWidget("import_known_format_combobox")
	known_format_combo$getModel()$clear()
	for (x in names(TIMESERIES.FORMATS)) {
		known_format_combo$appendText(x)
	}
	known_format_combo$setActive(0)
	theWidget("import_time_format_comboboxentry")$setActive(0)
	theWidget("import_time_format_codes_combobox")$setActive(0)
	theWidget("import_time_step_comboboxentry")$setActive(4)
	theWidget("import_makefactor_comboboxentry")$setActive(0)
	theWidget("import_accum_gaps_comboboxentry")$setActive(3)
	theWidget("import_transform_timestep_comboboxentry")$setActive(0)
	theWidget("import_transform_aggrfun_combobox")$setActive(0)
	theWidget("import_transform_qualfun_combobox")$setActive(0)
	theWidget("import_transform_ratio_timestep_comboboxentry")$setActive(3)
	theWidget("export_time_format_comboboxentry")$setActive(0)
	theWidget("export_time_format_codes_combobox")$setActive(0)

	sitelist_format_combo <- theWidget("scope_sitelist_format_combobox")
	sitelist_format_combo$getModel()$clear()
	for (x in names(SITELIST.FORMATS)) {
		sitelist_format_combo$appendText(x)
	}
	sitelist_format_combo$setActive(0)
	theWidget("scope_sitearchive_type_combobox")$setActive(0)
	#theWidget("scope_yearstart_combobox")$setActive(0)
	
	theWidget("explore_timeseries_aggr1_comboboxentry")$setActive(2)
	theWidget("explore_timeseries_aggr2_comboboxentry")$setActive(4)
	theWidget("explore_cdf_aggr1_radiobutton")$setActive(T)
	theWidget("explore_cdf_aggr1_comboboxentry")$setActive(2)
	theWidget("explore_cdf_aggr2_comboboxentry")$setActive(4)
	theWidget("impute_aggr1_comboboxentry")$setActive(2)
	theWidget("impute_aggr2_comboboxentry")$setActive(4)
	theWidget("impute_missing_gaps_comboboxentry")$setActive(0)
	theWidget("impute_missing_constant_combobox")$setActive(0)
	theWidget("multivar_relationplot_aggr1_radiobutton")$setActive(T)
	theWidget("multivar_relationplot_lag_comboboxentry")$setActive(0)
	theWidget("multivar_relationplot_aggr1_comboboxentry")$setActive(2)
	theWidget("multivar_relationplot_aggr2_comboboxentry")$setActive(4)
	theWidget("corr_smoothed_by_comboboxentry")$setActive(1)
	theWidget("corr_relationplot_lag_comboboxentry")$setActive(0)
	theWidget("corr_relationplot_aggr1_comboboxentry")$setActive(2)
	theWidget("corr_relationplot_aggr2_comboboxentry")$setActive(4)
	
	setTextviewMonospace(theWidget("log_textview"))
	setTextviewMonospace(theWidget("impute_textview"))
	setTextviewMonospace(theWidget("corr_contiguous_textview"))
	
	# set up table format on import page
	importTreeView <- theWidget("import_summary_treeview")
	insertTreeViewTextColumns(importTreeView, 
		colNames=c("ID", "Name", "Start", "End", "Length", "Timestep", "Location_X.Y.Z", "Qual", "Extra_data", "Role", "Data"),
		editors=list(ID=.hs_on_import_summary_treeview_id_edited,
			Name=.hs_on_import_summary_treeview_sitename_edited,
			Data=.hs_on_import_summary_treeview_dataname_edited,
			Role=.hs_on_import_summary_treeview_role_edited),
		combo=list(Role=data.frame(c("FLOW","RAIN","AREAL","OTHER"))) )
	importTreeView$getSelection()$setMode("multiple")
	try(importTreeView$setRubberBanding(T), silent=T) # not in 2.8.7
	importTreeView$setRulesHint(T)
	
	# set up table format on timeperiod page
	timeperiodTreeView <- theWidget("summary_treeview")
	insertTreeViewTextColumns(timeperiodTreeView, 
		colNames=c("Name", "Min", "Q25", "Median", "Q75", "Max", "Missing", ""))
	
	# open the project file if given
	if (!is.null(project)) {
		openProject(project)
	}
	
	updateNow()
	StateEnv$win$present()
	return(invisible(NULL))
}

addInitialLogMessage <- function() {
	# don't echo this to the console
	oldSetting <- StateEnv$echo.to.console
	on.exit(StateEnv$echo.to.console <- oldSetting)
	StateEnv$echo.to.console <- F
	
	addToLog(paste(
"## Hydrosanity version", VERSION, "\n",
"## Run by", Sys.info()["user"], "on", R.version.string, "\n\n",
"## This log keeps a record of the analysis procedure. You can edit it or 
## annotate it here in this frame. You can also export it to a file using the 
## export button. Saving the Hydrosanity project also retains this log. To run 
## commands again, copy and paste into the R Console.

library(hydrosanity)

## The variable hsp is used to store the current Hydrosanity Project. At any 
## time, type \"str(hsp)\" in the R Console to see what is stored there!"
	))
	addLogSeparator()
}

setIsImportMode <- function(isImportMode) {
	theWidget("timeperiod_scope_expander")$setExpanded(isImportMode)
	for (x in c("import_import_expander", "import_edit_expander",
		"import_transform_expander", "import_export_expander")) {
		theWidget(x)$setExpanded(FALSE)
	}
	if (isImportMode) {
		theWidget("import_import_expander")$setExpanded(TRUE)
	} else {
		theWidget("import_edit_expander")$setExpanded(TRUE)
	}
}

updateNow <- function(page.num=theWidget("notebook")$getCurrentPage()) {
	if (page.num %in% c(0, 1, 3, 9)) {
		theWidget("selection_frame")$setSensitive(FALSE)
	} else {
		theWidget("selection_frame")$setSensitive(TRUE)
	}
	
	if (page.num == 1) {
		if (StateEnv$update$import) { updateImportPage() }
	}
	if (page.num == 2) {
		if (StateEnv$update$timeperiod) { updateTimePeriodPage() }
	}
	if (page.num == 3) {
		if (StateEnv$update$summary) { updateSummaryPage() }
	}
	if (page.num == 4) {
		if (StateEnv$update$explore) { updateExplorePage() }
	}
	if (page.num == 5) {
		if (StateEnv$update$impute) { updateImputePage() }
	}
	if (page.num == 6) {
		if (StateEnv$update$rain) { updateRainPage() }
	}
	if (page.num == 7) {
		if (StateEnv$update$multivar) { updateMultivarPage() }
	}
	if (page.num == 8) {
		if (StateEnv$update$corr) { updateCorrPage() }
	}
}

datasetModificationUpdate <- function() {
	hsp$modified <<- T
	
	# set all pages to be updated
	StateEnv$update[] <- T
	
	# sort hsp$data by name
	if (is.unsorted(names(hsp$data))) {
		hsp$data <<- hsp$data[order(names(hsp$data))]
	}
	
	selection_iconview <- theWidget("selection_iconview")
	setupIconView(selection_iconview)
	if (length(iconViewGetSelectedNames(selection_iconview)) == 0) {
		iconViewSetSelection(selection_iconview, "first")
	}
	
	updateNow()
}

timeperiodModificationUpdate <- function() {
	hsp$modified <<- T
	
	# set all pages except "dataset" to be updated
	tmp <- StateEnv$update$import
	StateEnv$update[] <- T
	StateEnv$update$import <- tmp
	
	updateNow()
}

regionModificationUpdate <- function() {
	hsp$modified <<- T
	
	# set all pages except "dataset" to be updated
	tmp <- StateEnv$update$import
	StateEnv$update[] <- T
	StateEnv$update$import <- tmp
	
	updateNow()
}

.hs_on_select_all_button_clicked <- function(button) {
	theWidget("selection_iconview")$selectAll()
}

.hs_on_select_all_rain_button_clicked <- function(button) {
	iconViewSetSelection(theWidget("selection_iconview"), "rain")
}

.hs_on_menu_update_activate <- function(...) {
	datasetModificationUpdate()
}

.hs_on_notebook_switch_page <- function(widget, page, page.num, ...) {
	freezeGUI()
	on.exit(thawGUI())
	
	updateNow(page.num=page.num)
}

.hs_on_menu_quit_activate <- function(action, window) {
	freezeGUI()
	if (exists("hsp") && hsp$modified && (length(hsp$data) > 0)) {
		#if (gconfirm("Save project?")) {
		if (!is.null(questionDialog("Save project?"))) {
			saveProject()
		}
	}
	# TODO: only destroy those that are owned by hydrosanity
	for (x in playDevList()) playDevOff(x)
	#for (x in ls(plotAndPlayGTK:::StateEnv)) {
	#	try(plotAndPlayGTK:::StateEnv[[x]]$win$destroy(), silent=TRUE)
	#}
	StateEnv$win$destroy()
	rm(win, GUI, envir=StateEnv)
}

.hs_on_menu_about_activate <-  function(action, window) {
	about <- gladeXMLNew(getpackagefile("hydrosanity.glade"), 
		root="aboutdialog")
	about$getWidget("aboutdialog")$setVersion(VERSION)
	about$getWidget("aboutdialog")$setCopyright(COPYRIGHT)
	about$getWidget("aboutdialog")$setWebsite(WEBSITE)
}

.hs_on_export_log_button_clicked <- function(button) {
	freezeGUI()
	on.exit(thawGUI())
	
	filename <- choose.file.save("log.R", caption="Export Log", 
		filters=Filters[c("R","txt","All"),])
	StateEnv$win$present()
	if (is.na(filename)) return()
	
	if (get.extension(filename) == "") {
		filename <- paste(filename, "R", sep=".")
	}
	
	write(getTextviewText(theWidget("log_textview")), filename)
	
	setStatusBar("The log has been exported to ", filename)
}


sanitycheck.rain <- function(timeblobList) {
	if (is.data.frame(timeblobList)) { timeblobList <- list(timeblobList) }
	for (k in seq(along=timeblobList)) {
		cat("Sanity checking rainfall series", k, "(", names(timeblobList)[k], ") ...\n")
		rawdata <- timeblobList[[k]]$Data
		pctiles <- quantile(rawdata, na.rm=T)
		if (pctiles[["0%"]] != 0) {
			cat("  INSANITY: minimum not zero:", pctiles[["0%"]], "\n")
		} else {
			if (pctiles[["100%"]] < 1) {
				cat("  INSANITY: maximum less than 1:", pctiles[["100%"]], "\n")
			}
			if (pctiles[["50%"]] > 0) {
				cat("  INSANITY: median greater than zero:", pctiles[["50%"]], "\n")
			}
		}
	}
}


sanitycheck.flow <- function(timeblobList) {
	if (is.data.frame(timeblobList)) { timeblobList <- list(timeblobList) }
	for (k in seq(along=timeblobList)) {
		cat("Sanity checking streamflow series", k, "(", names(timeblobList)[k], ") ...\n")
		rawdata <- timeblobList[[k]]$Data
		pctiles <- quantile(rawdata, na.rm=T)
		if (pctiles[["0%"]] < 0) {
			cat("  INSANITY: minimum less than zero:", pctiles[["0%"]], "\n")
		} else {
			if (pctiles[["100%"]] < 1) {
				cat("  INSANITY: maximum less than 1:", pctiles[["100%"]], "\n")
			}
			if (mean(rawdata, na.rm=T) < 2 * pctiles[["50%"]]) {
				cat("  INSANITY: mean less than twice median (i.e. low skewness)", "\n")
			}
		}
	}
}


getpackagefile <- function(filename) {
	## Try firstly to load from the installed hydrosanity package
	## Otherwise, look locally.
	myPath <- system.file("etc", filename, package = "hydrosanity")
	if (identical(myPath, "")) 
		myPath <- file.path("hydrosanity", "hydrosanity", "inst", 
			"etc", filename)
	if (!file.exists(myPath)) stop("could not find file ", filename)
	myPath
}


select.sites.BOM.AU <- function(siteListFile, archivePath, return.data=FALSE, xlim=NULL, ylim=NULL, timelim=NULL, min.years=NA) {
	
	nCol <- 22
	colNames <- paste("V", 1:nCol, sep="")
	colClass <- rep("NULL", nCol)
	colNames[2] <- "id"
	colClass[2] <- "character"
	colNames[4] <- "name"
	colClass[4] <- "character"
	colNames[c(7:8,11)] <- c("y","x","elev")
	colClass[c(7:8,11)] <- "numeric"
	colNames[14:15] <- c("first.year","last.year")
	colClass[14:15] <- "numeric"
	siteinfo <- read.table(siteListFile, sep=',', quote="", header=F, 
		strip.white=T, col.names=colNames, colClasses=colClass)
	wordCaps <- function(x) {
		gsub("\\b(\\w)", "\\U\\1", tolower(x), perl=TRUE)
	}
	siteinfo$name <- make.unique(wordCaps(siteinfo$name))
	row.names(siteinfo) <- siteinfo$name
	
	# apply selection criteria
	ok <- rep(TRUE, nrow(siteinfo))
	if (!is.null(xlim)) {
		ok <- ok & (siteinfo$x >= min(xlim)) & (siteinfo$x <= max(xlim))
	}
	if (!is.null(ylim)) {
		ok <- ok & (siteinfo$y >= min(ylim)) & (siteinfo$y <= max(ylim))
	}
	if (!is.na(min.years)) {
		firstYear <- siteinfo$first.year
		lastYear <- siteinfo$last.year
		if (!is.null(timelim)) {
			yearlim <- as.POSIXlt(timelim)$year + 1900
			firstYear <- pmax(firstYear, yearlim[1])
			lastYear <- pmin(lastYear, yearlim[2])
		}
		nYears <- lastYear - firstYear + 1
		ok <- ok & (nYears >= min.years)
	}
	siteinfo$ok <- ok
	
	if (return.data == FALSE) {
		return(siteinfo)
	}
	
	siteinfo <- siteinfo[ok,]
	
	# read in the data
	dataset <- list()
	nSites <- nrow(siteinfo)
	for (i in seq(along=siteinfo$id)) {
		x <- siteinfo$id[i]
		filename <- paste('dr_', siteinfo$id[i], '.txt', sep='')
		cat("reading file", i, "/", nSites, ":", filename, "...\n")
		if (tolower(get.extension(archivePath)) == "zip") {
			fileConn <- unz(archivePath, filename, open="")
		} else {
			fileConn <- file.path(archivePath, filename)
		}
		dataset[[x]] <- read.timeblob(fileConn, skip=1, sep=",", 
			sitename=siteinfo$name[i], dataname="Rain (mm/day)", 
			dataCol=6, qualCol=7, extraCols=c(9), 
			extraNames=c("AccumSteps"), readTimesFromFile=F, 
			startTime=list(year=3, month=4, day=5),
			timeOffset=as.difftime(9 - 24, units="hours"))
		# observation covers 24 hours to 9am on the nominal day
		# but the time series format requires that the time is the start
		#dataset[[x]]$Time <- dataset[[x]]$Time + 
		#	as.difftime(9 - 24, units="hours")
		attr(dataset[[x]], "role") <- "RAIN"
		myLoc <- c(siteinfo$x[i], siteinfo$y[i])
		if (!any(is.na(myLoc))) {
			attr(dataset[[x]], "location.xy") <- myLoc
		}
		if (!is.na(siteinfo$elev[i])) {
			attr(dataset[[x]], "elevation") <- siteinfo$elev[i]
		}
	}
	
	return(dataset)
}

