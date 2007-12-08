## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

updateSummaryPage <- function() {
	TXV <- theWidget("summary_textview")
	TRV <- theWidget("summary_treeview")
	
	setTextview(TXV, "")
	TRV$setModel(rGtkDataFrame())
	
	StateEnv$update$summary <- F
	StateEnv$win$present()
	
	if (length(hsp$data) == 0) {
		return()
	}
	
	# generate summary
	missingSummary <- capture.output(
		missingFrac <- guiDo(summaryMissing.timeblobs(hsp$data, 
			timelim=hsp$timePeriod), doLog=F)
	)
	addTextview(TXV, paste(missingSummary, collapse="\n"))
	
	dfName <- dfMin <- dfQ25 <- dfMedian <- dfQ75 <- dfMax <- dfMissing <- character(length(hsp$data))
	
	for (i in seq(along=hsp$data)) {
		dfName[i] <- names(hsp$data)[i]
		subBlob <- window(hsp$data[[i]], hsp$timePeriod[1], hsp$timePeriod[2])
		myQuantiles <- round(quantile(
			subBlob$Data, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm=T), 
			digits=1)
		dfMin[i] <- myQuantiles[1]
		dfQ25[i] <- myQuantiles[2]
		dfMedian[i] <- myQuantiles[3]
		dfQ75[i] <- myQuantiles[4]
		dfMax[i] <- myQuantiles[5]
		dfMissing[i] <- sprintf('%.1f%%', missingFrac[i]*100)
	}
	
	dfModel <- rGtkDataFrame(data.frame(
		Name=dfName,
		Min=dfMin,
		Q25=dfQ25,
		Median=dfMedian,
		Q75=dfQ75,
		Max=dfMax,
		Missing=dfMissing,
		stringsAsFactors=F)
		)
	TRV$setModel(dfModel)
	
}

