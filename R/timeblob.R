## Hydrosanity: an interface for exploring hydrological time series in R
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL


timeblob <- function(Time, Data, Qual=NULL, extras=NULL, timestep=NULL, sitename="Unknown", dataname="Data") {
	# check types
	Time <- as.POSIXct(Time)
	if (any(is.na(Time))) { stop("'Time' must be a vector of valid times (POSIXt)") }
	if (is.list(Data)) {
		extras <- Data[-1]
		Data <- Data[[1]]
	}
	if (!is.numeric(Data)) { stop("'Data' must be numeric") }
	# construct timeblob
	blob <- data.frame(
		Time=Time,
		Data=Data
	)
	if (!is.null(Qual)) {
		blob$Qual <- Qual
	}
	if (!is.null(extras) && (length(extras) > 0)) {
		blob <- cbind(blob, extras)
	}
	class(blob) <- c("timeblob", "data.frame")
	# set timestep
	if (is.null(timestep)) {
		timestep <- as.byString(Time[2] - Time[1], digits=1)
		# check whether series is irregular
		timeStepDiffs <- diff(as.numeric(Time))
		# trim by 10% because of anomolies in seq.POSIXt with DSTdays
		timeStepRange <- c(quantile(timeStepDiffs, c(0.1, 0.9)))
		# up to 11% difference expected in regular series (feb vs jan)
		if (timeStepRange[2] > 1.11 * timeStepRange[1]) {
			timestep <- "irregular"
		}
	}
	attr(blob, "timestep") <- timestep
	attr(blob, "sitename") <- sitename
	attr(blob, "dataname") <- dataname
	attr(blob, "role") <- "OTHER"
	# check that it is.timeblob()!
	if (!is.timeblob(blob)) { stop("oops, timeblob() function made an invalid timeblob") }
	return(blob)
}

is.timeblob <- function(x) {
	!is.null(x) &&
	inherits(x, "timeblob") &&
	is.data.frame(x) &&
	(ncol(x) >= 2) &&
	inherits(x$Time, "POSIXct") &&
	is.numeric(x$Data) &&
	!is.null(attr(x, "timestep")) &&
	!is.null(attr(x, "sitename")) &&
	!is.null(attr(x, "dataname"))
}

lapply.timeblob.data <- function(blob.list, FUN, ...) {
	# check types
	if (!identical(class(blob.list),"list")) blob.list <- list(blob.list)
	if (any(sapply(blob.list, is.timeblob)==F)) stop("'blob.list' must be a list of timeblobs")
	# lapply to blob data
	lapply(lapply(blob.list, function(x) x$Data), FUN, ...)
}

sapply.timeblob.data <- function(blob.list, FUN, ...) {
	# check types
	if (!identical(class(blob.list),"list")) blob.list <- list(blob.list)
	if (any(sapply(blob.list, is.timeblob)==F)) stop("'blob.list' must be a list of timeblobs")
	# sapply to blob data
	sapply(lapply(blob.list, function(x) x$Data), FUN, ...)
}

read.timeblob <- function(file, skip=1, sep=",", sitename=NULL, dataname="Data", dataCol=2, qualCol=3, extraCols=c(), extraNames=paste("Extra",extraCols), readTimesFromFile=T, timeCol=1, timeFormat="%d %b %Y", startTime=NA, tz="GMT", timeSeqBy="days", ...) {
	# check types
	if (is.null(sitename)) {
		if (inherits(file, "connection")) {
			sitename <- make.names(deparse(substitute(file)))
		} else {
			sitename <- get.stem(file)
		}
	}
	if (!is.numeric(dataCol)) { stop("'dataCol' must be numeric (column number)") }
	if (readTimesFromFile) {
		if (!is.numeric(timeCol)) { stop("'timeCol' must be numeric (column number)") }
	} else {
		if (!is.list(startTime)) {
			startTime <- as.POSIXct(startTime, tz=tz)
			if (is.na(startTime)) { stop("could not convert 'startTime' to a time") }
		}
	}
	# unz seems to have problems, so just read in the whole file
	if (inherits(file, "unz")) { 
		fileText <- readLines(file)
		close(file)
		file <- textConnection(fileText)
	}
	# make sure extra column names correspond to given columns
	length(extraNames) <- length(extraCols)
	extraNames[is.na(extraNames)] <- paste("Extra", which(is.na(extraNames)))
	# number of columns in file
	fileCols <- 200 # assumed maximum
	if (!inherits(file, "unz")) { #isSeekable(file)) {
		firstLine <- read.table(file, header=F, skip=skip, sep=sep, strip.white=T, nrows=1, ...)
		fileCols <- ncol(firstLine)
	}
	if (dataCol > fileCols) {
		stop("Column ", dataCol, " ('dataCol') not found on line ", skip+1, 
		"; maybe 'sep'=\"", sep, "\" or 'skip'=", skip, " is wrong?")
	}
	# drop variables for which column does not exist in file
	if (qualCol > fileCols) { qualCol <- NULL }
	extraNames <- extraNames[!(extraCols > fileCols)]
	extraCols <- extraCols[!(extraCols > fileCols)]
	# define which columns to import and which to ignore
	fileColClasses <- rep("NULL", fileCols)
	if (readTimesFromFile) { fileColClasses[timeCol] <- "character" }
	fileColClasses[dataCol] <- "numeric"
	fileColClasses[qualCol] <- NA # note qualCol may be NULL
	fileColClasses[extraCols] <- NA # note extraCols may be NULL
	# read file
	rawData <- read.table(file, header=F, skip=skip, sep=sep, colClasses=fileColClasses, strip.white=T, ...)
	# work out which column of rawData has the data (from dataCol)
	dataIndex <- dataCol - sum(fileColClasses[1:dataCol]=="NULL", na.rm=T)
	qualIndex <- NULL
	timeIndex <- NULL
	# extract quality codes or set to default (factor("NA"))
	myQual <- NA
	if (!is.null(qualCol) && !is.na(qualCol)) {
		qualIndex <- qualCol - sum(fileColClasses[1:qualCol]=="NULL", na.rm=T)
		myQual <- rawData[[qualIndex]]
		if (is.factor(myQual)) {
			myQual <- factor(myQual, exclude=NULL)
		}
	} else {
		myQual <- rep(factor(NA, exclude=NULL), nrow(rawData))
	}
	# convert or construct the time sequence
	myTime <- NA
	if (readTimesFromFile) {
		timeIndex <- timeCol - sum(fileColClasses[1:timeCol]=="NULL", na.rm=T)
		myTime <- strptime(rawData[[timeIndex]], format=timeFormat, tz=tz)
		if (any(is.na(myTime))) {
			firstNA <- which(is.na(myTime))[1]
			stop('could not convert "', rawData[firstNA,timeIndex],
			'" to time with format string "', timeFormat, '"')
		}
	} else {
		if ("list" %in% class(startTime)) {
			timeBits <- lapply(startTime, function(i) {
				if (is.numeric(i)) { firstLine[1,i] } else { i }
			})
			if (is.null(timeBits$hour)) { timeBits$hour <- 0 }
			if (is.null(timeBits$min)) { timeBits$min <- 0 }
			if (is.null(timeBits$sec)) { timeBits$sec <- 0 }
			timeBits$tz <- tz
			startTime <- do.call(ISOdatetime, timeBits)
			if (is.na(startTime)) {
				myBits <- paste(paste(names(unlist(timeBits)), '=', unlist(timeBits)), collapse=', ')
				stop("could not construct starting time from columns given in 'startTime': ", myBits)
			}
		}
		myTime <- seq.POSIXt(from=startTime, by=timeSeqBy, length=nrow(rawData))
	}
	extras <- rawData[-c(timeIndex, dataIndex, qualIndex)]
	names(extras) <- extraNames
	blob <- timeblob(Time=myTime, Data=rawData[[dataIndex]], Qual=myQual, 
		extras=extras, sitename=sitename, dataname=dataname)
	return(blob)
}

# returns length 0 if x is empty
start.timeblob <- function(x, ...) {
	#if (!is.timeblob(x)) { stop("'x' must be a timeblob") }
	x$Time[min(1,nrow(x))]
}

# returns length 0 if x is empty
end.timeblob <- function(x, ...) {
	#if (!is.timeblob(x)) { stop("'x' must be a timeblob") }
	if (identical(attr(x, "timestep"), "irregular")) {
		x$Time[nrow(x)]
	} else {
		if (nrow(x)==0) { return(x$Time[0]) }
		# extrapolate last time step
		seq.POSIXt(from=x$Time[nrow(x)], by=attr(x, "timestep"), 
			length=2)[2]
	}
}

start.timeblobs <- function(x, ...) {
	# check types
	if (!identical(class(x),"list")) { x <- list(x) }
	globalStart <- as.POSIXct(min(unlist(lapply(x, start.timeblob))))
	return(globalStart)
}

end.timeblobs <- function(x, ...) {
	# check types
	if (!identical(class(x),"list")) { x <- list(x) }
	globalEnd <- as.POSIXct(max(unlist(lapply(x, end.timeblob))))
	return(globalEnd)
}

timelim.timeblobs <- function(x) {
	# NOTE: can't use c() because it strips the "tzone" attribute
	tmp <- start.timeblobs(x)
	tmp[2] <- end.timeblobs(x)
	tmp
}


window.timeblob <- function(x, start=NULL, end=NULL, inclusive=F, return.indices=F, extend=F, ...) {
	# check types
	if (!is.timeblob(x)) { stop("'x' must be a timeblob") }
	if (is.null(start)) { start <- start(x) }
	if (is.null(end)) { end <- end(x) }
	start <- as.POSIXct(start)
	end <- as.POSIXct(end)
	if (any(is.na(c(start, end)))) { stop("'start' and 'end' must be valid times (POSIXt)") }
	if (extend) {
		# pad with NA values out to specified limits
		timestep <- attr(x, "timestep")
		if (is.null(timestep)) {
			stop("'x' needs a timestep attribute for 'extend=T'")
		}
		negTimestep <- paste("-1", timestep)
		if (any(grep("^[0-9]", timestep))) {
			negTimestep <- paste("-", timestep, sep='')
		}
		if (start < start(x)) {
			extendTimes <- seq.POSIXt(start(x), start, by=negTimestep)[-1]
			extendTimes <- rev(extendTimes)
			extendBlob <- x[c(0,rep(NA,length(extendTimes))),]
			extendBlob$Time <- extendTimes
			x <- rbind(extendBlob, x)
		}
		if (end > end(x)) {
			extendTimes <- seq.POSIXt(end(x), end, by=timestep)[-1]
			extendBlob <- x[c(0,rep(NA,length(extendTimes))),]
			extendBlob$Time <- extendTimes
			x <- rbind(x, extendBlob)
		}
	}
	
	windowIdx <- findIntervalRange(start, end, x$Time, inclusive=inclusive)
	
	if (!identical(attr(x, "timestep"), "irregular")) {
		# TODO: need to handle last time step inclusive
	}
	if (return.indices) {
		return(windowIdx)
	}
	return(x[seq(windowIdx[1],windowIdx[2]),])
}

findIntervalRange <- function(xLo, xHi, vec, inclusive=F) {
	if (xLo > xHi) stop("'xHi' must be greater than 'xLo'")
	# check whether vec has any elements
	if (length(vec)==0) return(c(0,0))
	# check whether the period intersects at all with 'vec'
	if ((xHi < vec[1]) || (xLo > vec[length(vec)])) return(c(0,0))
	windowIdx <- findInterval(c(xLo,xHi), vec)
	if (inclusive) {
		# round up at end (findInterval rounds down)
		test <- vec[windowIdx[2]]
		if ((length(test)>0) && (test != xHi) 
		&& (windowIdx[2] < length(vec))) {
			windowIdx[2] <- windowIdx[2] + 1
		}
	} else {
		# round up at start (findInterval rounds down)
		test <- vec[windowIdx[1]]
		if ((length(test)>0) && (test != xLo)
		&& (windowIdx[1] < length(vec))) {
			windowIdx[1] <- windowIdx[1] + 1
		}
	}
	# if there are no complete intervals (inclusive==F)
	if (windowIdx[1] > windowIdx[2]) return(c(0,0))
	return(windowIdx)
}


syncTo.timeblobs <- function(blob.list, blob, extractColumn="Data") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	# construct data frame
	syncData <- data.frame(
		Time=blob$Time,
		lapply(blob.list, function(x) {
			mySyncIndices <- matchtimes.timeblob(x, blob$Time)
			x[mySyncIndices, extractColumn]
		})
	)
	attr(syncData, "timestep") <- attr(blob, "timestep")
	return(syncData)
}



# timestep needs to be fast enough for all blobs (does not aggregate to slower time steps)
sync.timeblobs <- function(blob.list, timestep=NULL, timelim=NULL, extractColumn="Data") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- timelim.timeblobs(blob.list)
	} else {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window, min(timelim), max(timelim))
	}
	# setup
	if (is.null(timestep)) {
		timestep <- common.timestep.timeblobs(blob.list)
	}
	times <- seq.POSIXt(min(timelim), max(timelim), by=timestep)
	# omit last time since the 'timelim' extends to *end* of last period
	times <- times[-length(times)]
	# construct data frame
	syncData <- data.frame(
		Time=times,
		lapply(blob.list, function(x) {
			mySyncIndices <- matchtimes.timeblob(x, times)
			x[mySyncIndices, extractColumn]
		})
	)
	attr(syncData, "timestep") <- timestep
	return(syncData)
}

# find indices into timeblob 'blob' for each time in 'times'
# values of NA are used for times outside 'blob'
#
# it resamples blob$Time to correspond to each time in 'times'
#
# x <- seq(as.POSIXct("1970-01-01"), by="2 years", length=5)
# blob <- timeblob(Time=x, Data=seq(70, by=2, length=5))
# yearseq <- seq(as.POSIXct("1965-01-01"), by="years", length=10)
# data.frame(yearseq, 
# 	blob.index=matchtimes.timeblob(blob, yearseq), 
# 	blob.data=blob$Data[matchtimes.timeblob(blob, yearseq)])
#
# sixseq <- seq(as.POSIXct("1970-06-06"), as.POSIXct("1981-06-06"), by="6 months")
# data.frame(sixseq, 
# 	blob.index=matchtimes.timeblob(blob, sixseq), 
# 	blob.data=blob$Data[matchtimes.timeblob(blob, sixseq)])

matchtimes.timeblob <- function(blob, times) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	times <- as.POSIXct(times)
	if (any(is.na(times))) { stop("'times' must be a vector of valid times (POSIXt)") }
	# default for indices is NA
	periodIndices <- rep(as.integer(NA), length(times))
	# each blob here may be outside 'times', and may be empty
	if ((nrow(blob)==0)
	  || (start(blob) > times[length(times)])
	  || (end(blob) < times[1])) {
		return(periodIndices)
	}
	# each blob here is not entirely outside 'times'
	blobBounds <- findIntervalRange(start(blob), end(blob), times)
	blobWindowIndices <- seq(blobBounds[1], blobBounds[2])
	# now times[blobWindowIndices] is not outside blob$Time
	periodIndices[blobWindowIndices] <- findInterval(times[blobWindowIndices], blob$Time)
	return(periodIndices)
}

common.timestep.timeblobs <- function(blob.list, default="DSTdays") {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	timestep <- NULL
	for (i in seq(along=blob.list)) {
		if (identical(attr(blob.list[[i]], "timestep"), "irregular")) { next }
		thisStepDelta <- as.numeric.byString(
			attr(blob.list[[i]], "timestep"))
		if (is.null(timestep) || (thisStepDelta < timestep)) {
			timestep <- thisStepDelta
		}
	}
	if (is.null(timestep)) { return(default) }
	else { return(as.byString(timestep)) }
}


# invisibly returns missing fraction for each series
summaryMissing.timeblobs <- function(blob.list, timelim=NULL, timestep=NULL) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (is.null(timelim)) {
		timelim <- timelim.timeblobs(blob.list)
	} else {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window, timelim[1], timelim[2])
	}
	# setup
	nBlobs <- length(blob.list)
	if (is.null(timestep)) {
		timestep <- common.timestep.timeblobs(blob.list)
	}
	times <- seq.POSIXt(timelim[1], timelim[2], by=timestep)
	myLengths <- sapply(blob.list, nrow)
	myNAs <- lapply(blob.list, function(x) { is.na(x$Data) })
	myDataPoints <- myLengths - sapply(myNAs, sum)
	myDataFrac <- myDataPoints / length(times)
	overallDataFrac <- mean(myDataFrac)
	myCompleteN <- sum(myDataFrac >= 1)
	my95PctN <- sum(myDataFrac > 0.95)
	my75PctN <- sum(myDataFrac > 0.75)
	
	# find whether data exists for each timeblob for each time in myPeriod
	# (note: this rounds down if times do not match)
	dataMatrix <- sync.timeblobs(blob.list, timestep=timestep)
	dataExistsMatrix <- !is.na(as.matrix(dataMatrix[-1]))
	
	activeNs <- apply(dataExistsMatrix, 1, sum)
	allActiveSteps <- sum(activeNs == nBlobs)
	allActiveFrac <- allActiveSteps / length(times)
	activeNQ <- quantile(activeNs, probs=c(0.25, 0.5, 0.75))
	activeNQFrac <- activeNQ / nBlobs
	
	cat(sprintf('Overall, %.0f%% of data is missing.\n', (1-overallDataFrac)*100))
	cat(sprintf('There are %i time series, of which %i %s complete.\n', 
		nBlobs, myCompleteN, ifelse(myCompleteN==1,'is','are')))
	cat(sprintf('...%i %s > 95%% complete and %i %s > 75%% complete.\n', 
		my95PctN, ifelse(my95PctN==1,'is','are'), my75PctN, ifelse(my75PctN==1,'is','are')))
	cat('\n')
	cat(sprintf('%i time steps (%.1f%%) have data from all series.\n', allActiveSteps, allActiveFrac*100))
	cat(sprintf('The median number of active sites is %i (%.0f%%).\n', activeNQ[2], activeNQFrac[2]*100))
	cat(sprintf('...Half the time, the number of active sites is between %i and %i.\n', activeNQ[1], activeNQ[3]))
	#cat(sprintf('The number of active sites ranges from %i to %i.\n', activeNQ[1], activeNQ[3]))
	
	# gap length distribution
	
	# quality code summary
	
	# missing fraction for each series
	missingFrac <- (length(times) - apply(dataExistsMatrix, 2, sum)) / length(times)
	
	invisible(missingFrac)
}


# this only handles regular series (the calculation of NA proportion requires it)
aggregate.timeblob <- function(x, by="1 year", FUN=mean, fun.qual=c("worst","median","mode","omit"), start.month=1, ...) {
	# check types
	if (!is.timeblob(x)) stop("'x' must be a timeblob")
	fun.qual <- match.arg(fun.qual)
	if (is.null(start.month)) start.month <- 1
	if (!is.numeric(start.month)) stop("'start.month' must be a month number")
	# disaccumulate (redistribute values accumulated over multiple time-steps)
	# (just distribute evenly over gap, should be good enough for aggregating)
	if (!is.null(x$AccumSteps)) {
		x <- quick.disaccumulate.timeblob(x)
	}
	# adjust blob start time to specified calendar month
	if (any(grep(" month", by)) || any(grep("year", by))) {
		newStart <- as.POSIXlt(truncMonth(start(x)))
		origMonth <- newStart$mon
		newMonth <- start.month - 1 # convert to base-zero
		newStart$mon <- newMonth
		if (newMonth > origMonth) { newStart$year <- newStart$year - 1 }
		x <- window(x, start=newStart, extend=T)
		# can not use "years" in cut.POSIXt (uses calendar years only),
		# so convert to "months"
		if (any(grep("year", by))) {
			if (start.month != 1) {
				byBits <- strsplit(by, " ")[[1]]
				nYears <- 1
				if (length(byBits) == 2) {
					nYears <- as.numeric(byBits[1])
				}
				by <- paste(12 * nYears, "months")
			}
		}
	}
	# extend start and end times to find missing values in first and last groups
	newStart <- decr.POSIXt(start(x), by=attr(x, "timestep"))
	newEnd <- incr.POSIXt(end(x), by=attr(x, "timestep"))
	x <- window(x, start=newStart, end=newEnd, extend=T)
	# construct groups
	dateGroups <- cut.POSIXt(x$Time, breaks=by)
	newDates <- as.POSIXct(levels(dateGroups), tz=attr(x$Time, "tzone"))
	# aggregate
	aggrVars <- c("Data")
	hasExtraVars <- (length(names(x)) > 3)
	if (hasExtraVars) {
		aggrVars <- c(aggrVars, names(x)[-(1:3)])
	}
	allNewVals <- aggregate(as.data.frame(x[aggrVars]), 
		by=list(dateGroups), FUN=FUN, ...)[-1]
	# aggregate quality codes
	newQual <- NULL
	if (fun.qual != "omit") {
		FUN.Qual <- switch(fun.qual, 
			worst=function(x) {
				x <- x[!is.na(x)]
				if (length(x)>0) {
					rev(levels(x[,drop=T]))[1]
				} else { NA }
			},
			median=function(x) {
				x <- x[!is.na(x)]
				if (length(x)>0) {
					x <- as.ordered(x)
					half <- ceiling((length(x)+1)/2)
					as.character(sort(x, partial=half)[half])
				} else { NA }
			},
			mode=function(x) {
				x <- x[!is.na(x)]
				if (length(x)>0) {
					a <- table(x)
					rownames(a)[which.max(a)]
				} else { NA }
			})
		tmpQual <- x$Qual
		# ignore quality codes where data is missing
		tmpQual[is.na(x$Data)] <- NA
		# ignore disaccumulated values for aggregation
		tmpQual[tmpQual == "disaccumulated"] <- "good"
		# apply function fun.qual to each aggregated group
		newQual <- tapply(tmpQual, list(dateGroups), FUN=FUN.Qual)
		newQual[is.na(allNewVals[[1]])] <- NA
		# make sure new factor levels are ordered correctly
		oldLevels <- levels(x$Qual)
		newLevels <- unique(newQual)
		newLevels <- oldLevels[sort(match(newLevels, oldLevels))]
		newQual <- factor(newQual, levels=newLevels, ordered=T)
	}
	# construct new blob
	newBlob <- timeblob(Time=newDates, Data=allNewVals, Qual=newQual,
		timestep=by)
	attr(newBlob, "role") <- attr(x, "role")
	attr(newBlob, "dataname") <- attr(x, "dataname")
	attr(newBlob, "sitename") <- attr(x, "sitename")
	attr(newBlob, "location.xy") <- attr(x, "location.xy")
	attr(newBlob, "elevation") <- attr(x, "elevation")
	return(newBlob)
}

smooth.timeblob <- function(blob, by="1 year") {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	if (!is.null(blob$AccumSteps)) {
		blob <- quick.disaccumulate.timeblob(blob)
	}
	triangularFilter <- function(n) {
		tmp <- c(1:floor(n/2), ceiling(n/2):1)
		tmp / sum(tmp)
	}
	delta <- as.numeric.byString(attr(blob, "timestep"))
	smoothDelta <- as.numeric.byString(by)
	winSize <- round(smoothDelta / delta)
	tmp.filter <- triangularFilter(winSize)
	blob$Data <- filter(blob$Data, tmp.filter)
	blob
}

quick.disaccumulate.timeblob <- function(blob) {
	# check types
	if (!is.timeblob(blob)) { stop("'blob' must be a timeblob") }
	if (is.null(blob$AccumSteps)) { return(blob) }
	spanInfo <- data.frame(end=which(blob$AccumSteps > 1))
	if (nrow(spanInfo) == 0) {
		blob$AccumSteps <- NULL
		return(blob)
	}
	spanInfo$length <- blob$AccumSteps[spanInfo$end]
	spanInfo$start <- with(spanInfo, end - length + 1)
	spanInfo$accum <- blob$Data[spanInfo$end]
	# concatenated indices of all the time steps in accums
	allSpans <- expand.indices(spanInfo)
	# evenly redistribute
	blob$Data[allSpans] <- with(spanInfo, rep(accum / length, times=length))
	levels(blob$Qual) <- union(levels(blob$Qual), "disaccumulated")
	blob$Qual[allSpans] <- "disaccumulated"
	blob$AccumSteps <- NULL
	return(blob)
}

impute.timeblobs <- function(blob.list, which.impute=names(blob.list), timelim=NULL, extend=F, withinTimeframe=NA, method=c("distance", "correlation", "constant"), constant=c("mean", "zero", "extend"), trim=0) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (!is.null(timelim)) {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
		blob.list <- lapply(blob.list, window, timelim[1], timelim[2])
	}
	method <- match.arg(method)
	constant <- match.arg(constant)
	if (method == "distance") {
		loc <- lapply(blob.list, attr, "location.xy")
		ok <- (sapply(loc, length) == 2)
		if (any(!ok)) {
			stop(paste("These items do not have a valid 'location.xy' attribute:",
				paste(names(blob.list)[!ok], collapse=", ")))
		}
	}
	
	if (extend) {
		myTimelim <- timelim
		if (is.null(timelim)) {
			myTimelim <- timelim.timeblobs(blob.list)
		}
		for (x in which.impute) {
			if (start(blob.list[[x]]) > min(myTimelim)) {
				blob.list[[x]] <- window(blob.list[[x]], 
					start=min(myTimelim), extend=T)
			}
			if (end(blob.list[[x]]) < max(myTimelim)) {
				blob.list[[x]] <- window(blob.list[[x]], 
					end=max(myTimelim), extend=T)
			}
		}
	}
	
	# calculate distances
	loc <- NA
	pairDistance <- NA
	if (method == "distance") {
		loc <- sapply(blob.list, attr, "location.xy")
		loc <- data.frame(x=loc[1,], y=loc[2,])
		pairDistance <- as.matrix(dist(loc))
		diag(pairDistance) <- NA
	}
	
	pairScales <- list()
	if (method %in% c("distance", "correlation")) {
		tmp.data <- lapply(blob.list, quick.disaccumulate.timeblob)
		tmp.data <- sync.timeblobs(tmp.data)
		# calculate scaling matrix:
		# ratio of some function (eg mean) between each pair of sites
		# (using only pairwise-complete observations)
		for (x in make.names(which.impute)) {
			blob.ok <- !is.na(tmp.data[[x]])
			for (yName in names(blob.list)) {
				y <- make.names(yName)
				ok <- blob.ok & !is.na(tmp.data[[y]])
				if (sum(ok) < 10) {
					pairScales[[x]][[yName]] <- NA
				} else {
					pairScales[[x]][[yName]] <- 
						mean(tmp.data[[x]][ok], trim=trim) /
						mean(tmp.data[[y]][ok], trim=trim)
				}
			}
		}
		rm(tmp.data)
	}
	
	# find all multiple accumulations (using AccumSteps column)
	# and set to NA to indicate a missing value there (it is part of the gap)
	# this is so that Data column is directly comparable to Imputed
	for (x in names(blob.list)) {
		if (!is.null(blob.list[[x]]$AccumSteps)) {
			spanEnd <- which(blob.list[[x]]$AccumSteps > 1)
			# set accumulated observation to NA (it is part of the gap)
			blob.list[[x]]$Data[spanEnd] <- NA
		}
	}
	
	# exclude any "imputed" values: do not use imputed values to impute
	for (x in names(blob.list)) {
		blob.list[[x]]$Data[ (blob.list[[x]]$Qual == "imputed") ] <- NA
	}
	
	ROWS <- 1 # constant
	
	for (blobName in which.impute) {
		print(paste("*** going to impute site", blobName))
		blob <- blob.list[[blobName]]
		
		if (method == "constant") {
			val <- NA
			if (constant == "mean") {
				if (!is.null(blob$AccumSteps)) {
					blob <- quick.disaccumulate.timeblob(blob)
				}
				val <- mean(blob$Data, trim=trim, na.rm=T)
			}
			if (constant == "zero") {
				val <- 0
			}
			if (constant == "extend") {
				# use last available data point
				val <- blob$Data[lastTime(!is.na(blob$Data))]
			}
			blob.list[[blobName]]$Imputed <- val
		}
		
		if (method == "distance") {
			# synchronise all other data to this blob
			rawSync <- syncTo.timeblobs(blob.list, blob)
			blobNameOK <- make.names(blobName) # in rawSync
			blobIndex <- match(blobName, names(blob.list))
			
			dist <- pairDistance[blobIndex,]
			scales <- pairScales[[blobNameOK]]
			# select nearest site in each of four geometric quadrants
			isEast <- (loc$x > loc[blobName, "x"])
			isNorth <- (loc$y > loc[blobName, "y"])
			subsets <- data.frame(
				NE=(isNorth & isEast),
				NW=(isNorth & !isEast),
				SE=(!isNorth & isEast),
				SW=(!isNorth & !isEast))
			row.names(subsets) <- row.names(loc)
			# omit current blob
			dropRows <- blobName
			# omit items if we don't know the scaling factor
			dropRows <- c(dropRows, names(scales)[is.na(scales)])
			# drop these rows
			subsets <- subsets[ !(row.names(subsets) %in% dropRows), ]
			predictorSubsets <- list()
			predictors <- character()
			for (x in names(subsets)) {
				# make it a vector of names
				xNames <- row.names(subsets)[ subsets[[x]] ]
				# order by distance
				byDistance <- order(dist[xNames], na.last=NA)
				xNames <- xNames[byDistance]
				# only consider closest two sites
				length(xNames) <- min(2, length(xNames))
				predictorSubsets[[x]] <- xNames
			}
			predictors <- unlist(predictorSubsets)
			print(predictorSubsets)
			cat("DISTANCES:\n")
			print(dist[predictors])
			cat("SCALE factors (ratio of means):\n")
			print(scales[predictors])
			# set up data
			imputed <- rep(as.numeric(NA), nrow(blob))
			data.matrix <- as.matrix(rawSync[make.names(predictors)])
			colnames(data.matrix) <- predictors
			# inverse distance weighting
			weights <- 1 / dist[predictors]
			# and scale according to ratio of means
			weights <- weights * scales[predictors]
			weights.matrix <- matrix(weights, byrow=T,
				ncol=length(predictors), nrow=nrow(data.matrix))
			colnames(weights.matrix) <- predictors
			weights.matrix[is.na(data.matrix)] <- 0
			# exclude second-choice sites when first-choice exists
			for (x in names(subsets)) {
				if (length(predictorSubsets[[x]]) > 1) {
					firstChoice <- predictorSubsets[[x]][1]
					secondChoice <- predictorSubsets[[x]][2]
					firstOK <- !is.na(data.matrix[,firstChoice])
					weights.matrix[firstOK,secondChoice] <- 0
				}
			}
			# normalise weights at each time step
			weights.normalise <- apply(weights.matrix, ROWS, sum)
			weights.matrix <- weights.matrix / weights.normalise
			cat("DATA:\n")
			print(summary(data.matrix))
			cat("WEIGHTS:\n")
			print(summary(weights.matrix))
			# apply weights
			data.matrix <- data.matrix * weights.matrix
			# compute the interpolated values
			imputed <- apply(data.matrix, ROWS, sum, na.rm=T)
			noprediction <- apply(is.na(data.matrix), ROWS, all)
			imputed[noprediction] <- NA
			imputed[!is.finite(imputed)] <- NA
			blob.list[[blobName]]$Imputed <- imputed
		}
		
		if (method == "correlation") {
			# synchronise all other data to this blob
			rawSync <- syncTo.timeblobs(blob.list, blob)
			blobNameOK <- make.names(blobName) # in rawSync
			blobIndex <- match(blobName, names(blob.list))
			
			scales <- pairScales[[blobNameOK]]
			
			# calculate correlations with blob$Data
			cors <- sapply(rawSync[-1], cor, blob$Data, use="complete")
			names(cors) <- names(blob.list)
			cors <- rev(sort(cors))
			
			predictors <- names(cors)[cors > 0.1]
			predictors <- predictors[(predictors != blobName)]
			
			cat("CORRELATIONS:\n")
			print(cors[predictors])
			cat("SCALE factors (ratio of means):\n")
			print(scales[predictors])
			
			data.matrix <- as.matrix(rawSync[make.names(predictors)])
			colnames(data.matrix) <- predictors
			
			data.matrix <- data.matrix * 
				rep(scales[predictors], each=nrow(data.matrix))
			
			masked <- rep(F, nrow(data.matrix))
			i <- 1
			predicted.frac <- sum(!is.na(data.matrix[!masked,i])) / nrow(data.matrix)
			print(paste(predictors[i], ": predicted", round(100*predicted.frac), "%"))
			
			i <- 2
			while (i <= ncol(data.matrix)) {
				masked <- masked | !is.na(data.matrix[,i-1])
				data.matrix[masked,i] <- NA
				if (sum(!masked) > 0) {
					predicted.frac <- sum(!is.na(data.matrix[!masked,i])) / nrow(data.matrix)
					print(paste(predictors[i], ": predicted", round(100*predicted.frac), "%"))
				}
				i <- i + 1
			}
			
			# compute the interpolated values
			imputed <- apply(data.matrix, ROWS, sum, na.rm=T)
			noprediction <- apply(is.na(data.matrix), ROWS, all)
			imputed[noprediction] <- NA
			blob.list[[blobName]]$Imputed <- imputed
		}
		
	}
	return(blob.list[which.impute])
}

imputeGaps.timeblobs <- function(blob.list, which.impute=names(blob.list), type=c("disaccumulated", "imputed"), fallBackToConstantDisaccum=T, maxGapLength=Inf, internalGapsOnly=F, ...) {
	type <- match.arg(type, several.ok=T)
	# first, impute
	imputed.blobs <- impute.timeblobs(blob.list, which.impute=which.impute, ...)
	# then, fill in the imputed values in gaps
	for (x in which.impute) {
		impBlob <- imputed.blobs[[x]]
		imputedPeriod <- timelim.timeblobs(impBlob)
		lim <- window(blob.list[[x]], start(impBlob), end(impBlob), 
			return.indices=T)
		#lim <- findInterval(imputedPeriod, blob.list[[x]]$Time)
		# first disaccumulate
		spanInfo <- data.frame(end=which(impBlob$AccumSteps > 1))
		if (("disaccumulated" %in% type) && (nrow(spanInfo) > 0)) {
			spanInfo$length <- impBlob$AccumSteps[spanInfo$end]
			spanInfo$start <- with(spanInfo, end - length + 1)
			# drop any gaps which were not completely imputed
			cumNAs <- cumsum(is.na(impBlob$Imputed))
			spanNAs <- with(spanInfo, cumNAs[end] -
				ifelse(start==0, 0, cumNAs[start-1]))
			ok <- (spanNAs == 0)
			spanInfo <- spanInfo[ok,]
			# get known (observed) totals
			spanInfo$accum <- blob.list[[x]]$Data[spanInfo$end + lim[1] - 1]
			# work out sum of imputed values in each gap
			cumSum <- cumsum(ifelse(is.na(impBlob$Imputed), 0, 
				impBlob$Imputed))
			spanInfo$sum <- cumSum[spanInfo$end] - 
				ifelse(spanInfo$start==0, 0, cumSum[spanInfo$start-1])
			# concatenated indices of all the time steps in accums
			allSpans <- expand.indices(spanInfo)
			allSpansOrig <- allSpans + lim[1] - 1
			levels(blob.list[[x]]$Qual) <- union(levels(blob.list[[x]]$Qual),
				"disaccumulated")
			# insert rescaled imputed values into gaps
			blob.list[[x]]$Data[allSpansOrig] <- impBlob$Imputed[allSpans] *
				with(spanInfo, rep(accum / sum, times=length))
			blob.list[[x]]$Qual[allSpansOrig] <- "disaccumulated"
			blob.list[[x]]$AccumSteps[allSpansOrig] <- 1
			if (fallBackToConstantDisaccum) {
				constBlob <- quick.disaccumulate.timeblob(
					window(blob.list[[x]], start(impBlob), end(impBlob)))
				blobWindow <- seq(lim[1], lim[2])
				blob.list[[x]]$Data[blobWindow] <- constBlob$Data
				blob.list[[x]]$Qual[blobWindow] <- constBlob$Qual
			}
			# remove AccumSteps column if no longer relevant
			if (!any(blob.list[[x]]$AccumSteps > 0)) {
				blob.list[[x]]$AccumSteps <- NULL
			}
		}
		if ("imputed" %in% type) {
			# need to take window here; it may have been modified above
			tmpBlob <- window(blob.list[[x]], start(impBlob), end(impBlob))
			# if there are any multiple accumulations, treat as gaps
			# i.e. set the end step to NA, overwriting the total value
			# NB: this throws away information -- cannot be reversed! (TODO)
			if (!is.null(blob.list[[x]]$AccumSteps)) {
				tmpBlob$Data[(tmpBlob$AccumSteps > 1)] <- NA
			}
			allGaps <- expand.indices(gaps(tmpBlob$Data, max.length=maxGapLength,
				internal.only=internalGapsOnly))
			allGapsOrig <- allGaps + lim[1] - 1
			levels(blob.list[[x]]$Qual) <- union(levels(blob.list[[x]]$Qual),
				"imputed")
			blob.list[[x]]$Data[allGapsOrig] <- impBlob$Imputed[allGaps]
			blob.list[[x]]$Qual[allGapsOrig] <- "imputed"
		}
		# remove quality code levels if possible
		blob.list[[x]]$Qual <- blob.list[[x]]$Qual[,drop=T]
	}
	return(blob.list[which.impute])
}

unimputeGaps.timeblobs <- function(blob.list, timelim=NULL, type=c("imputed", "disaccumulated")) {
	# check types
	if (!identical(class(blob.list),"list")) { blob.list <- list(blob.list) }
	if (any(sapply(blob.list, is.timeblob)==F)) { stop("'blob.list' must be a list of timeblobs") }
	if (!is.null(timelim)) {
		timelim <- as.POSIXct(timelim)
		if (any(is.na(timelim))) { stop("'timelim' must be a pair of valid times (POSIXt)") }
	}
	type <- match.arg(type, several.ok=T)
	
	# revert imputed values
	for (x in names(blob.list)) {
		if (!("imputed" %in% type)) { break }
		imputed <- (blob.list[[x]]$Qual == "imputed")
		imputed[is.na(imputed)] <- F
		if (!is.null(timelim)) {
			# restrict to be within time window
			lim <- window(blob.list[[x]], timelim[1], timelim[2], 
				return.indices=T)
			iRow <- seq(along=blob.list[[x]]$Data)
			imputed <- imputed & (lim[1] <= iRow) & (iRow <= lim[2])
		}
		# set imputed values back to NA
		blob.list[[x]]$Data[imputed] <- NA
		blob.list[[x]]$Qual[imputed] <- NA # or whatever
	}
	
	# revert disaccumulated values
	for (x in names(blob.list)) {
		if (!("disaccumulated" %in% type)) { break }
		accumd <- (blob.list[[x]]$Qual == "disaccumulated")
		accumd[is.na(accumd)] <- F
		if (!is.null(timelim)) {
			# restrict to be within time window
			lim <- window(blob.list[[x]], timelim[1], timelim[2], 
				return.indices=T)
			iRow <- seq(along=blob.list[[x]]$Data)
			accumd <- accumd & (lim[1] <= iRow) & (iRow <= lim[2])
		}
		# indices and lengths of strings of consecutive "disaccumulated"
		spanInfo <- gaps(ifelse(accumd, NA, T), internal.only=F)
		spanInfo$end <- with(spanInfo, start + length - 1)
		spanInfo$sum <- mapply(
			function(start,len) {
				sum(blob.list[[x]]$Data[seq(start,length=len)])
			}, spanInfo$start, spanInfo$length)
		# set values back to NA
		blob.list[[x]]$Data[accumd] <- NA
		blob.list[[x]]$Qual[accumd] <- NA # or whatever
		# set accum value
		blob.list[[x]]$Data[spanInfo$end] <- spanInfo$sum
		blob.list[[x]]$Qual[spanInfo$end] <- "suspect" # or whatever
		# keep track of number of days of accumulation
		if (is.null(blob.list[[x]]$AccumSteps)) {
			blob.list[[x]]$AccumSteps <- as.integer(1)
		}
		blob.list[[x]]$AccumSteps[spanInfo$end] <- spanInfo$length
	}
	
	# remove quality code levels if possible
	for (x in names(blob.list)) {
		blob.list[[x]]$Qual <- blob.list[[x]]$Qual[,drop=T]
	}
	
	# contract series (drop NAs at start or end) to reverse 'extend=T'
	# TODO
	
	return(blob.list)
}

waterQuarters <- function(x, start.month=1) {
	if (is.null(start.month)) start.month <- 1
	monthQuarters <- rep(1:4, each=3)
	shiftWrap <- function(x, shift) x[(seq_along(x)-shift-1)%%length(x)+1]
	monthQuarters <- shiftWrap(monthQuarters, start.month-1)
	waterYearMonthNames <- shiftWrap(monthNames, 12-(start.month-1))
	quarterNames <- sapply(c(3,6,9,12), function(i)
		paste(waterYearMonthNames[seq(i-2,i)], collapse=" "))
	x.mon <- as.POSIXlt(x)$mon+1
	factor(monthQuarters[x.mon], labels=quarterNames, ordered=T)
}

## general functions for time series as numeric vectors

gaps <- function(x, max.length=Inf, internal.only=T) {
	seriesNA <- is.na(x)
	# diffNA is 1 at start of gap, -1 at end of gap, 0 otherwise
	diffNA <- c(0, diff(seriesNA))
	preDataGap <- match(F, seriesNA) - 1
	postDataGap <- match(F, rev(seriesNA)) - 1
	# so we don't detect a gap-end at start of data:
	diffNA[preDataGap+1] <- 0
	# find indices where NA is followed by data
	gapEnd <- which(diffNA==-1) - 1
	nGaps <- length(gapEnd)
	naCumSum <- cumsum(seriesNA)
	gapLength <- naCumSum[gapEnd] - 
		naCumSum[c(preDataGap+1,gapEnd[-nGaps])]
	if (internal.only == FALSE) {
		gapLength <- c(if (preDataGap>0) { preDataGap },
			gapLength, if (postDataGap>0) { postDataGap })
		gapEnd <- c(if (preDataGap>0) { preDataGap },
			gapEnd, if (postDataGap>0) { length(x) })
	}
	ok <- (gapLength <= max.length)
	gapLength <- gapLength[ok]
	gapEnd <- gapEnd[ok]
	gapStart <- gapEnd - gapLength + 1
	gapInfo <- data.frame(length=gapLength, start=gapStart)
	if (internal.only) {
		attr(gapInfo, "pre.data") <- preDataGap
		attr(gapInfo, "post.data") <- postDataGap
	}
	return(gapInfo)
}

expand.indices <- function(info) {
	if (is.null(info$length) || is.null(info$start)) {
		stop("'info' should have components $length and $start, see ?gaps")
	}
	sequence(info$length) + rep(info$start, times=info$length) - 1
}

peaks <- function(x) {
	xBackDiff <- c(NA, diff(x)) # backwards difference
	xFwdDiff <- c(xBackDiff[-1], NA) # forwards difference
	peakIdx <- which((xBackDiff > 0) & (xFwdDiff <= 0))
	peakIdx
}

rises <- function(x) {
	# backwards difference, i.e. rises are placed at their end time
	c(NA, pmax(0, diff(x)))
}

rises.old <- function(x) {
	xBackDiff <- c(NA, diff(x)) # backwards difference
	#xFwdDiff <- c(xBackDiff[-1], NA) # forwards difference
	#peaksIdx <- which((xBackDiff > epsilon) & (xFwdDiff < -epsilon))
	
	# TODO: find start of rise and take increase
	isRising <- (xBackDiff > 0)
	isRising[is.na(isRising)] <- F # take NAs as non-rises
	isFalling <- (xBackDiff < 0)
	isFalling[is.na(isFalling)] <- F # take NAs as non-falls
	# total rises and falls
	cumRise <- cumsum(ifelse(isRising, xBackDiff, 0))
	cumFall <- cumsum(ifelse(isFalling, xBackDiff, 0))
	# isRisingFwdDiff: is 1 at start of rise, -1 at end of rise
	isRisingFwdDiff <- c(diff(isRising), NA)
	# find when rises end (may or may not be a peak)
	stopIdx <- which(isRisingFwdDiff == -1)
	stopIdx_prev <- c(1, stopIdx[-length(stopIdx)])
	stopIdx_next <- c(stopIdx[-1], length(x))
	
	rise <- cumRise[stopIdx] - cumRise[stopIdx_prev]
	
	#peakIdx <- which(
	#	((cumRise[stops] - cumRise[stops_prev]) > epsilon) &
	#	((cumFall[stops] - cumFall[stops_next]) > epsilon)
	#)
	
	#peakIdx <- which(isRisingBackDiff == -1)
	#prevPeakIdx <- c(1, peakIdx[-length(peakIdx)])
	#cumRise <- cumsum(ifelse(isRising, xBackDiff, 0))
	#rise <- cumRise[peakIdx] - cumRise[prevPeakIdx]
	#if (!is.na(epsilon)) {
	#	ok <- (rise >= epsilon)
	#	rise <- rise[ok]
	#	peakIdx <- peakIdx[ok]
	#}
	return(list(rise=rise, peak.index=stopIdx))
}


lastTime <- function(x) {
	# returns the most recent TRUE index throughout vector x
	# this could use rle()
	x[is.na(x)] <- F
	theTimes <- which(x == TRUE)
	preGap <- theTimes[1] - 1
	finalGap <- length(x) - theTimes[length(theTimes)] + 1
	interTimes <- c(diff(theTimes), finalGap)
	lastTime <- c(rep(NA, preGap), rep(theTimes, times=interTimes))
	lastTime
}


## other useful functions not specific to timeblobs

timestepTimeFormat <- function(timestep) {
	if (any(grep("month", timestep))) { return("%Y-%b") }
	if (any(grep("year", timestep))) { return("%Y") }
	return("")
}

# numeric method
as.byString <- function(x, digits=getOption("digits"), explicit=F) {
	#if (!identical(class(x), "difftime")) {
	#	if (!is.numeric(x)) { stop("'x' must be difftime or numeric") }
	#	x <- diff(as.POSIXct(c(0,x)))
	#}
	
	if (inherits(x, "difftime")) x <- as.numeric(x, units="secs")
	stopifnot(is.numeric(x))
	#x <- as.numeric.byString(x)
	if (x >= 363*24*60*60) {
		it <- paste(round(x / (365.25*24*60*60)), "years")
	} else
	if (x >= 28*24*60*60) {
		it <- paste(round(x / (30*24*60*60)), "months")
	} else
	if (x >= 23*60*60) {
		it <- paste(round(x / (24*60*60)), "days")
	} else
	if (x >= 60*60) {
		it <- paste(round(x / (60*60)), "hours")
	} else
	if (x >= 60) {
		it <- paste(round(x / (60)), "mins")
	} else {
		it <- paste(round(x), "secs")
	}
	if (!explicit) { it <- sub("^1 ", "", it) }
	#it <- sub(" day", " DSTday", it)
	return(it)
}

incr.POSIXt <- function(x, by="days") {
	seq(x, by=by, length=2)[2]
}

decr.POSIXt <- function(x, by="days") {
	negBy <- paste("-1", by)
	if (any(grep("^[0-9]", by))) {
		negBy <- paste("-", by, sep='')
	}
	seq(x, by=by, length=2)[2]
}

as.numeric.byString <- function(x) {
	if (identical(x, "irregular")) { return(0) }
	timeseq <- seq.POSIXt(from=ISOdate(1970,1,1,0,0,0), by=x, length=2)
	return(as.numeric(timeseq[2]) - as.numeric(timeseq[1]))
}

as.POSIXct.numeric <- function(secs_since_1970, tz="GMT") {
	stopifnot(is.numeric(secs_since_1970))
	secs_since_1970 <- as.numeric(secs_since_1970)
	class(secs_since_1970) <- c("POSIXt", "POSIXct")
	attr(secs_since_1970, "tzone") <- tz
	return(secs_since_1970)
}

truncMonth <- function(x) {
	zz <- as.POSIXlt(x)
	zz$mday <- 1
        zz$hour <- zz$min <- zz$sec <- 0
	zz$isdst <- -1
	return(zz)
}

truncYear <- function(x) {
	zz <- as.POSIXlt(x)
	zz$mday <- 1
	zz$mon <- 0
        zz$hour <- zz$min <- zz$sec <- 0
	zz$isdst <- -1
	return(zz)
}

truncDecade <- function(x) {
	zz <- as.POSIXlt(truncYear(x))
	zz$year <- (zz$year %/% 10) * 10
	return(zz)
}

one.step.acf <- function(blob) {
	acf(blob$Data, na.action=na.pass, lag.max=1, plot=F)$acf[2]
}

