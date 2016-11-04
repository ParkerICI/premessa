#Modified from https://github.com/nolanlab/cytofCore
#Fixes the parameters in the flowFrame, based on information
#from the corresponding exprs matrix
update_flowFrame_keywords <- function(flowFrame, exprs.m) {

    params <- flowCore::parameters(flowFrame)
    pdata <- flowCore::pData(params)

    for (i in 1:ncol(flowFrame)) {
        s <- paste("$P",i,"S",sep="")
        n <- paste("$P",i,"N",sep="")
        r <- paste("$P",i,"R",sep="")
        b <- paste("$P",i,"B",sep="")
        e <-  paste("$P",i,"E",sep="")

        keyval <- list()

        keyval[[s]] <- colnames(exprs.m)[i]
        keyval[[n]] <- colnames(exprs.m)[i]
        keyval[[r]] <- ceiling(max(exprs.m[,i]) - min(exprs.m[,i]))
        keyval[[b]] <- 32
        keyval[[e]] <- "0,0"
        flowCore::keyword(flowFrame) <- keyval


        pdata[i,"minRange"] <- min(exprs.m[,i])
        pdata[i,"maxRange"] <- max(exprs.m[,i])

    }

    flowCore::pData(params) <- pdata
    flowCore::parameters(flowFrame) <- params

    # keyval[["$DATATYPE"]] <- "F"
    return(flowFrame)
}

copy_keywords_name_desc <- function(source.frame, target.frame) {
    source.keywords <- flowCore::keyword(source.frame)

    for (i in 1:ncol(target.frame)) {
        s <- paste("$P",i,"S",sep="")
        n <- paste("$P",i,"N",sep="")

        if(!is.null(source.keywords[[s]]))
            flowCore::keyword(target.frame) <- source.keywords[s]

        if(!is.null(source.keywords[[n]]))
            flowCore::keyword(target.frame) <- source.keywords[n]
    }
    return(target.frame)
}



#' Write FCS files, with proper processing of parameters and keywords
#'
#' @param exprs.m The data matrix. Parameter names will be taken from \code{colnames(exprs.m)},
#' but see also the description of the \code{source.flowFrame} parameter
#' @param out.name The name of the output file
#' @param source.flowFrame If a flowFrame object is supplied, the function will copy matching names and descriptions
#' keywords from it (e.g. $P1S in the \code{source.flowFrame} is copied to $P1S of the new flowFrame etc.). Extra columns present
#' in exprs.m are preserved (i.e. if source.flowFrame doesn't contain $P1S the original version is preserved)
#'
write_FCS <- function(exprs.m, out.name, source.flowFrame = NULL) {
    flow.frame <- flowCore::flowFrame(exprs.m)
    flow.frame <- update_flowFrame_keywords(flow.frame, exprs.m)

    if(!is.null(source.flowFrame))
        flow.frame <- copy_keywords_name_desc(source.flowFrame, flow.frame)

    flowCore::write.FCS(flow.frame, out.name)
}


slide_function <- function(data, window, fun, step = 1, ...) {
    res <- NULL
    total <- length(data)

    if(total >= window) {
        spots <- seq(from = 1, to = (total - window + 1), by = step)
        res <- vapply(spots, FUN = function(i) {return(median(data[i:(i + window - 1)], ...))}, FUN.VALUE = 1)
    }
    else
        res <- fun(data, ...)

    return(res)
}

smooth_beads <- function(beads.data, window.size = 201) {
    #ret <- apply(beads.data, 2, function(x) {return(slide_function(x, window = window.size, fun = median))})

    ret <- apply(beads.data, 2, runmed, k = window.size)
    return(ret)

}

compute_bead_slopes <- function(beads.data, baseline, beads.col.names) {
    beads.data <- beads.data[, beads.col.names]
    baseline <- baseline[beads.col.names]

    stopifnot(length(baseline) == dim(beads.data)[2])
    m <- t(t(beads.data) * baseline)

    #Minimizing residual sum of squares, assuming a no-intercept model
    ret <- rowSums(m) / rowSums(beads.data ^ 2)
    return(ret)
}

#' Normalize data
#'
#' This function performs the actual normalization
#'
#' @param m The data matrix
#' @param beads.data The beads.data for this data matrix. This would typically be a subset of the rows
#'  representing the bead events
#' @param baseline A vector of values containing the bead channels values to be used as baseline
#'  for normalization
#' @param beads.col.names A vector with the names of the beads channels
#' @param time.col.name The name of the time column
#'
#' @return Returns a list with the following components
#' \itemize{
#'  \item \code{m.normed} The normalized data matrix
#'  \item \code{smoothed.beads} A matrix with the smoothed beads data
#'  \item \code{beads.slopes} A matrix with the calculated beadlsopes with \code{nrow(beads.slopes) = nrow(beads.data)} and
#'  two columns: \code{time} and \code{slope}
#' }
#'
#'
correct_data_channels <- function(m, beads.data, baseline, beads.col.names, time.col.name = "Time") {
    sm.beads <- smooth_beads(beads.data)
    slopes <- compute_bead_slopes(sm.beads, baseline, beads.col.names)

    tt <- beads.data[, time.col.name]
    y <- slopes
    int.slopes <- approx(tt, y, m[, time.col.name], method = "linear", rule = 2)

    include.channels <- grepl("Di$|Dd$", colnames(m))


    print(sprintf("Modfying channels: %s", paste(colnames(m)[include.channels], collapse = ",")))
    print(sprintf("Leaving alone channels: %s", paste(colnames(m)[!include.channels], collapse = ",")))

    m[, include.channels] <- m[, include.channels] * int.slopes$y
    beads.slopes <- cbind(time = tt, slope = slopes)

    return(list(m.normed = m, smoothed.beads = sm.beads, beads.slopes = beads.slopes))
}


#' Beads identification
#'
#' Applies the beads gates to the data matrix and idenfies the beads
#' @param m The matrix, asinh transformed
#' @param gates Gates to be applied (only the gates for this specific matrix)
#' @param beads.cols.names A character vector containing the names of the beads columns
#' @param dna.col The index (either character, numeric or boolean) of the DNA channel column
#'
#' @return Returns a boolean vector of length \code{nrow(m)} with the indexes of the bead events
identify_beads <- function(m, gates, beads.cols.names, dna.col) {
    sel <- lapply(beads.cols.names, function(n) {
        g <- gates[[n]]
        ret <- (m[,n] > g$x[1]) & (m[,n] < g$x[2]) & (m[,dna.col] > g$y[1]) & (m[,dna.col] < g$y[2])

    })

    return(Reduce("&", sel))
}


#' Calculate baseline for normalization
#'
#' Extract beads events from all the FCS files in the working directory
#' and calculates the baseline for normalization
#' @inheritParams normalize_folder
#'
#' @return Returns a vector with the medians of the beads events across all processed files
#'
calculate_baseline <- function(wd, beads.gates, beads.type) {
    ret <- lapply(names(beads.gates), function(f.name) {
        fcs <- flowCore::read.FCS(file.path(wd, f.name))
        beads.cols <- find_bead_channels(fcs, beads.type)
        beads.cols.names <- get_parameter_name(fcs, beads.cols)
        dna.col <- find_dna_channel(fcs)

        m <- flowCore::exprs(fcs)
        m.transformed <- asinh(m / 5)

        sel <- identify_beads(m.transformed, beads.gates[[f.name]], beads.cols.names, dna.col)
        #The gating is done on transformed data, but we return the untransformed
        return(m[sel,])
    })

    ret <- Reduce("rbind", ret)
    ret <- apply(ret, 2, median)
    return(ret)
}



#' Normalize a folder of FCS files
#'
#' Main wrapper for the normalization pipeline. Use beads gates to identify beads, calculate a baseline
#' for normalization and then normalize the files, writing new output FCS
#'
#' @param wd Working directory (character)
#' @param output.dir.name Name of the output directory (character). The output directory
#'  will be created as a subfolder of the Working directory
#' @param beads.gates Gates to identify the beads. This is a data structure with the following format
#'  \code{list(file_name = list(channel_name = list(x = [xMin, xMax], y = [yMin, yMax]), ...), ...)}.
#'  Note that only files in \code{names(beads.gates)} will be processed. Also note that the data structure
#'  may contain gates for extra channels, but which channels will be used depends on the \code{beads.type} parameter
#' @param beads.type Type of beads. Must be on of \code{"Fluidigm"}, \code{"Beta"}
#'
#'
#' @export
normalize_folder <- function(wd, output.dir.name, beads.gates, beads.type) {

    baseline <- calculate_baseline(wd, beads.gates, beads.type)

    out.dir.path <- file.path(wd, output.dir.name)
    dir.create(out.dir.path, recursive = T)



    ll <- lapply(names(beads.gates), function(f.name) {
        fcs <- flowCore::read.FCS(file.path(wd, f.name))
        beads.cols <- find_bead_channels(fcs, beads.type)
        beads.cols.names <- get_parameter_name(fcs, beads.cols)
        dna.col <- find_dna_channel(fcs)

        m <- flowCore::exprs(fcs)
        #This requires to have the beads for each individual file
        beads.events <- identify_beads(asinh(m / 5), beads.gates[[f.name]], beads.cols.names, dna.col)
        beads.data <- m[beads.events,]

        norm.res <- correct_data_channels(m, beads.data, baseline, beads.cols.names)
        write_FCS(norm.res$m.normed, file.path(out.dir.path, f.name), fcs)
        #Return the median of smoothed beads
    })
}
