smooth_beads <- function(beads.data, window.size = 201) {
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
#'  \item{\code{m.normed}}{ The normalized data matrix}
#'  \item{\code{beads.smoothed}}{ A matrix containing the smoothed beads data}
#'  \item{\code{beads.normed}}{ A matrix containing the normalized beads data}
#'  \item{\code{beads.slopes}}{ A matrix containing the calculated beadlsopes with \code{nrow(beads.slopes) = nrow(beads.data)} and
#'  two columns: \code{time} and \code{slope}}
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
    beads.normed <- beads.data
    beads.normed[, beads.col.names] <- beads.normed[, beads.col.names] * beads.slopes[, "slope"]

    return(list(m.normed = m, beads.smoothed = sm.beads,
                beads.normed = beads.normed, beads.slopes = beads.slopes))
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
#'
#' @inheritParams normalize_folder
#' @param files.type The type of FCS files contained in \code{wd}. If \code{data}, the files represent
#'  full datasets (i.e. beads + cells events), the gates in \code{beads.gates} will be applied to extract
#'  the beads, and the median intensity of the beads events will be returned to be used as baseline
#'  for normalization. If \code{beads}, the files are assumed to only contain beads events. The
#'  \code{beads.gates} parameter is ignored, and the median of all the beads events (i.e. the entire
#'  content of the files) will be returned.
#'
#' @return Returns a vector with the medians of the beads events across all processed files
#'
calculate_baseline <- function(wd, beads.type, files.type = c("data", "beads"), beads.gates = NULL) {
    files.type <- match.arg(files.type)

    files.list <- switch(files.type,
        "data" = names(beads.gates),
        "beads" = list.files(wd, pattern = "*.fcs$", ignore.case = T)
    )

    ret <- lapply(files.list, function(f.name) {
        fcs <- flowCore::read.FCS(file.path(wd, f.name))
        beads.cols.names <- find_beads_channels_names(fcs, beads.type)
        dna.col <- find_dna_channel(fcs)

        m <- flowCore::exprs(fcs)
        if(files.type == "data") {
            m.transformed <- asinh(m / 5)
            sel <- identify_beads(m.transformed, beads.gates[[f.name]], beads.cols.names, dna.col)
        } else {
            sel <- rep(TRUE, nrow(m))
        }

        #The gating is done on transformed data, but we return the untransformed
        return(m[sel, beads.cols.names])
    })

    ret <- Reduce("rbind", ret)
    ret <- apply(ret, 2, median)
    return(ret)
}



#' Calculates the Mahalanobis distance of each event from the centroid of the beads population
#'
#' @param m The untransformed matrix of data. Must contain a column called \code{beadEvent} which indicates
#' when the corresponding event is a bead (1) or not (0)
#' @param beads.events A vector of length \code{nrow(m)} indicating which rows of m represent beads events
#' @param beads.cols.names The names of the bead columns
#'
#' @return A vector of length \code{nrow(m)} containing the \code{sqrt} of the Mahalanobis distance of each of
#' \code{m} from the centroid of the beads population
#'
#'
get_mahalanobis_distance_from_beads <- function(m, beads.events, beads.cols.names) {
    m <- m[, beads.cols.names]
    m <- asinh(m / 5)
    beads.data <- m[beads.events,]

    cov.m <- cov(beads.data)
    ret <- sqrt(mahalanobis(m, colMeans(beads.data), cov.m))
    return(ret)
}


#' Remove beads from a flowFrame object
#'
#' @param fcs The \code{flowFrame} object from which beads must be removed. Must contain a column
#' called \code{beadDist} representing the Mahalanobis distance of each event from the centroid
#' of the beads population
#' @param dist.threshold The Mahalanobis distance threhsold for beads removal. Events with
#' \code{beadDist <= dist.threshold} will be removed
#'
#' @return Returns a list of two \code{flowFrame} objects
#' \itemize{
#'  \item{\code{data.fcs}}{ The data without the beads events}
#'  \item{\code{beads.fcs}}{ The beads events}
#' }
#'
#'
#' @export
remove_beads_from_fcs <- function(fcs, dist.threshold) {
    m <- flowCore::exprs(fcs)
    beads.events <- m[, "beadDist"] <= dist.threshold
    data.fcs <- as_flowFrame(m[!beads.events, ], fcs)
    beads.fcs <- as_flowFrame(m[beads.events, ], fcs)

    ret <- list(data.fcs = data.fcs, beads.fcs = beads.fcs)
    return(ret)
}



#' Remove beads from an FCS file
#'
#' This function removes beads from an FCS file according to a specified Mahalanobis distance cutoff
#' and writes the results in an output folder
#'
#' @param input.fname The path to the FCS file from which beads must be removed
#' @param out.dir The path to the output directory
#'
#' @inheritParams remove_beads_from_fcs
#'
#'
#' @export
remove_beads_from_file <- function(input.fname, dist.threshold, out.dir) {
    fcs <- flowCore::read.FCS(input.fname)
    beads.dir <- file.path(out.dir, "removed_events")
    dir.create(beads.dir, recursive = T)
    base.fname <- tools::file_path_sans_ext(basename(input.fname))
    data.fname <- paste(base.fname, "beadsremoved.fcs", sep = "_")
    beads.fname <- paste(base.fname, "removedEvents.fcs", sep = "_")

    temp <- premessa::remove_beads_from_fcs(fcs, dist.threshold)
    premessa::write_flowFrame(temp$data.fcs, file.path(out.dir, data.fname))
    premessa::write_flowFrame(temp$beads.fcs, file.path(beads.dir, beads.fname))
    logfile <- file.path(out.dir, "beads_removal_cutoffs.json")
    logdata <- NULL

    if(file.exists(logfile))
        logdata <- jsonlite::fromJSON(logfile)
    else
        logdata <- list()

    logdata[[basename(input.fname)]] <- dist.threshold
    cat(jsonlite::toJSON(logdata, auto_unbox = T, pretty = T), file = logfile)

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
#' @param baseline If \code{NULL} the median beads intensities of the current files will be used as baseline
#'  for normalization. Alternatively this can be a character string with the path of a directory containing
#'  FCS files of beads events, whose median intensities will be used as baseline.
#'
#'
#' @export
normalize_folder <- function(wd, output.dir.name, beads.gates, beads.type, baseline = NULL) {
    baseline.data <- NULL
    if(is.null(baseline))
        baseline.data <- calculate_baseline(wd, beads.type, files.type = "data", beads.gates)
    else
        baseline.data <- calculate_baseline(baseline, beads.type, files.type = "beads")

    out.dir.path <- file.path(wd, output.dir.name)
    beads.dir.path <- file.path(out.dir.path, "beads")
    beads.vs.time.path <- file.path(out.dir.path, "beads_vs_time")
    #This will also create the upstram out.dir.path
    dir.create(beads.dir.path, recursive = T)
    dir.create(beads.vs.time.path, recursive = T)


    cat(jsonlite::toJSON(beads.gates, pretty = T), file = file.path(out.dir.path, "beads_gates.json"))

    ll <- lapply(names(beads.gates), function(f.name) {
        fcs <- flowCore::read.FCS(file.path(wd, f.name))
        beads.cols <- find_bead_channels(fcs, beads.type)
        beads.cols.names <- get_parameter_name(fcs, beads.cols)
        dna.col <- find_dna_channel(fcs)

        m <- flowCore::exprs(fcs)
        beads.events <- identify_beads(asinh(m / 5), beads.gates[[f.name]], beads.cols.names, dna.col)
        beads.data <- m[beads.events,]

        norm.res <- correct_data_channels(m, beads.data, baseline.data, beads.cols.names)
        m.normed <- norm.res$m.normed
        m.normed <- cbind(m.normed,
                        beadDist = get_mahalanobis_distance_from_beads(m.normed, beads.events, beads.cols.names))


        out.name <- paste(tools::file_path_sans_ext(f.name), "normalized.fcs", sep = "_")
        beads.file.name <- paste(tools::file_path_sans_ext(f.name), "beads.fcs", sep = "_")
        write_flowFrame(as_flowFrame(m.normed, fcs), file.path(out.dir.path, out.name))
        write_flowFrame(as_flowFrame(m.normed[beads.events, ], fcs), file.path(beads.dir.path, beads.file.name))

        beads.normed <- apply(norm.res$beads.normed[, beads.cols], 2, median)
        beads.smoothed <- apply(norm.res$beads.smoothed[, beads.cols], 2, median)

        p <- plot_beads_over_time(norm.res$beads.smoothed, smooth_beads(norm.res$beads.normed), beads.cols)

        ggplot2::ggsave(file.path(beads.vs.time.path, gsub(".fcs$", ".pdf", f.name, ignore.case = T)), plot = p, width = 11, height = 8.5, units = "in")
        return(list(beads.normed = beads.normed, beads.smoothed = beads.smoothed))
    })

    beads.medians <- t(sapply(ll, function(x) {return(x[["beads.normed"]])}))
    beads.medians <- rbind(beads.medians,
            t(sapply(ll, function(x) {return(x[["beads.smoothed"]])})))

    beads.medians <- data.frame(beads.medians, row.names = NULL)

    beads.medians$sample <- rep(names(beads.gates), 2)
    beads.medians$type <- c(rep("After", length(ll)), rep("Before", length(ll)))
    beads.medians$type <- factor(beads.medians$type, levels = c("Before", "After"))
    p <- plot_beads_medians(beads.medians)
    ggplot2::ggsave(file.path(out.dir.path, "beads_before_and_after.pdf"), plot = p, width = 11, height = 8.5, units = "in")

}
