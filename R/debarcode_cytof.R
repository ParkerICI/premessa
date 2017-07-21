#' Loading a barcode key
#'
#' @param fname The path of the file containing the barcode key. Please refer to the online documentation
#'  for a description of the format
#'
#' @return Returns a data frame containing the barcode key
#'
#' @export
read_barcode_key <- function(fname) {
    return(read.csv(fname, check.names = F, row.names = 1))
}

#' Get the names of the barcode channels
#'
#' @param m The data matrix, as returned by \code{flowCore::exprs}
#' @param bc.key The barcode key, as returned by \code{read_barcode_key}
#'
#' @return Returns a vector containing the names of the barcode channels. The vector can be used as index
#'  to select the appropriate columns of \code{m}
get_barcode_channels_names <- function(m, bc.key) {
    masses <- names(bc.key)
    grep.string <- paste(masses, collapse = "|")
    ret <- as.character(grep(grep.string, colnames(m), value = T))
    return(ret)
}



#' This assumes that all barcodes are identified by the same number of channels
#' @param m Transformed data matrix
#' @param expected.positive A single number. The expected number of positive barcode channels for each event
calculate_bcs_separation <- function(m, bc.channels, expected.positive, cutoff) {
    m.bcs <- m[, bc.channels]
    m.bcs <- t(apply(m.bcs, 1, sort, decreasing = T))
    deltas <- m.bcs[, expected.positive] - m.bcs[, expected.positive + 1]

    lowest.bc <- m.bcs[, expected.positive]
    lowest.bc[lowest.bc < cutoff] <- NA


    return(list(deltas = deltas, lowest.bc = lowest.bc))
}

get_barcode_label <- function(m, bc.channels, bc.key, lowest.bc) {

    bc.labels <- row.names(bc.key)
    bc.key <- apply(bc.key, 1, paste, collapse = "")
    bc.key <- data.frame(row.names = bc.key, label = bc.labels, stringsAsFactors = F)

    m.bcs <- m[, bc.channels]
    #This works because R does the comparison by column
    m.bcs <- m.bcs >= lowest.bc
    mode(m.bcs) <- "numeric"

    event.key <- apply(m.bcs, 1, paste, collapse = "")
    return(bc.key[event.key, "label"])
}

#' Normalize the barcode channels
#'
#' This function uses preliminary barcode assignments to normalize the intensity of the barcode channels
#'
#' @param m The data matrix
#' @param bc.res The debarcoding results to be used as basis for normalization. Data is normalized by grouping
#'  together all the events assigned to the same barcoded sample. Rows for which \code{is.na(bc.res$labels) == TRUE}
#'  are left unnormalized
#' @param bc.key The barcode key, as returned by \code{read_barcode_key}
#'
#' @return Returns a matrix of normalized data. The ordering of the rows of the input matrix is preserved
normalize_by_pop <- function(m, bc.res, bc.key) {
    #This is an ugly way to do this but we need to normalize the matrix
    #in-place in order to preserve the ordering of the rows

    bc.channels <- bc.res$bc.channels
    bc.labels <- bc.res$labels


    for(lab in row.names(bc.key)) {

        rr <- bc.labels == lab

        #This is necessary because the comparison with NA's will produce NA's
        rr[is.na(rr)] <- FALSE

        if(any(rr)) {
            pos.channels <- as.matrix(bc.key[lab, ])[1, ]
            pos.channels <- bc.channels[pos.channels == 1]

            #Calculates a single normalization value across all the positive channels
            norm.factor <- quantile(as.vector(m[rr, pos.channels]), probs = 0.95, na.rm = T)

            #This produces values that are not strictly in [0, 1]
            m[rr, bc.channels] <- m[rr, bc.channels] / norm.factor
        }

    }
    return(m)
}


#' Assign events to a specific sample
#'
#' @param bc.results The debarcoding results returned from \code{debarcode_data}
#' @param sep.threshold The threshold value for the separation between the positive and negative barcode
#'  channels. Only events whose separation is greater than this threshold are assigned to the sample
#' @param mahal.threshold If not \code{NULL} The threshold value for the Mahalanobis distance between each
#'  event and the centroid of the sample the event has been assigned to. Only events with distance lower
#'  than this threshold are assigned to the sample.
#' @param mahal.dist A vector of Mahalnobis distances, as returned by \code{get_mahalanobis_distance}. Only
#'  used if \code{mahal.threshold} is not \code{NULL}
#'
#' @return Returns a character vector containing the sample labels, given the current thresholds.
#'
#' @export
get_assignments_at_threshold <- function(bc.results, sep.threshold, mahal.threshold = NULL, mahal.dist = NULL) {
    ret <- bc.results$labels
    ret[is.na(ret)] <- "Unassigned"
    ret[bc.results$deltas <= sep.threshold] <- "Unassigned"
    if(!is.null(mahal.threshold))
        ret[mahal.dist > mahal.threshold] <- "Unassigned"
    return(ret)
}


#' Returns the indices of the events corresponding to a given sample label
#'
#'
#' @param label Character string. The desired sample label
#' @inheritParams get_assignments_at_threshold
#'
#' @return Returns a boolean vector with \code{TRUE} corresponding to the desired events
#'
get_sample_idx <- function(label, bc.results, sep.threshold, mahal.threshold = NULL, mahal.dist = NULL) {
    assignments <- get_assignments_at_threshold(bc.results, sep.threshold, mahal.threshold, mahal.dist)
    sel.rows <- assignments == label

    return(sel.rows)
}


#' Calculates the squared Mahalanobis distance from the centroid of the barcoded sample
#'
#' @param m The data matrix
#' @param bc.res The debarcoding results
#' @param sep.threshold The minimum separation in the intensity of the barcoding channels. Barcode assignments
#'  are performed based on this threshold. Then the function calculates the Mahalanobis distance between each event
#'  and the centroid of the debarcoded sample, as assigned using this threshold.
#'
#' @return Returns a vector of squared Mahalanobis distances, as calculated by \code{stats::mahalanobis}. The distance
#'  values are capped at 30
#'
#' @export
get_mahalanobis_distance <- function(m, bc.res, sep.threshold) {
    bc.channels <- bc.res$bc.channels
    assignments <- get_assignments_at_threshold(bc.res, sep.threshold)
    ret <- numeric(nrow(m))

    for(lab in unique(assignments)) {
        sel.rows <- assignments == lab
        temp <- m[sel.rows, bc.channels]
        cov.m <- cov(temp)
        #Here we are not taking the sqrt (different from normalizer)
        mahal <- mahalanobis(temp, colMeans(temp), cov.m)
        ret[sel.rows] <- mahal
    }
    ret[ret > 30] <- 30
    return(ret)
}

#' Calculate number of events assigned to each sample
#'
#'
#' @inheritParams get_assignments_at_threshold
get_well_abundances <- function(bc.results, sep.thresholds, mahal.threshold = NULL, mahal.dist = NULL) {
    all.labels <- unique(bc.results$labels)
    all.labels[is.na(all.labels)] <- "Unassigned"

    ret <- sapply(sep.thresholds, function(i) {
        label <- get_assignments_at_threshold(bc.results, i, mahal.threshold, mahal.dist)
        label <- factor(label, levels = all.labels)

        df <- as.data.frame(table(label))
        df <- cbind(df, threshold = i)
        df$label <- as.character(df$label)
        return(list(df))
    })
    return(Reduce("rbind", ret))
}


#' Debarcodes an individual matrix of data
#'
#' @param m The input matrix
#' @param bc.channels caracter vector. The names of the barcode channels
#' @param bc.key The barcode key, as return by \code{read_barcode_key}
#'
#' @return Returns a list with the following components
#'  \itemize{
#'      \item{\code{labels}}{ character vector of length \code{nrow(m)}. The sample assignments}
#'      \item{\code{deltas}}{ numeric vector of length \code{nrow(m)}. For each event, the separation between the lowest
#'          barcode channel and the highest non-barcode channel}
#'      \item{\code{bc.channels}}{ character vector. The name of the barcode channels}
#'  }
debarcode_data_matrix <- function(m, bc.channels, bc.key) {
    expected.positive <- unique(rowSums(bc.key))
    if(length(expected.positive) > 1)
        stop("Barcoding schems with a variable number of positive channels per sample are not currently supported")

    cutoff <- 0

    seps <- calculate_bcs_separation(m, bc.channels, expected.positive, cutoff)
    labels <- get_barcode_label(m, bc.channels, bc.key, seps$lowest.bc)
    return(list(labels = labels, deltas = seps$deltas, bc.channels = bc.channels))

}

#' Debarcodes data by doing normalization after preliminary barcode assignemnts
#'
#' @param m The data matrix
#' @param bc.key The barcode key, as returned by \code{read_barcode_key}
#'
#' @return Returns a list with the following components
#'  \itemize{
#'      \item{\code{m.normed}}{ matrix with \code{nrow(m)} rows. The normalized barcode intensities}
#'      \item{\code{labels}}{ character vector of length \code{nrow(m)}. The sample assignments}
#'      \item{\code{deltas}}{ numeric vector of length \code{nrow(m)}. For each event, the separation between the lowest
#'          barcode channel and the highest non-barcode channel}
#'      \item{\code{bc.channels}}{ character vector. The name of the barcode channels}
#'  }
#'
#' @export
debarcode_data <- function(m, bc.key) {
    barcode.channels <- get_barcode_channels_names(m, bc.key)


    bc.res <- debarcode_data_matrix(m, barcode.channels, bc.key)
    m.normed <- normalize_by_pop(m, bc.res, bc.key)
    bc.res.normed <- debarcode_data_matrix(m.normed, barcode.channels, bc.key)
    return(list(m.normed = m.normed, labels = bc.res.normed$labels,
                deltas = bc.res.normed$deltas, bc.channels = barcode.channels))

}

#' Main wrapper for the debarcoder pipeline.
#'
#' Takes all the parameters as input, does everything and writes back the FCS files
#'
#' @param fcs The input \code{flowFrame}
#' @param bc.key The barcode key, as returned by \code{read_barcode_key}
#' @param output.dir The output directory
#' @param output.basename The basename of the output files. For each debarcoded sample, the sample label will
#'  be appended to this basename
#' @param sep.threshold The minimum seperation between the lowest barcode channel, and the highest non-barcode channel.
#'  The data will be normalized before doing the assignments, therefore this number will typically be in \code{[0, 1]}
#' @param mahal.dist.threshold The maximum squared Mahalanobis distance between each event and the centroid
#'  of the population that event has been assigned to. This is for further filtering the barcode assignments and it is not
#'  always necessary. Events with distance above this threshold are left unassigned. The distance is capped at 30, therefore
#'  the default value of this option corresponds to no filtering
#'
#' @return For each debarcoded sample, a new FCS file is created in the output directory. Unassigned events are written
#'  in a separate file
#' @export
debarcode_fcs <- function(fcs, bc.key, output.dir, output.basename, sep.threshold, mahal.dist.threshold = 30) {
    m <- flowCore::exprs(fcs)
    m.transformed <- asinh(m / 10)


    bc.res <- debarcode_data(m.transformed, bc.key)
    mahal.dist <- get_mahalanobis_distance(m.transformed, bc.res, sep.threshold)

    assignments <- get_assignments_at_threshold(bc.res, sep.threshold, mahal.dist.threshold, mahal.dist)

    for(lab in unique(assignments)) {
        temp <- m[assignments == lab,]
        out.fcs <- as_flowFrame(temp, fcs)
        out.fname <- file.path(output.dir, sprintf("%s_%s.fcs", output.basename, lab))
        write_flowFrame(out.fcs, out.fname)
    }
}



