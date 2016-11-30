# Transformed matrix




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


#' Rows for which bc.labels is NA are left unnormalized
normalize_by_pop <- function(m, bc.channels, bc.labels) {
    #This is an ugly way to do this but we need to normalize the matrix
    #in-place in order to preserve the ordering of the rows
    for(lab in unique(bc.labels)) {
        if(!is.na(lab)) {
            rr <- bc.labels == lab

            #This is necessary because the comparison with NA's will produce NA's
            rr[is.na(rr)] <- FALSE

            cc <- bc.channels

            norm.factor <- apply(m[rr, cc], 2, quantile, probs = 0.95, na.rm = T)
            #Divides each row by the norm.factor vector
            #This produces values that are not strictly in [0, 1]
            m[rr, cc] <- m[rr, cc] %*% diag(1 / norm.factor)
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


# The mahalanobis distance is hard-capped at 30
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


#' Debarcodes an individual data matrix
debarcode_data_matrix <- function(m, bc.channels, bc.key) {
    expected.positive <- 3
    cutoff <- 0

    seps <- calculate_bcs_separation(m, bc.channels, expected.positive, cutoff)
    labels <- get_barcode_label(m, bc.channels, bc.key, seps$lowest.bc)
    return(data.frame(labels, deltas = seps$deltas, stringsAsFactors = F))

}

#' Debarcodes data by doing normalization after preliminary barcode assignemnts
#'

debarcode_data <- function(m, bc.key) {
    barcode.channels <- get_barcode_channels_names(m, bc.key)


    bc.res <- debarcode_data_matrix(m, barcode.channels, bc.key)
    m.normed <- normalize_by_pop(m, barcode.channels, bc.res$labels)
    bc.res.normed <- debarcode_data_matrix(m.normed, barcode.channels, bc.key)
    return(list(m.normed = m.normed, labels = bc.res.normed$labels,
                deltas = bc.res.normed$deltas, bc.channels = barcode.channels))

}

#' Main wrapper for the debarcoder pipeline. Takes all the parameters as input
#' does everything and writes back the FCS files

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
        flowCore::write.FCS(out.fcs, out.fname)

    }


}




#bc.res <- debarcode(m, barcode.channels, bc.key)
#m.normed <- normalize_by_pop(m, barcode.channels, bc.res$labels)
#bc.res.normed <- debarcode(m.normed, barcode.channels, bc.key)

#get_well_abundances(bc.res.normed$deltas, bc.res.normed$labels, seq(0, 1, 0.1))





