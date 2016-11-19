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

get_assignemnts_at_threshold <- function(bc.results, sep.threshold) {
    ret <- bc.results$labels
    ret[is.na(ret)] <- "Unassigned"
    ret[bc.results$deltas <= sep.threshold] <- "Unassigned"
    return(ret)
}

get_mahalanobis_distance <- function(m, bc.res, bc.channels, sep.threshold) {
    assignments <- get_assignemnts_at_threshold(bc.res, sep.threshold)
    ret <- numeric(nrow(m))

    for(lab in unique(assignments)) {
        sel.rows <- assignments == lab
        temp <- m[sel.rows, bc.channels]
        cov.m <- cov(temp)
        #Here we are not taking the sqrt (different from normalizer)
        mahal <- mahalanobis(temp, colMeans(temp), cov.m)
        ret[sel.rows] <- mahal
    }
    return(ret)
}


get_well_abundances <- function(bc.results, sep.thresholds) {
    all.labels <- unique(bc.results$labels)
    all.labels[is.na(all.labels)] <- "Unassigned"

    ret <- sapply(sep.thresholds, function(i) {
        label <- get_assignemnts_at_threshold(bc.results, i)
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
    return(list(m.normed = m.normed, bc.res.normed = bc.res.normed, bc.channels = barcode.channels))

}


#Returns the row indices that select the specific sample
get_sample_idx <- function(label, bc.results, sep.threshold) {
    assignments <- get_assignemnts_at_threshold(bc.results, sep.threshold)
    sel.rows <- assignments == label

    return(sel.rows)
}


#' Main wrapper for the debarcoder pipeline. Takes all the parameters as input
#' does everything and writes back the FCS files

debarcode_fcs <- function(fcs, bc.key, output.dir, output.basename, sep.threshold) {
    m <- flowCore::exprs(fcs)
    m.transformed <- asinh(m / 10)

    res <- debarcode_data(m.transformed, bc.key)
    assignments <- get_assignemnts_at_threshold(res$bc.res, sep.threshold)

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





