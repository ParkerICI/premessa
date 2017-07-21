#Modified from https://github.com/nolanlab/cytofCore
#Fixes the parameters in the flowFrame, based on information
#from the corresponding exprs matrix
#if desc is different from NULL, also sets the parmeter description
#(i.e. $PnS)
#Set the range from the data or from a fixed value
update_flowFrame_keywords <- function(flowFrame, exprs.m, desc = NULL, data.range = "data") {

    params <- flowCore::parameters(flowFrame)
    pdata <- flowCore::pData(params)

    if(is.null(desc))
        desc <- colnames(exprs.m)

    for (i in 1:ncol(flowFrame)) {
        s <- paste("$P",i,"S",sep="")
        n <- paste("$P",i,"N",sep="")
        r <- paste("$P",i,"R",sep="")
        b <- paste("$P",i,"B",sep="")
        e <-  paste("$P",i,"E",sep="")

        keyval <- list()

        if(!is.na(desc[i]))
            keyval[[s]] <- desc[i]

        keyval[[n]] <- colnames(exprs.m)[i]

        if(data.range == "data")
            keyval[[r]] <- ceiling(max(exprs.m[,i]) - min(exprs.m[,i]))
        else if(is.numeric(data.range))
            keyval[[r]] <- data.range
        else
            stop("Invalid data.range parameter")

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

#' Copy FCS keywords from a source to a target flowFrame object
#'
#' @param source.frame The source \code{flowFrame} object
#' @param target.frame The target \code{flowFrame} object
#' @param kw.list The list of keywords to copy. The keywords in this list that
#'  are not present in the \code{source.frame} are ignored
#'
#' @return Returns a \code{flowFrame} object
#'
copy_keywords <- function(source.frame, target.frame, kw.list) {
    source.keywords <- flowCore::keyword(source.frame)

    for(kw in kw.list)
        if(!is.null(source.keywords[[kw]]))
            flowCore::keyword(target.frame) <- source.keywords[kw]

    return(target.frame)
}



#' Convert a matrix to a flowFrame object
#'
#' This function converts a matrix to a \code{flowFrame} object, taking care of properly setting
#' parameters and keywords in the resulting object
#'
#' @param exprs.m The data matrix. Parameter names will be taken from \code{colnames(exprs.m)},
#' but see also the description of the \code{source.flowFrame} parameter
#' @param source.flowFrame If a flowFrame object is supplied, the function will copy matching names and descriptions
#' keywords from it (e.g. $P1S in the \code{source.flowFrame} is copied to $P1S of the new flowFrame etc.). Extra columns present
#' in exprs.m are preserved (i.e. if source.flowFrame doesn't contain $P1S the original version is preserved). Note that this
#' in general only useful in simple cases where both the data matrix and the \code{flowFrame} represent the same
#' data, and have the same column ordering, and you are interested in copy things like parameter names and descriptions.
#' You will probably introduce errors and inconsistencies in the resulting \code{flowFrame} object if that is not the case.
#' A number of extra optional columns (such as \code{$CYT}) will also be copied if present
#'
#' @return Returns a \code{flowFrame} object
#'
#' @export
as_flowFrame <- function(exprs.m, source.frame = NULL) {
    flow.frame <- flowCore::flowFrame(exprs.m)
    flow.frame <- update_flowFrame_keywords(flow.frame, exprs.m)

    if(!is.null(source.frame)) {
        num.cols <- ncol(flow.frame)
        kw.list <- paste("$P", 1:num.cols, "S", sep = "")
        kw.list <- c(kw.list, paste("$P", 1:num.cols, "N", sep = ""))
        kw.list <- c(kw.list, "$CYT", "$CYTSN", "$DATE", "$FIL", "$BTIM", "$ETIM")
        flow.frame <- copy_keywords(source.frame, flow.frame, kw.list)
    }
    return(flow.frame)

}

#' Write a flowFrame as FCS file
#'
#' This function writes a flowFrame as an FCS file, taking care of updating the \code{$FILENAME} keyword
#'
#' @param flowFrame the \code{flowFrame} to write
#' @param path destination path
#' @export
write_flowFrame <- function(flowFrame, path) {
    f.name <- basename(path)
    flowCore::keyword(flowFrame)$FILENAME <- f.name
    flowCore::write.FCS(flowFrame, path)
}

read_fcs <- function(f.name) {
    fcs <- flowCore::read.FCS(f.name)
    ret <- list()
    ret$m <- flowCore::exprs(fcs)
    p.names <-  as.character(flowCore::parameters(fcs)$name)
    colnames(ret$m) <- p.names
    ret$desc <- as.character(flowCore::parameters(fcs)$desc)
    names(ret$desc) <- p.names
    ret$keywords <- flowCore::keyword(fcs)
    return(ret)
}


write_fcs <- function(fcs, out.name) {
    # Drop parameter keywords
    keys <- fcs$keywords
    keys <- keys[grep("\\$P[0-9]+.", names(keys), invert = T)]

    flow.frame <- flowCore::flowFrame(fcs$m)
    flow.frame <- update_flowFrame_keywords(flow.frame, fcs$m, fcs$desc, data.range = 262144)
    flowCore::keyword(flow.frame) <- keys
    write_flowFrame(flow.frame, out.name)
}


#to.remove = names of parameters to remove
remove_parameters <- function(fcs, to.remove) {
    ret <- fcs
    ret$m <- ret$m[, !(colnames(ret$m) %in% to.remove)]
    ret$desc <- ret$desc[colnames(ret$m)]
    return(ret)
}


add_parameter <- function(fcs, v, name, desc) {
    ret <- fcs
    ret$m <- cbind(ret$m, v)
    colnames(ret$m)[ncol(ret$m)] <- name

    ret$desc <- c(ret$desc, desc)
    names(ret$desc)[length(ret$desc)] <- name
    return(ret)
}
