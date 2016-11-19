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



#' Convert a matrix to a flowFrame object
#'
#' This function converts a matrix to a \code{flowFrame} object, taking care of properly setting
#' parameters and keywords in the resulting object
#'
#' @param exprs.m The data matrix. Parameter names will be taken from \code{colnames(exprs.m)},
#' but see also the description of the \code{source.flowFrame} parameter
#' @param source.flowFrame If a flowFrame object is supplied, the function will copy matching names and descriptions
#' keywords from it (e.g. $P1S in the \code{source.flowFrame} is copied to $P1S of the new flowFrame etc.). Extra columns present
#' in exprs.m are preserved (i.e. if source.flowFrame doesn't contain $P1S the original version is preserved)
#'
#' @return Returns a \code{flowFrame} object
#'
#' @export
as_flowFrame <- function(exprs.m, source.flowFrame = NULL) {
    flow.frame <- flowCore::flowFrame(exprs.m)
    flow.frame <- update_flowFrame_keywords(flow.frame, exprs.m)

    if(!is.null(source.flowFrame))
        flow.frame <- copy_keywords_name_desc(source.flowFrame, flow.frame)
    return(flow.frame)

}

