
#' Read parameters from a set of FCS files
#'
#' This function reads information about the parameters in a set of FCS files
#' and returns the aggregate information in a table
#'
#' @param files.list A character vector containing the paths to the files
#'
#' @return Returns a data.frame with \code{row.names} corresponding to the union of all
#'   parameter names (i.e. $PnN) in all the files. Each column correspond to a file
#'   and the \code{[row, column]} values indicating the description (i.e. $PnS) value
#'   for the \code{row} parameter in the \code{column} file. If the description field
#'   is empty, the value is \code{""}. If a parameter is missing in a file the value
#'   is \code{NA}
#'
#' @export
read_parameters <- function(files.list) {
    ret <- lapply(files.list, function(f) {
        fcs <- flowCore::read.FCS(f, which.lines = 1)
        df <- data.frame(name = as.character(flowCore::parameters(fcs)$name),
            desc = as.character(flowCore::parameters(fcs)$desc), check.names = F,
            stringsAsFactors = F)
        df$desc[is.na(df$desc)] <- ""
        names(df) <- gsub("desc", basename(f), names(df))
        return(df)

    })

    ret <- Reduce(function(a, b) {
        merge(a, b, by = "name", all.x = T, all.y = T)
    }, ret)

    ret <- data.frame(ret, check.names = F, stringsAsFactors = F)

    row.names(ret) <- ret$name
    ret$name <- NULL

    return(ret)
}


#' Rename FCS parameter names
#'
#' This function renames FCS parameter names (i.e. the $PnN keyword)
#'
#' @param fcs FCS data as returned by \code{read_fcs}
#' @param names.map A named character vector mapping old names to new names.
#'   The \code{names} of this vector should be the old names, and the values
#'   the corresponding new names. New names need to be unique
#'
#' @return Returns the FCS data with the updated names
#'
#' @export
rename_fcs_parameters_name <- function(fcs, names.map) {
    ret <- fcs
    old.names <- colnames(ret$m)
    new.names <- as.character(names.map[old.names])
    stopifnot(!any(duplicated(new.names)))
    colnames(ret$m) <- new.names
    names(ret$desc) <- new.names

    spill.keyword <- grep("SPILL", names(ret$keywords), value = T)

    if(length(spill.keyword) > 0) {
        m <- ret$keywords[[spill.keyword]]
        stopifnot(is.matrix(m))
        colnames(m) <- as.character(names.map[colnames(m)])
        ret$keywords[[spill.keyword]] <- m
    }
    return(ret)
}

#' Rename FCS parameter descriptions
#'
#' This function renames FCS parameter descriptions (also called "short names", i.e.
#'   the $PnS keyword)
#'
#' @param fcs FCS data as returned by \code{read_fcs}
#' @param names.map A named character vector mapping parameter names (i.e. $PnN) to
#'   the desired parameter descriptions ($PnS). The \code{names} of this vector
#'   should be the parameter names, and the values the parameter descriptions. All
#'   the parameter names of the \code{fcs} data must be present in \code{names(names.map)}
#'
#' @return Returns the FCS data with the updated descriptions
#'
#' @export
rename_fcs_parameters_desc <- function(fcs, names.map) {
    ret <- fcs
    stopifnot(all(colnames(ret$m) %in% names(names.map)))

    ret$desc <- names.map[colnames(ret$m)]
    return(ret)
}


#' Renames FCS parameters in a set of files
#'
#' This function renames FCS parameters in a set of files, as specified by a template
#'
#' @param working.dir The directory in which the files are located
#' @param out.dir The output directory. This will be created as a subfolder of
#'   \code{working.dir}
#' @param tab The table specifying how the renaming should be performed. This
#'   is a structure similar as the \code{data.frame} returned from \code{read_parameters}. \code{row.names}
#'   correspond to the current parameter names in the files, and columns correspond to files (which
#'   should be located in the \code{working.dir}). The value at \code{[row, column]} should contain the
#'   value you want to set for the corresponding parameter description (i.e. $PnS). In addition the
#'   \code{data.frame} must contain two extra columns with the following names
#'   \itemize{
#'      \item{\code{Remove}}{ logical indicating whether the parameter should be removed from the files}
#'      \item{\code{Parameter}}{ character indicating the desired new name of the parameter (i.e. the new
#'      $PnN)}
#'  }
#' @export
rename_parameters_in_files <- function(working.dir, out.dir, tab) {
    out.dir <- file.path(working.dir, out.dir)
    dir.create(out.dir, recursive = T)
    file.cols <- grep("Remove|Parameter", names(tab), invert = T)
    for(i in file.cols) {
        f.name <- file.path(working.dir, names(tab)[i])
        print(sprintf("Processing %s ...", f.name))
        fcs <- read_fcs(f.name)
        to.remove <- row.names(tab)[tab$Remove]
        if(length(to.remove) > 0)
            fcs <- remove_parameters(fcs, to.remove)


        names.map <- tab$Parameter
        names(names.map) <- row.names(tab)
        fcs <- rename_fcs_parameters_name(fcs, names.map)
        names.map <- tab[, i]
        names(names.map) <- tab$Parameter
        names.map <- names.map[!is.na(tab[, i])]
        fcs <- rename_fcs_parameters_desc(fcs, names.map)

        out.name <- file.path(out.dir, names(tab)[i])
        write_fcs(fcs, out.name)
    }
}


get_common_names <- function(tab) {
    ret <- apply(tab, 1, function(x) {
        x <- as.character(x)
        return(names(sort(table(x), decreasing = T)[1]))
    })

    return(as.vector(ret))
}

get_problem_idx <- function(tab, common.names) {
    ret <- tab != common.names
    ret[is.na(ret)] <- 1
    return(ret)
}

