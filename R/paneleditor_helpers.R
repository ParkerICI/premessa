
read_parameters <- function(files.list) {
    ret <- lapply(files.list, function(f) {
        fcs <- flowCore::read.FCS(f)
        df <- data.frame(name = as.character(flowCore::parameters(fcs)$name), 
            desc = as.character(flowCore::parameters(fcs)$desc), check.names = F,
            stringsAsFactors = F)
        names(df) <- gsub("desc", f, names(df))
        return(df)

    })

    ret <- Reduce(function(a, b) {
        merge(a, b, by = "name", all.x = T, all.y = T)
    }, ret)
    row.names(ret) <- ret$name
    ret$name <- NULL
    return(ret)
}



#old.names and new.names are named vectors
#mapping parameter names to descriptions the two vectors need
#to have the same ordering

rename_fcs <- function(fcs, old.names, new.names) {
    ret <- fcs
    stopifnot(!any(duplicated(names(old.names))))
    stopifnot(!any(duplicated(names(new.names))))

    #this allows the conversion of names
    p.names.map <- names(new.names)
    names(p.names.map) <- names(old.names)

    colnames(ret$m) <- p.names.map[colnames(ret$m)]
    ret$desc <- new.names[colnames(ret$m)]
    return(ret)
}


process_files <- function(working.dir, old.tab, new.tab) {
    for(i in 1:ncol(old.tab)) {
        f.name <- file.path(working.dir, names(old.tab)[i])
        fcs <- read_fcs(f.names)
    }

}

get_most_common_names <- function(tab) {
    ret <- apply(tab, 1, function(x) {
        return(names(sort(table(x))[1]))
    })
    return(as.vector(ret))
}