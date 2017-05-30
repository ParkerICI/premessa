
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

    #ret <- lapply(ret,  function(x) {
    #    x[is.na(x)] <- "NA" 
    #    return(x)
    #})

    ret <- data.frame(ret, check.names = F, stringsAsFactors = F)

    row.names(ret) <- ret$name
    ret$name <- NULL

    return(ret)
}



#old.names and new.names are named vectors
#mapping parameter names to descriptions the two vectors need
#to have the same ordering. also all the names need be present in
#the fcs file

rename_fcs <- function(fcs, old.names, new.names) {
    ret <- fcs
    stopifnot(!any(duplicated(names(old.names))))
    stopifnot(!any(duplicated(names(new.names))))
    stopifnot(names(old.names) == names(new.names))
    stopifnot(dim(old.names) == dim(new.names))
    stopifnot(all(names(old.names) %in% colnames(fcs$m)))

    #this allows the conversion of names
    #maps old names to new ones
    p.names.map <- names(new.names)
    names(p.names.map) <- names(old.names)
    
    #make sure columns are in the right order
    ret$m <- ret$m[, names(p.names.map)]

    colnames(ret$m) <- p.names.map[colnames(ret$m)]
    ret$desc <- new.names[colnames(ret$m)]
    return(ret)
}

# names.map: old FCS parameter names -> new FCS parameter names
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

# names.map: FCS name -> FCS desc
rename_fcs_parameters_desc <- function(fcs, names.map) {
    ret <- fcs
    stopifnot(all(colnames(ret$m) %in% names(names.map)))

    ret$desc <- names.map[colnames(ret$m)]
    return(ret)
}

#Tab [, c("Remove", "Parameter",....)]


process_files <- function(working.dir, out.dir, tab) {
    out.dir <- file.path(working.dir, out.dir)
    dir.create(out.dir, recursive = T)
    for(i in 3:ncol(tab)) {
        f.name <- file.path(working.dir, names(tab)[i])
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

