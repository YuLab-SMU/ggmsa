##' assign dms value to alignments.
##'
##' @title assign_dms
##' @param x data frame from tidy_msa()
##' @param dms dms data frame
##' @return tree
##' @export
##' @author Lang Zhou

assign_dms <- function(x, dms) {
    dms_value <- lapply(unique(x$position), function(i) {
        xx <- x[x$position == i,]
        dmss <- dms[dms$site_RBD == i,]

        wt <- unique(as.character(dmss[, "wildtype"]))
        if (nrow(dmss) == 0 || length(wt) != 1) {
            xx$mutation <- NA_character_
            xx$bind_avg <- NA_real_
            return(xx)
        }

        xx$mutation <- paste0(wt, xx$position, xx$character)
        xx$bind_avg <- dmss$bind_avg[match(xx$mutation, dmss$mutation_RBD)]

        return(xx)
    }) %>% do.call("rbind",.)
    return(dms_value )
}







