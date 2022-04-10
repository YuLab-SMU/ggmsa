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
        
        wt <- unique(dmss[,"wildtype"])
        xx$mutation <- paste0(wt, xx$position, xx$character)
        xx$bind_avg  <- lapply(seq_along(xx$mutation),function(j) {
            bind_avg <- dmss[dmss$mutation_RBD %in% xx[j,"mutation"],"bind_avg"]
            return(bind_avg)
        }) %>% unlist
        
        return(xx)
    }) %>% do.call("rbind",.)
    return(dms_value )
}







