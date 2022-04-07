##' assign MDS value to alignments.
##'
##' @title assign_mds
##' @param x data frame from tidy_msa()
##' @param mds MDS data frame
##' @return tree
##' @export
##' @author Lang Zhou

assign_mds <- function(x, mds) {
    mds_value <- lapply(unique(x$position), function(i) {
        xx <- x[x$position == i,]
        mdss <- mds[mds$site_RBD == i,]
        
        wt <- unique(mdss[,"wildtype"])
        xx$mutation <- paste0(wt, xx$position, xx$character)
        xx$bind_avg  <- lapply(seq_along(xx$mutation),function(j) {
            bind_avg <- mdss[mdss$mutation_RBD %in% xx[j,"mutation"],"bind_avg"]
            return(bind_avg)
        }) %>% unlist
        
        return(xx)
    }) %>% do.call("rbind",.)
    return(mds_value )
}







