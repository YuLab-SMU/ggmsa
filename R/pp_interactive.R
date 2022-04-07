
make_gap <- function(gap, previous_seq) {
    gap_df <- previous_seq[rep(1, each=gap),] 
    gap_start <- max(previous_seq$position) + 1
    gap_df$position <- gap_start : (gap_start + gap - 1 )
    gap_df$character <- "-"
    
    if("pos_previous"  %in% names(gap_df)) {
        gap_df$pos_previous <- 0
    }
    
    return(gap_df)
}

##' merge two MSA
##'
##' @title merge_seq
##' @param previous_seq previous MSA
##' @param subsequent_seq subsequent MSA
##' @param gap gap length
##' @param adjust_name logical value. merge seq name or not
##' @return tidy MSA data frame
##' @export
##' @author Lang Zhou
merge_seq <- function(previous_seq, gap, subsequent_seq, adjust_name = TRUE) {
    
    name_pre <- levels(previous_seq$name)
    name_subse <- levels(subsequent_seq$name)
    
    if(length(name_pre) != length(name_subse)) {
        stop("The sequences number of previous_seq and subsequent_seq is inconsistent")
    }
    
    gap_df <- make_gap(gap = gap, previous_seq = previous_seq)
    subsequent_seq$position <- 
        subsequent_seq$position - min(subsequent_seq$position) + 1
    subsequent_seq$position <- 
        subsequent_seq$position + max(previous_seq$position) + gap
    
    t_merge <- rbind(previous_seq,gap_df,subsequent_seq)
    
    if (adjust_name) {
        rownames(t_merge) <- seq(nrow(t_merge))
        names(t_merge)[1] <- "name_previous"
        t_merge$name <- ""
        
        for(i in seq(length(name_pre))) {
            t_merge[t_merge$name_previous %in% c(name_pre[i], name_subse[i]),"name"] <- 
                paste0(name_pre[i],"-", name_subse[i])
        }
        t_merge$name <- factor(t_merge$name)
    }
    return(t_merge)
}


##' tidy protein-protein interactive position data
##'
##' @title tidy_hdata
##' @param gap gap length
##' @param inter protein-protein interactive position data
##' @param previous_seq previous MSA
##' @param subsequent_seq subsequent MSA
##' @importFrom R4RNA as.helix
##' @return helix data
##' @export
##' @author Lang Zhou
tidy_hdata <- function(gap, inter, previous_seq,subsequent_seq) {
    inter$j <- inter$Res.no..2 - 
        min(subsequent_seq$position) + 
        max(previous_seq$position) + gap + 1
    hdata <- data.frame(i = inter$Res.no.1, 
                        j = inter$j,
                        length = 1, 
                        value = NA, 
                        colour = "blue")
    hdata <- as.helix(hdata)
    return(hdata)
}

##' reset MSA position
##'
##' @title reset_pos
##' @param seq_df MSA data
##' @return data frame
##' @export
##' @author Lang Zhou
reset_pos <- function(seq_df) {
    names(seq_df)[2] <- "pos_previous"
    seq_df$position <- ""
    
    for(i in unique(seq_df$pos_previous)%>% seq) {
        uni <- unique(seq_df$pos_previous)
        seq_df[seq_df$pos_previous == uni[i],"position"] <- i
    }
    
    seq_df$position <- as.numeric(seq_df$position)
    return(seq_df)
    
}

##' reset hdata data position
##'
##' @title simplify_hdata 
##' @param hdata data from tidy_hdata()
##' @param sim_msa MSA data frame
##' @return data frame
##' @export
##' @author Lang Zhou
simplify_hdata <- function(hdata, sim_msa) {
    
    new_hdata <- lapply(seq(nrow(hdata)), function(a) {
        n <- hdata[a,]
        n$pre_i <- n$i
        n$i <- sim_msa[sim_msa$pos_previous == n$i,"position"] %>% unique
        return(n)
    }) %>% do.call("rbind",.)
    
    new_hdata <- lapply(seq(nrow(new_hdata)), function(a) {
        n <- new_hdata[a,]
        n$pre_j <- n$j
        n$j <- sim_msa[sim_msa$pos_previous == n$j,"position"] %>% unique
        return(n)
    }) %>% do.call("rbind",.)
    
    new_hdata <- as.helix(new_hdata)
    
    return(new_hdata)
    
}









