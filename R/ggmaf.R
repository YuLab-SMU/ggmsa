##' plot MAF
##'
##' @title ggmaf 
##' @param data a tidy MAF data frame.You can get it by tidy_maf_df() 
##' @param ref character, the name of reference genome. 
##' eg:"hg38.chr1_KI270707v1_random"
##' @param block_start a numeric vector(>0). The start block to plot.
##' @param block_end a numeric vector(< max block). The end block to plot.
##' @param facet_field a numeric vector. The field in a facet panel.
##' @param heights two numeric vector.The plot proportion between 
##' "Genomic location" panel(upon) and "Alignment" panel(down).
##' Default:c(0.4,0.6)
##' @param facet_heights Numeric vectors.The facet proportion.
##' @return ggplot object
##' @export
##' @author Lang Zhou
ggmaf <- function(data, 
                  ref, 
                  block_start = NULL, 
                  block_end = NULL, 
                  facet_field = NULL, 
                  heights = c(0.4,0.6),
                  facet_heights = NULL) {
  
  d <- data[data$block_number %in% c(block_start : block_end),]
  
  if(is.null(facet_field)) {
    maf_p <- maf_plot(d = d, ref = ref)
    p <- plot_list(gglist = maf_p, heights = heights)
    return(p)
  }else {
    d <- facet_maf(mafData = d, field = facet_field)
    p_ls <- lapply(unique(d$facet), function(i) {
      facet_d <- d[d$facet == i,]
      maf_p <- maf_plot(d = facet_d, ref = ref)
      pp <- plot_list(gglist = maf_p, heights = heights)
      return(pp)
    })
    p <- plot_list(gglist = p_ls, ncol =  1, heights = facet_heights)
    return(p)
  }
}


##' tidy MAF data frame 
##'
##' @title tidy_maf_df
##' @param maf_df a MAF data frame.You can get it by read_maf() 
##' @param ref character, the name of reference genome. 
##' eg:"hg38.chr1_KI270707v1_random"
##' @return data frame
##' @export
##' @author Lang Zhou
tidy_maf_df <- function(maf_df,ref) {
  ##add ref position to other genome
  block_num <- unique(maf_df$block)
  tidy_df <- lapply(block_num, function(i) {
    x <- maf_df[maf_df$block == i,]
    x$ref_start <- x[x$src == ref, "start"]
    x$ref_end <- x[x$src == ref, "end_gap"]
    return(x)
  })%>% do.call("rbind", .)
  
  tidy_df$block_number <- factor(tidy_df$block, levels = 
                                   unique(tidy_df$block)) %>% as.numeric
  tidy_df$bs <- paste0(tidy_df$src,"-",tidy_df$block) 
  tidy_df$merge_y <- factor(tidy_df$src) %>% as.numeric
  tidy_df$label <- paste0("B",tidy_df$block_number)
  tidy_df <- order_aln(tidy_df,ref)
  return(tidy_df)
  
}


#put the ref sequence the first in each block, new col "y"
order_aln <- function(tidy_df, ref) {
    block_num <- unique(tidy_df$block)
    lev <- sapply(block_num, function(i) {
        x <- tidy_df[tidy_df$block == i,]
        order <- c(ref, x$src[!x$src %in% ref]) 
        
        lev <- paste0(order, "-",x$block)  
        return(lev)
    })%>% unlist %>% rev
    tidy_df$y <- factor(tidy_df$bs,levels = lev) %>% as.numeric
    return(tidy_df)
}

##' @importFrom utils getFromNamespace
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_text
maf_plot <- function(d, ref, 
                     positive_color = "#a9c9d4",
                     negative_color = "#ffa389") {
  geom_rrect <- getFromNamespace("geom_rrect","statebins")
  ##plot down panel
  p_maf_aln <- ggplot(data = d) + 
    geom_rrect(mapping=aes_(xmin =~ ref_start,
                            xmax =~ ref_end,
                            ymin =~ y - 0.3,
                            ymax =~ y + 0.3,
                            fill =~ strand)) +
    geom_rrect(data = d,
               mapping=aes_(xmin =~ ref_start,
                            xmax =~ ref_end,
                            ymin =~ max(y) + 1 - 0.3,
                            ymax =~ max(y) + 1 + 0.3),
               fill = "#a9c9d4",color = "black") +
    scale_y_continuous(breaks = c(d$y,max(d$y + 1)),labels = c(d$bs, ref)) +
    scale_fill_manual(breaks = c("+","-"),
                      values = c(positive_color,negative_color)) +
    theme_void() +
    theme(axis.text.x = element_text(),
          axis.text.y = element_text(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "grey"))
  
  ##plot upon panel
  aim <- d[d$src != ref, ]
  p_maf_genomePos <- ggplot(data = aim) + 
    geom_rrect(mapping = aes_(xmin =~ start,
                              xmax =~ end_gap,
                              ymin =~ merge_y - 0.3,
                              ymax =~ merge_y + 0.3,
                              fill =~ strand),
               color = "black", 
               size = 0.5, 
               alpha = 0.8, 
               show.legend = FALSE) + 
    scale_y_continuous(breaks = unique(aim$merge_y),
                       labels = unique(aim$src)) +
    scale_fill_manual(breaks = c("+","-"),
                      values = c(positive_color,negative_color)) +
    theme_void() + theme(panel.grid.major.y = element_line(color = "grey"),
                         axis.text.x = element_text(),
                         axis.text.y = element_text(),
                         strip.text = element_blank()) + 
    geom_text(aes_(x =~ (start + end_gap)/2, 
                   y =~ merge_y,label =~ label), 
              size = 3) +
    facet_wrap(~src, scales = "free", ncol = 1)
  return(list(p_maf_genomePos, p_maf_aln))
}

#assign facet number to blocks
facet_maf <- function(mafData, field) {
    
    if(min(mafData$block_number) > 1){
        pos_reset <- mafData$block_number - min(mafData$block_number) + 1
        #pos_reset[pos_reset == 0] <- 1
    }else {
        pos_reset <- mafData$block_number
    }
    mafData$facet <- pos_reset %/% field
    
    mafData[(pos_reset %% field) == 0, "facet"] <-
        mafData[(pos_reset %% field) == 0, "facet"] - 1
    
    return(mafData)
}











