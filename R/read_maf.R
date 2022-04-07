##' read 'multiple alignment format'(MAF) file
##'
##' @title read_maf
##' @param multiple_alignment_format a multiple alignment format(MAF) file
##' @return data frame
##' @export
##' @author Lang Zhou
read_maf <- function(multiple_alignment_format) {
    
    line <- readLines(multiple_alignment_format)
    head <- sapply(line, function(i) substring(i,1,1))
    rm(line)# 'line' in names(heads) 
    
    #remove header
    head <- head[-seq(which(head == "#"))]
    
    #split block
    blank <- which(head == "")
    block_ls <- lapply(seq(blank), function(i) {
        if (blank[i] == min(blank)) {
            x <- names(head)[1:blank[i]]
        }else {
            x <- names(head)[blank[i-1]:blank[i]]
        }
        return(x)
    })
    names(block_ls) <- paste0("block_",seq(length(block_ls)))
    
    #extra lines starting with "s"
    s_block <- lapply(seq(length(block_ls)), function(i) {
        blocki <- block_ls[[i]]
        line_s <- blocki[sapply(blocki, function(j) substring(j,1,1))  == "s"] 
    }) 
    names(s_block) <- names(block_ls)
    
    #get a MAF df
    s_name <- c("type", "src", "start", "size", "strand", 'src_size', "text")
    seq_df <-lapply(seq(length(s_block)), function(i) {
        
        blocki <- s_block[[i]]
        seq_df <- lapply(seq(length(blocki)), function(j) {
            x <- blocki[[j]]
            #extra all columns
            x <- strsplit(x, " ") %>% unlist 
            x1 <- x[sapply(x, nchar) > 0]
            #convert to data frame
            seq <- t(as.matrix(x1)) %>% as.data.frame()
            names(seq) <- s_name
            seq[,c("start","size",'src_size')] <- 
                seq[,c("start","size",'src_size')] %>%as.numeric()
            
            seq$size_gap <- nchar(seq$text)
            seq$end <- seq$start + seq$size
            seq$end_gap <- seq$start + seq$size_gap
            seq$block <- names(s_block[i])
            return(seq)
        })%>% do.call("rbind", .)
        return(seq_df)
        
    }) %>% do.call("rbind", .)
}
