tidy_color <- function(y, consensus, disagreement) { 
    c <- lapply(unique(y$position), function(i) {
        msa_cloumn <- y[y$position == i, ]
        cons_char  <- consensus[consensus$position == i, "character"]
        logic <- msa_cloumn$character == cons_char
        if(cons_char == "X") {
          msa_cloumn$color <- NA
        }
        if (disagreement) {
            msa_cloumn[logic, "color"] <- NA 
        }else {
            msa_cloumn[!logic, "color"] <- NA 
        }
        msa_cloumn
    }) %>% do.call("rbind", .)
    return(c)
}


get_consensus <- function(tidy) {
    cons <- lapply(unique(tidy$position), function(i) { #Iterate through each columns
        msa_cloumn <- tidy[tidy$position == i, ] 
        cons <- data.frame(position = i)
        # if(ignore_gaps) {
        #     msa_cloumn <- tidy[tidy$position == i&!tidy$character %in% "-", ] 
        # }
        #Gets the highest frequency characters
        fre <- table(msa_cloumn$character) %>% data.frame
        max_element <- fre[fre[2] == max(fre[2]),]
        max_number <-  max_element %>% nrow
        if(max_number == 1) {
            cons$character <- max_element[1,1]
        }else {
            cons$character <- "X"
        }
        cons
        }) %>% do.call("rbind", .)
  
        cons$name = "Consensus"
        return(cons)
}


order_name <- function(name, order = NULL, consensus_views = FALSE) {
    name_uni <- unique(name)
    if(consensus_views){
        #placed 'consensus' at the top
        name_expect <- name_uni[!name_uni %in% "Consensus"] %>% rev
        name <- factor(name, levels = c(name_expect, "Consensus"))
    }else{
        name_levels <- rev(name_uni)
        name <- factor(name, levels = name_levels)
    }
    #adjust the msa order according to 'order'
    if(!is.null(order)) {
        if(!length(name_uni) == length(order)) {
        stop("The 'order' length does not match the number of names")
      }
      name_levels <- levels(name)[order]
      name <- factor(name, levels = name_levels)
    }
    return(name)
}
ggmsa(protein_sequences, 164, 213, color="Chemistry_AA", consensus_views = T, use_dot = T)
