##'  Plots nucleltide secondary structure as helices in arc diagram
##'
##' @title gghelix
##' @param helix_data a data frame. The file of nucleltide secondary structure
##' and then read by readSSfile().
##' @param overlap Logicals. If TRUE, two structures data called predict 
##' and known must be given(eg:heilx_data = list(known = data1, 
##'                                              predicted = data2)), 
##' plots the predicted helices that are known on top, predicted helices that
##'  are not known on the bottom, and finally plots unpredicted helices 
##'  on top in black.
##' @param color_by generate colors for helices by various rules,
##' including integer counts and value ranges one of "length" and "value"
##' @return ggplot object
##' @export
##' @examples
##' RF03120 <- system.file("extdata/Rfam/RF03120_SS.txt", package="ggmsa")
##' helix_data <- readSSfile(RF03120, type = "Vienna")
##' gghelix(helix_data)
##' @author Lang Zhou
gghelix <- function(helix_data, color_by = "length",overlap = FALSE){
    if(is.data.frame(helix_data)) {
        helix_tidy <- tidy_helix(helix_data, color_by = color_by)
    }else {
        helix_tidy <- tidy_list_helix(helix_data, color_by = color_by)
    }
    ly <- layer_helix(helix_data = helix_tidy, overlap = overlap)
    p <- ggplot() + ly + theme_helix()
    return(p)
}

##' The layer of helix plot
##'
##' @title geom_helix
##' @param helix_data a data frame. The file of nucleltide secondary structure
##' and then read by readSSfile().
##' @param overlap Logicals. If TRUE, two structures data called predict 
##' and known must be given(eg:heilx_data = list(known = data1, 
##'                                              predicted = data2)), 
##' plots the predicted helices that are known on top,
##' predicted helices that are not known on the bottom, and finally plots 
##' unpredicted helices on top in black.
##' @param color_by generate colors for helices by various rules,
##' including integer counts and value ranges one of "length" and "value"
##' @param ... additional parameter
##' @return ggplot2 layers
##' @export
##' @examples
##' RF03120 <- system.file("extdata/Rfam/RF03120_SS.txt", package="ggmsa")
##'RF03120_fas <- system.file("extdata/Rfam/RF03120.fasta", package="ggmsa")
##'SS <- readSSfile(RF03120, type = "Vienna")
##'ggmsa(RF03120_fas, font = NULL,border = NA, 
##'     color = "Chemistry_NT", seq_name = FALSE) +
##'geom_helix(SS)
##' @author Lang Zhou
geom_helix <- function(helix_data, color_by = "length", overlap = FALSE,  ...) {
  structure(list(helix_data = helix_data,
                 color_by = color_by,
                 overlap = overlap),
            class = "nucleotideeHelix")
}

##' Read secondary structure file
##'
##' @title readSSfile
##' @importFrom utils read.delim
##' @param file A text file in connect format
##' @param type file type. one of "Helix, "Connect", "Vienna" and "Bpseq"
##' @return data frame
##' @importFrom R4RNA readHelix
##' @importFrom R4RNA readConnect
##' @importFrom R4RNA readVienna
##' @importFrom R4RNA readBpseq
##' @importFrom R4RNA expandHelix
##' @importFrom R4RNA collapseHelix
##' @export
##' @examples
##' RF03120 <- system.file("extdata/Rfam/RF03120_SS.txt", package="ggmsa")
##' helix_data <- readSSfile(RF03120, type = "Vienna")
##' @author Lang Zhou
readSSfile <- function(file, type = NULL) {
    type <- match.arg(type, c("Helix", "Connect", "Vienna", "Bpseq"))
    load_data <- switch(type,
                        Helix = readHelix(file),
                        Connect = readConnect(file),
                        Vienna = readVienna(file),
                        Bpseq = expandHelix(file))

    data <- collapseHelix(load_data)
    return(data)

}

tidy_list_helix <- function(helix_data, color_by = "length"){
  known <- tidy_helix(helix_data$known, color_by = color_by)
  predicted <-  tidy_helix(helix_data$predicted, color_by = color_by)
  return(list(known = known, predicted = predicted))
}

tidy_helix <- function(helix_data, color_by = "length"){
    helix_data <- color_helix(helix_data, color = color_by)
    names(helix_data)[c(1,2)] <- c("from","to")
    helix_data$x0 <- (helix_data$to + helix_data$from)/2
    helix_data$r <- (helix_data$to - helix_data$from)/2
    return(helix_data)
}

color_helix <- function(helix_data, color){
    #color <- match.arg(color, c("length", "value"))
    if(color == "length"){
        data_color <- colorBy_length(helix_data)
    }else if(color == "value") {
        data_color <- colorBy_value(helix_data)
    }else {
      helix_data$col <- color
      data_color <- helix_data
    }
      data <- expandHelix(data_color)
      return(data)
}

colorBy_length <- function(helix_data){
    pal_lenght <- colorRampPalette(brewer.pal(name = "Paired", n = 12))
    helix_data$col <- nrow(helix_data) %>% pal_lenght()
    return(helix_data)
}

colorBy_value <- function(helix_data){
    pal_value <- colorRampPalette(rev(brewer.pal(name = "Blues", n = 4)))
    helix_data$col <- nrow(helix_data) %>% pal_value()
    return(helix_data)
}

##' @importFrom ggforce geom_arc
layer_helix <- function(helix_data, overlap = FALSE, seq_numbers = 0){
    mapping_above <- aes_(x0 = ~x0, 
                          y0 = ~(seq_numbers + 0.5), 
                          r = ~r, start = ~1.5*pi, 
                          end = ~2.5*pi)
    mapping_below <- aes_(x0 = ~x0, 
                          y0 = ~(-0.5), 
                          r = ~r, start = ~pi/2, 
                          end = ~1.5*pi)
    if(seq_numbers > 0) {
        mapping_below <- modifyList(mapping_below, aes_(y0 = ~0))
    }
    if(is.list(helix_data) & "col" %in% names(helix_data[[2]])) {
        mapping_above <- modifyList(mapping_above, aes_(color = ~I(col)))
        mapping_below <- modifyList(mapping_below, aes_(color = ~I(col)))
      }

    if(overlap) {
        if(!is.list(helix_data)| length(helix_data) != 2){
            stop("Overlapping structures must input a list with
                 2 helix data.
                 (eg: heilx_data = list(known = data1, predicted = data2)")
        }
        if(!names(helix_data) %in% c("known", "predicted") %>% all) {
            stop("helix_data names must be 'known' and 'predicted'. 
                 (eg: heilx_data = list(known = data1, predicted = data2)")
        }

        overlap_data <- overlap_helix(known = helix_data[["known"]],
                                      predicted = helix_data[["predicted"]])

        if (overlap_data[["above_justknown"]] %>% nrow == 0){
            ly_up <- geom_arc(data = overlap_data[["above_both"]],
                              mapping = mapping_above)
            ly_below <- geom_arc(data = overlap_data[["below"]], 
                                 mapping = mapping_below)
            return(list(ly_up, ly_below))

        }else {
            ly_up <- geom_arc(data = overlap_data[["above_both"]],
                              mapping = mapping_above)
            ly_up_justknown <- 
              geom_arc(data = overlap_data[["above_justknown"]], 
                       mapping = mapping_above, 
                       color = "black")
            
            ly_below <- geom_arc(data = overlap_data[["below"]], 
                                 mapping = mapping_below)
            return(list(ly_up, ly_up_justknown, ly_below))
        }

    }else {#overlap = FALSE
        if(is.list(helix_data) & length(helix_data) == 2) {
            if(!"col" %in% names(helix_data[["known"]])) {
                mapping_below <- modifyList(mapping_below, 
                                            aes_(color = I("#8fce5e")))
            }
            ly_up <- geom_arc(data = helix_data[["known"]], 
                              mapping = mapping_below)
            ly_below <- geom_arc(data = helix_data[["predicted"]], 
                                 mapping = mapping_above)
            return(list(ly_up, ly_below))

        }else if(is.data.frame(helix_data)){
            if("col" %in% names(helix_data)){
                mapping_above <- modifyList(mapping_above, 
                                            aes_(color = ~I(col)))
            }
            ly_arc <- geom_arc(data = helix_data, mapping = mapping_above)
            return(ly_arc)
        }else {
            stop("Only a data frame or a list with 2 of helix data are allowed.
                 eg: heilx_data = data or 
                 heilx_data = list(known = data1, predicted = data2)")
        }
    }
}

overlap_helix <- function(known, predicted){
    if(!c("from", "to") %in% names(known) %>% all) {
        stop("'known' must be a output from 'readSSfile()'")
    }
    if(!c("from", "to") %in% names(predicted) %>% all) {
        stop("'predicted' must be a output from 'readSSfile()'")
    }

    known$heli <- paste0(known$from, "t",known$to)
    predicted$heli <- paste0(predicted$from, "t", predicted$to)

    below <- predicted[!predicted$heli %in% known$heli,] #predicted & not known
    above_both <- predicted[predicted$heli %in% known$heli,] #predicted & known
    above_justknown <- known[!known$heli %in% above_both$heli,] #unpredicted & known

    return(list(below = below,
                above_both = above_both,
                above_justknown = above_justknown))
}

##' @importFrom ggplot2 theme_void
##' @importFrom ggplot2 element_text
##' @importFrom grid arrow
theme_helix <- function(){
    list(theme_void(),
         scale_y_continuous(breaks = 0),
         coord_fixed(),
         theme(panel.grid.major.y = element_line(size = 1, arrow = arrow(length = unit(0.3, 'cm'))),
               panel.grid.major.x = element_line(color = "#eaeaea", size = 0.4),
               axis.text.x = element_text())
         )
  }




