setClass("SeqDiff",
         representation = representation(
             file = "character",
             sequence = "BStringSet",
             reference = "numeric",
             diff = "data.frame"
         )
         )
