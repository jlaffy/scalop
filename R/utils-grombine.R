.is_grob <- function(x) {
    'grob' %in% class(x)
} 

.which_to_grob <- function(x) {
    sapply(x, function(i) !is_grob(i))
}

.add_rows <- function(Grob, n) {
    if (n < 1) {return(Grob)}
    for (i in 1:n) {
        Grob <- gtable::gtable_add_rows(Grob, unit(0, "null"))
    }

    Grob
}


.add_cols <- function(Grob, n) {
    if (n < 1) {return(Grob)}
    for (i in 1:n) {
        Grob <- gtable::gtable_add_cols(Grob, unit(0, "null"))
    }
    Grob
}


.equalize_grob_dim <- function(grobs, FUN.count = ncol, FUN.add = .add_cols) {
    Dims <- sapply(grobs, FUN.count) %>% unlist(use.names = F)
    dimmax <- max(Dims)
    dimdifs <- dimmax - Dims
    Map(FUN.add, Grob = grobs, n = dimdifs)
}

