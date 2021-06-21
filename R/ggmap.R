
ggmap = function(m, ...) {
    gmap(reshape2::melt(as.matrix(m)), ...) 
}
