    
ggraster = function(m, ...) {
    graster(reshape2::melt(as.matrix(m)), ...) 
}
