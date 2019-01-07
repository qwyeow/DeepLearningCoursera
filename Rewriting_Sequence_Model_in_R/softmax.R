softmax <- function(x){
        top = exp(x-max(x))
        bottom = matrix(colSums(exp(x-max(x))), nrow = dim(top)[1], dim(top)[2], byrow =T)
        top/bottom
        
        
}