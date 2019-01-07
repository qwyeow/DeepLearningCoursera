softmax <- function(x){
                numerator = exp(x-max(x))
                denominator = matrix(colSums(exp(x-max(x))), nrow = dim(numerator)[1], dim(numerator)[2], byrow =T)
                numerator/denominator
        
        
}