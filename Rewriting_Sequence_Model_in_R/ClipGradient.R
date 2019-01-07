#library(ramify)
ClipGradient <- function(gradients, maxValue) {
        for(i in 1:length(gradients)){
                gradients[[i]] = clip(gradients[[i]], 
                                      .min = -maxValue, 
                                      .max = maxValue)}
                gradients
}
