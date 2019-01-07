
rnn_cell_forward <- function(xt, a_prev, parameters){
        
                        source("softmax.R")
                        
                        Wax = parameters[["Wax"]]
                        Waa = parameters[["Waa"]]
                        Wya = parameters[["Wya"]]
                        ba = parameters[["ba"]]
                        by = parameters[["by"]]    
        
                        Wax.aa = Wax%*%xt + Waa %*%a_prev 
                        a_next = tanh(Wax.aa + matrix(ba, nrow = dim(Wax.aa)[1], dim(Wax.aa)[2]))
                        Wya.a_next <- Wya %*% a_next
                        yt_pred = softmax(Wya.a_next + matrix(by, nrow = dim(Wya.a_next)[1], dim(Wya.a_next)[2]))
                        cache = list("a_next" = a_next, "a_prev" =a_prev, "xt" = xt, "parameters" = parameters)
                        list("a_next" = a_next, "yt_pred" = yt_pred, "cache" = cache)
}


