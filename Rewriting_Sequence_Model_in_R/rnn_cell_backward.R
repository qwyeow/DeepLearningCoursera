rnn_cell_backward <- function(da_next, cache) {
        
                        a_next = cache$a_next
                        a_prev = cache$a_prev
                        xt = cache$xt
                        parameters = cache$parameters
        
        
                        Wax = parameters[["Wax"]]
                        Waa = parameters[["Waa"]]
                        Wya = parameters[["Wya"]]
                        ba = parameters[["ba"]]
                        by = parameters[["by"]]
        
                        Wax.aa = Wax%*%xt + Waa %*%a_prev 
                        dtanh = da_next*(1-(tanh(Wax.aa + matrix(ba, nrow = dim(Wax.aa)[1], dim(Wax.aa)[2])))^2)
                        dxt = t(Wax)%*%dtanh
                        dWax = dtanh%*%t(xt)
                        da_prev = t(Waa)%*%dtanh
                        dWaa = dtanh %*% t(a_prev)
                        dba = rowSums(dtanh)
        
                        gradients = list("dxt"=dxt, "da_prev"=da_prev, "dWax"=dWax, "dWaa"=dWaa, "dba"=dba)
                        gradients
}