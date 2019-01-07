rnn_step_forward <- function(parameters, a_prev,xt) {
        
        Waa = parameters[['Waa']]
        Wax = parameters[['Wax']]
        Wya = parameters[['Wya']]
        by = parameters[['by']]
        b = parameters[['b']]
        
        Wax.aa = Wax%*%xt + Waa %*%a_prev 
        a_next = tanh(Wax.aa + matrix(b, nrow = dim(Wax.aa)[1], dim(Wax.aa)[2]))
        Wya.a_next <- Wya %*% a_next
        yt_pred = softmax(Wya.a_next + matrix(by, nrow = dim(Wya.a_next)[1], dim(Wya.a_next)[2]))
        
        list("a_next" = a_next, "p_t" = yt_pred)
}