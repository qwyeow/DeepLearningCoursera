lstm_cell_forward <- function(xt, a_prev, c_prev, parameters) {
        
        Wf = parameters[["Wf"]]
        bf = parameters[["bf"]]
        Wi = parameters[["Wi"]]
        bi = parameters[["bi"]]
        Wc = parameters[["Wc"]]
        bc = parameters[["bc"]]
        Wo = parameters[["Wo"]]
        bo = parameters[["bo"]]
        Wy = parameters[["Wy"]]
        by = parameters[["by"]]
        
        n_x = dim(xt)[1]
        m   = dim(xt)[2]
        n_y = dim(Wy)[1]
        n_a = dim(Wy)[2]
        
        concat = array(0, c(n_x + n_a , m))
        concat[1:n_a, ] = a_prev
        concat[(n_a + 1 ):nrow(concat), ] = xt
        
        sigmoid <- function(x){ 1/(1+exp(-x))}
        Wf.concat = Wf %*%concat
        ft = sigmoid(Wf.concat + matrix(bf, nrow = dim(Wf.concat)[1], dim(Wf.concat)[2]))
        Wi.concat = Wi%*%concat
        it = sigmoid(Wi.concat + matrix(bi, nrow = dim(Wi.concat)[1], dim(Wi.concat)[2]))
        Wc.concat = Wc%*%concat
        cct = tanh(Wc.concat + matrix(bc, nrow = dim(Wc.concat)[1], dim(Wc.concat)[2]))
        c_next = c_prev*ft + cct*it
        Wo.concat = Wo %*%concat
        ot = sigmoid(Wo.concat + matrix(bo, nrow = dim(Wo.concat)[1], dim(Wo.concat)[2]))
        a_next = tanh(c_next)*ot
        
        source("softmax.R")
        Wy.a_next = Wy%*%a_next
        yt_pred = sigmoid(Wy.a_next + matrix(by, nrow = dim(Wy.a_next)[1], dim(Wy.a_next)[2]))
        
        
        cache = list("a_next" = a_next, "c_next" = c_next, "a_prev" =a_prev, "c_prev" =c_prev,
                     "ft" = ft, "it" = it, "cct" = cct, "ot" = ot, "xt" = xt,"parameters" = parameters)
        
        list("a_next" = a_next, "c_next" = c_next, "yt_pred" = yt_pred, "cache" = cache)
        
}