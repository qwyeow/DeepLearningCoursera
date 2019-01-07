lstm_cell_backward <- function(da_next, dc_next, cache) {
        
        a_next = cache$a_next
        c_next = cache$c_next
        a_prev = cache$a_prev
        c_prev = cache$c_prev
        ft = cache$ft
        it = cache$it
        cct = cache$cct
        ot = cache$ot
        xt = cache$xt
        parameters = cache$parameters
        
        n_x = dim(xt)[1]
        m   = dim(xt)[2]
        n_a = dim(a_next)[1]
        m = dim(a_next)[2]
        
        dot = da_next*tanh(c_next)*ot*(1-ot)
        dcct = (dc_next*it+ ot*(1-(tanh(c_next))^2)*it*da_next)*(1-(cct)^2)
        dit = (dc_next*cct + ot*(1-(tanh(c_next))^2)*cct*da_next)*it*(1-it)
        dft = (dc_next*c_prev + ot*(1-(tanh(c_next))^2)*c_prev*da_next)*ft*(1-ft)
        
        
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
        
        dWf = dft %*% t(rbind(a_prev,xt))
        dWi = dit %*% t(rbind(a_prev,xt))
        dWc = dcct %*% t(rbind(a_prev,xt))
        dWo = dot %*% t(rbind(a_prev,xt))
        
        dbf = rowSums(dft)
        dbi = rowSums(dit)
        dbc = rowSums(dcct)
        dbo = rowSums(dot)
        
        da_prev = t(Wf[ ,1:n_a]) %*% dft + t(Wi[ ,1:n_a]) %*% dit + t(Wc[ ,1:n_a]) %*% dcct + t(Wo[ ,1:n_a]) %*% dot
        dc_prev =  dc_next*ft + ot*(1-(tanh(c_next))^2)*ft*da_next
        dxt = t(Wf[ ,(n_a + 1):ncol(Wf)])%*% dft + t(Wi[ ,(n_a + 1):ncol(Wi)])%*% dit + t(Wc[ ,(n_a + 1):ncol(Wc)])%*% dcct + t(Wo[ ,(n_a + 1):ncol(Wo)])%*% dot
        
        gradients = list("dxt"= dxt, "da_prev"= da_prev, "dc_prev"= dc_prev, "dWf"= dWf,"dbf"= dbf,
                         "dWi"= dWi,"dbi"= dbi,"dWc"= dWc,"dbc"= dbc, "dWo"= dWo,"dbo"= dbo)
        
        gradients
        
}