lstm_backward <- function(da, caches) {
        caches = Caches$caches
        x = Caches$x
        
        a1 = caches[[1]]$a_next
        c1 = caches[[1]]$c_next
        a0 = caches[[1]]$a_prev
        c0 = caches[[1]]$c_prev
        f1 = caches[[1]]$ft
        i1 = caches[[1]]$it
        cc1 = caches[[1]]$cct
        o1 = caches[[1]]$ot
        x1 = caches[[1]]$xt
        parameters = caches[[1]]$parameters
        
        
        n_a = dim(da)[1]
        m   = dim(da)[2]
        T_x = dim(da)[3]
        n_x = dim(x1)[1]
        
        dx = array(0,c(n_x, m, T_x))
        da0 = array(0,c(n_a, m))
        da_prevt = array(0,c(n_a, m))
        dc_prevt = array(0,c(n_a, m))
        dWf = array(0,c(n_a, n_a + n_x))
        dWi = array(0,c(n_a, n_a + n_x))
        dWc = array(0,c(n_a, n_a + n_x))
        dWo = array(0,c(n_a, n_a + n_x))
        dbf = array(0,c(n_a, 1))   
        dbi = array(0,c(n_a, 1))
        dbc = array(0,c(n_a, 1))
        dbo = array(0,c(n_a, 1))
        
        for (t in rev(1:T_x)){
                gradients = lstm_cell_backward(da[ , ,t] + da_prevt, dc_prevt, caches[[t]])
                dxt = gradients$dxt
                dWf = gradients$dWf
                dWi = gradients$dWi
                dWc = gradients$dWc
                dWo = gradients$dWo
                dbf = gradients$dbf
                dbi = gradients$dbi
                dbc = gradients$dbc
                dbo = gradients$dbo
                da_prevt = gradients$da_prev
                dc_prevt = gradients$dc_prev
        }
        
        da0 = gradients$da_prev
        
        gradients = list("dx"= dx, "da0"= da0, "dWf"= dWf,"dbf"= dbf, "dWi"= dWi,"dbi"= dbi,
                         "dWc"= dWc,"dbc"= dbc, "dWo"= dWo,"dbo"= dbo)
        gradients
}