lstm_forward <- function(x, a0, parameters) {
        caches = list()
        
        n_x = dim(x)[1]
        m   = dim(x)[2]
        T_x = dim(x)[3]
        
        n_y = dim(Wy)[1]
        n_a = dim(Wy)[2]
        
        a = array(0, c(n_a, m, T_x))
        c = array(0, c(n_a, m, T_x))
        y = array(0, c(n_y, m, T_x))
        
        a_next = a0
        c_next = array(0, c(n_a, m))
        
        for (t in 1:T_x){
                lstm.output <- lstm_cell_forward(x[ , ,t], a_next, c_next, parameters)
                a[ , ,t] <- lstm.output$a_next
                c[ , ,t] <- lstm.output$c_next
                y[ , ,t] <- lstm.output$yt_pred
                cache <- lstm.output$cache
                caches[[t]]=cache
        }
        
        Caches = list("caches" = caches, "x" =  x)
        list("a" = a, "y" = y, "c" = c, "Caches" = Caches)
}