rnn_backward <- function(da, Caches) {
                source("rnn_cell_backward.R")
                caches = Caches$caches
                x = Caches$x
  
                a1 = caches[[1]]$a_next
                a0 = caches[[1]]$a_prev
                x1 = caches[[1]]$xt
                parameters = caches[[1]]$parameters
  
                n_a = dim(da)[1]
                m   = dim(da)[2]
                T_x = dim(da)[3]
                n_x = dim(x1)[1]
  
                dx = array(0,c(n_x, m, T_x))
                dWax = array(0,c(n_a, n_x))
                dWaa = array(0,c(n_a, n_a))
                dba = array(0,c(n_a, 1))
                da0 = array(0,c(n_a, m))
                da_prevt = array(0,c(n_a, m))
  
  
                for (t in rev(1:T_x)){
                        gradients = rnn_cell_backward(da[ , ,t] + da_prevt, caches[[t]])
          
                        dxt = gradients$dxt
                        da_prevt = gradients$da_prev
                        dWaxt = gradients$dWax
                        dWaat = gradients$dWaa
                        dbat = gradients$dba
          
                        dx[ , , t] = dxt
                        dWax = dWax + dWaxt
                        dWaa = dWaa + dWaat
                        dba =  dba + dbat
                        }
  
  
                da0 = da_prevt
                gradients = list("dx"= dx, "da0"= da0, "dWax"= dWax, "dWaa"= dWaa,"dba"= dba)
                gradients
}