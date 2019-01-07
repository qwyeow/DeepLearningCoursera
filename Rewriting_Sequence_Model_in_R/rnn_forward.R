rnn_forward <- function(x, a0, parameters) {
        
        source("rnn_cell_forward.R")
        
        n_x <- dim(x)[1]
        m   <- dim(x)[2]
        T_x <- dim(x)[3]
        n_y <- dim(parameters[["Wya"]])[1]
        n_a <- dim(parameters[["Wya"]])[2]
        
        caches = list(length(T_x))
        
        a <- array(0,c(n_a, m, T_x))
        y_pred = array(0, c(n_y, m, T_x))
        #a_next = a[ , , 1] + a0
        a_next = a0
        
        for (i in 1:T_x){
                rnn.output <- rnn_cell_forward(x[ , ,i], a_next, parameters)
                a_next  <- rnn.output$a_next
                yt_pred <-  rnn.output$yt_pred
                cache   <-  rnn.output$cache
                a[ , ,i] = a_next
                y_pred[ , ,i] = yt_pred
                caches[[i]]=cache
                
        }
        
        Caches = list("caches" = caches, "x" =  x)
        list("a" = a, "y_pred" = y_pred, "Caches" = Caches)
        
}

