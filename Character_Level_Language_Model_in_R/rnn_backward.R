rnn_backward <- function(X, Y, parameters, cache) {
        
        gradients = list()
        
        y_hat = cache$y_hat
        a = cache$a
        x = cache$x
        
        Waa = parameters[['Waa']]
        Wax = parameters[['Wax']]
        Wya = parameters[['Wya']]
        by = parameters[['by']]
        b = parameters[['b']]
        
        
        gradients[['dWax']] = array(0, c(dim(Wax)[1],dim(Wax)[2]))
        gradients[['dWaa']] = array(0, c(dim(Waa)[1],dim(Waa)[2]))
        gradients[['dWya']] = array(0, c(dim(Wya)[1],dim(Wya)[2])) 
        gradients[['db']] = array(0, c(dim(b)[1],dim(b)[2])) 
        gradients[['dby']] = array(0, c(dim(by)[1],dim(by)[2])) 
        gradients[['da_next']] = array(0, c(dim(a[[1]])[1],dim(a[[1]])[2])) # python uses a[0]
        
        
        for (t in (rev(1:length(X)))){
                                     dy = y_hat[[t]]
                                     dy[Y[t]] = dy[Y[t]] - 1
                                     gradients = rnn_step_backward(dy, gradients, parameters, x[[t]], a[[t+1]], a[[t]]) 
                                                                               # Note! in python this is a[t] and a[t-1]
                                     }
        
        list("gradients" = gradients, "a" = a)
}
