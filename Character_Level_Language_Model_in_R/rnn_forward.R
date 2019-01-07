rnn_forward <- function(X, Y, a0, parameters, vocab_size = 27) {
        
        x = list()
        a = list()
        y_hat = list()
        a[[1]] =  a0    # in python this is a[-1] = np.copy(a0)
        loss = 0
        
        for (t in 1:length(X)){
                
                              x[[t]] = array(0, c(vocab_size,1))
                              if (is.na(X[t]) != TRUE){ x[[t]][X[t]] = 1}
                              rnn_step_forward.output = rnn_step_forward(parameters, a[[t]], x[[t]])
                              a[[t+1]] = rnn_step_forward.output$a_next
                              y_hat[[t]] = rnn_step_forward.output$p_t
                
                              loss = loss - log(y_hat[[t]][Y[t],1])
                              }
        
        cache = list("y_hat" = y_hat, "a" =a, "x" = x)
        
        list("loss" = loss, "cache" =  cache)
        
}



