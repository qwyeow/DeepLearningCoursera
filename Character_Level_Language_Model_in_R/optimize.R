optimize <- function(X, Y, a_prev, parameters, learning_rate = 0.005) {
        source("rnn_forward.R")
        source("rnn_step_forward.R")
        source("softmax.R")
        rnn_forward.output = rnn_forward(X, Y, a_prev, parameters)
        loss = rnn_forward.output$loss
        cache = rnn_forward.output$cache
        
        source("rnn_backward.R")
        source("rnn_step_backward.R")
        rnn_backward.output = rnn_backward(X, Y, parameters, cache)  
        gradients = rnn_backward.output$gradients
        a = rnn_backward.output$a
        
        source("ClipGradient.R")
        gradients = ClipGradient(gradients, 5)
        
        source("update_parameters.R")
        parameters = update_parameters(parameters, gradients, learning_rate)
        
        list("loss" = loss, "gradients" = gradients, "a_Final" = a[(length(X)+1)], "parameters" = parameters )
        #Note! in python this is a[len(X)-1] as a[] starts from -1 to len(X) or len(a) = len(X)+ 1
        
}