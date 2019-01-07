rnn_step_backward <- function(dy, gradients, parameters, x, a, a_prev) {
        
        gradients[["dWya"]] = gradients[["dWya"]] + dy %*% t(a)
        gradients[["dby"]] = gradients[["dby"]] + dy
        da = t(parameters[["Wya"]]) %*% dy + gradients[["da_next"]]
        daraw = (1- a^2)*da
        gradients[["db"]] = gradients[["db"]]  + daraw
        gradients[["dWax"]] = gradients[["dWax"]] + daraw %*% t(x)
        gradients[["dWaa"]] = gradients[["dWaa"]] + daraw %*% t(a_prev)
        gradients[["da_next"]] = gradients[["da_next"]] +  t(parameters[["Waa"]]) %*% daraw
        
        gradients
}