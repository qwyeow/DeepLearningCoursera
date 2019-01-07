update_parameters <- function(parameters, gradients, lr) {
                                                        parameters[['Wax']] = parameters[['Wax']] -lr*gradients[['dWax']]
                                                        parameters[['Waa']] = parameters[['Waa']] -lr*gradients[['dWaa']]
                                                        parameters[['Wya']] = parameters[['Wya']] -lr*gradients[['dWya']]
                                                        parameters[['b']] = parameters[['b']] -lr*gradients[['db']]
                                                        parameters[['by']] = parameters[['by']] -lr*gradients[['dby']]
        
                                                        parameters
                                                        }