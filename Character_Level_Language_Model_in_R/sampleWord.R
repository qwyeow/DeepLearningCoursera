sampleWord <- function(parameters, char_to_ix){
        
        source("softmax.R")                                
                
        Waa = parameters[['Waa']] 
        Wax = parameters[['Wax']] 
        Wya = parameters[['Wya']] 
        by = parameters[['by']] 
        b = parameters[['b']]
        
        vocab_size = dim(by)[1]
        n_a = dim(Waa)[2]
        
        x = array(0, c(vocab_size,1))
        a_prev = array(0, c(n_a,1))
        
        
        indices = list()
        idx = -1
        counter = 0
        newline_character = char_to_ix['\n']$ix
        indicesIndex <- 1
        
        while(idx!= newline_character & (counter != 50)) {
                                                Wax.x = Wax%*%x
                                                Waa.a_prev = Waa%*%a_prev
                                                a = tanh(Wax.x + Waa.a_prev + matrix(b, nrow = dim(Wax.x )[1], dim(Wax.x)[2]))
                                                Wya.a = Wya%*%a
                                                z = Wya.a + matrix(by, nrow = dim(Wya.a)[1], dim(Wya.a)[2])
                                                y = softmax(z)
                
                                                idx = sample(1:vocab_size,1,prob = y)  
                                                indices[[indicesIndex]] = idx
                                                indicesIndex = indicesIndex + 1
                                                counter = counter + 1
                
                                                x = array(0, c(vocab_size,1))
                                                x[idx] = 1
                                                a_prev = a
                                                }
        
        if (counter == 50){indices[[indicesIndex]] <- newline_character }
        
        indices
}