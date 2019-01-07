model <- function(examples, char_to_ix, num_iterations = 35000, n_a = 50, dino_names = 7, vocab_size = 27) {
        
                n_x = vocab_size
                n_y = vocab_size

                #initialize_parameters
                source("initialize_parameters.R")
                parameters = initialize_parameters(n_a, n_x, n_y)
        
        
                #get_initial_loss
                source("get_initial_loss.R")
                loss = get_initial_loss(vocab_size, dino_names)
        
                #shuffle data
                randomized = sample(1:length(examples),length(examples))
                examples = examples[randomized]
        
                a_prev = array(0, c(n_a,1))
        
        
                source("sampleWord.R")
                library(ramify)
                for (j in 1:num_iterations){
                
                                        index = (j %% length(examples)) + 1
                                        examples[index]
                                        library(tokenizers)
                                        example_index = unlist(tokenize_characters(examples[index]))
                
                                        X = list()
                                        for (ch in 1:length(example_index)){X[[ch]] = char_to_ix[example_index[ch]]$ix}
                
                
                                        X = as.vector(unlist(c(NA, X)))
                                        names(X) <- NULL
                                        Y = as.vector(c(X[2:length(X)], 1)) # \n coded as "1"  
                                        names(Y) <- NULL
                
                                        source("optimize.R")
                                        optimize.output = optimize(X, Y, a_prev, parameters, learning_rate = 0.01)
                
                                        curr_loss = optimize.output$loss 
                                        gradients = optimize.output$gradients
                                        #a_prev = unlist(optimize.output$a_Final[[1]]) 
                                        #decided did not update a_prev and use zero vector array(0, c(n_a,1)) 
                                        #as initial state instead
                                        parameters = optimize.output$parameters
                
                                        source("smooth.R")
                                        loss = smooth(loss, curr_loss)
                
                        
                
                                        if ((j %% 2000) == 0){ 
                                                        cat("Iteration:", j ,"Loss:" ,loss, "\n")
                                                        for (name in 1:dino_names){
                                                                        sampled = unlist(sampleWord(parameters, char_to_ix))
                                                                        for (k in 1:length(sampled)){
                                                                                                cat(char_to_ix[ix == sampled[k]]$char)
                                }
                                
                        }
                }
                
                
        }
parameters        
}
