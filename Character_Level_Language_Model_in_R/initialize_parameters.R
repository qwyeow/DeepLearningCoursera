initialize_parameters <- function(n_a, n_x, n_y) {
        Wax = (matrix(rnorm(n_a*n_x,0,1), nrow = n_a, ncol = n_x))*0.01
        Waa = (matrix(rnorm(n_a*n_a,0,1), nrow = n_a, ncol = n_a))*0.01
        Wya = (matrix(rnorm(n_y*n_a,0,1), nrow = n_y, ncol = n_a))*0.01
        b = array(0, c(n_a,1))
        by = array(0, c(n_y,1))
        parameters = list("Wax" = Wax, "Waa"= Waa, "Wya"= Wya, "b"= b,"by"= by)
        
        parameters
}
