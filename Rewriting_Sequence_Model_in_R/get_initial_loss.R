get_initial_loss <- function(vocab_size, seq_length) {
        -log(1/vocab_size)*seq_length
}