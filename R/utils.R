##'
##' @export

top_n_margin <- function(data,
                         n = 2) {
    data <- data %>% arrange(desc(pot_prof_margin)) %>%
        select(pot_prof_margin) %>% .[1:n, 1] %>% mean()
    return(data)
}

top_n_neg_spread <- function(data,
                             n = 2) {
    data <- data %>% arrange(desc(best_difference)) %>%
        select(best_difference) %>% .[1:n, 1] %>% mean()
    return(data)
}

top_n_profit <- function(data,
                         n = 2) {
    data <- data %>% arrange(desc(pot_prof_aft_fee)) %>%
        select(pot_prof_aft_fee) %>% .[1:n, 1] %>% mean()
    return(data)
}

## Function to filter potential results
filter_trades <- function(data,
                          eth_required_limits) {
    data = data %>% filter(eth_required >= eth_required_limits[1] && eth_required <= eth_required_limits[2])
}
