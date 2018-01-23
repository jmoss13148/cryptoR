##'
##' @export

possibilities <- function(full_data) {
    ## Let's find the number of alts where a theoretical arbitrage condition existed
    alts = unique(full_data$market)
    arb_alts <- filter(full_data, best_difference > 0) %>%
        select(market) %>% .[, 1] %>% unique() %>% as.character()
    arb_ratio = length(arb_alts) / length(alts)

    ## Find number where profit after fees
    prof_alts <- filter(full_data, pot_prof_aft_fee > 0) %>%
        select(market) %>% .[, 1] %>% unique() %>% as.character()
    prof_ratio <- length(prof_alts) / length(alts)

    data = data.frame(type = c("Arbitrage", "Profit after Fees"),
                      value = c(length(arb_alts), length(prof_alts)))
    p1 = ggplot(data = data, aes(x = type, y = value, fill = type)) + geom_bar(stat = "identity") +
        labs(title = "Potential for Arbitrage", x = "Metric", y = "# of currencies") + guides(fill=FALSE)

    return(list("profit_ratio" = prof_ratio,
                "arb_ratio" = arb_ratio,
                "profit" = length(prof_alts),
                "arb" = length(arb_alts),
                "plot" = p1))
}
