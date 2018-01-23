##'
##' @export

clean_data <- function(data) {
    full_data <- data %>% mutate(binance_difference = binance_bids_price - kucoin_asks_price,
                                 binance_arb        = ifelse(binance_difference > 0, TRUE, FALSE),
                                 max_trade_qt_bi   = pmin(binance_bids_quantity, kucoin_asks_quantity),
                                 kucoin_difference = kucoin_bids_price - binance_asks_price,
                                 kucoin_arb        = ifelse(kucoin_difference > 0, TRUE, FALSE),
                                 max_trade_qt_ku   = pmin(kucoin_bids_quantity, binance_asks_quantity),
                                 not_logical        = ifelse(binance_arb & kucoin_arb, TRUE, FALSE), ## both exchanges have negative spread
                                 best_difference    = pmax(binance_difference, kucoin_difference),
                                 pot_profit_eth     = ifelse(binance_difference > kucoin_difference, binance_difference * max_trade_qt_bi, kucoin_difference * max_trade_qt_ku),
                                 pot_profit_dollars = pot_profit_eth * 1000,
                                 eth_required       = ifelse(binance_difference > kucoin_difference, max_trade_qt_bi * binance_bids_price, max_trade_qt_ku * binance_bids_price),
                                 est_fee            = eth_required * 1000 * 0.002 + 20,
                                 pot_prof_aft_fee   = pot_profit_dollars - est_fee,
                                 pot_prof_margin    = pot_prof_aft_fee / (eth_required * 1000),
                                 binance_time       = ymd_hms(as.character(binance_time)),
                                 kucoin_time       = ymd_hms(as.character(kucoin_time))

    ) %>% arrange(desc(pot_prof_aft_fee))
    return(full_data)
}
