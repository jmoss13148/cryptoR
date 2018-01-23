##'
##' @export

graph_profit <- function(data = full_data,
                         this_market = "NEOETH",
                         function_type = top_n_profit){

    one_market <- filter(full_data, market == this_market)

    graph_data = by(data = one_market, INDICES = one_market[,"binance_time"], FUN = top_n_profit) %>%
        cbind(.) %>% as.data.frame(.) %>% cbind(row.names(.), .)
    names(graph_data) = c("binance_time", "metric")
    graph_data$binance_time <- graph_data$binance_time %>% as.character(.) %>% ymd_hms(.)

    ## Create green and red
    rect_data <- data.frame(xmin = min(graph_data$binance_time),
                            xmax = max(graph_data$binance_time),
                            ymin = c(ifelse(min(graph_data$metric) < 0, min(graph_data$metric), 0), 0),
                            ymax = c(0, ifelse(max(graph_data$metric) > 0, max(graph_data$metric), 0)),
                            col = c("red","green"))

    plot <- ggplot(data = graph_data, aes(x = binance_time, y = metric)) +
        geom_line(color = "black") +
        labs(title = paste("Top Two Trades Profit after Fees for", this_market, sep = " "), x = "Time", y = "Profit after Fees ($)") +
        geom_rect(data = rect_data, inherit.aes=FALSE, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=col), alpha=0.4) +
        scale_fill_identity()
    return(plot)
}

graph_margin <- function(data = full_data,
                         this_market = "NEOETH",
                         function_type = top_n_margin){

    one_market <- filter(full_data, market == this_market)

    graph_data = by(data = one_market, INDICES = one_market[,"binance_time"], FUN = top_n_margin) %>%
        cbind(.) %>% as.data.frame(.) %>% cbind(row.names(.), .)
    names(graph_data) = c("binance_time", "metric")
    graph_data$binance_time <- graph_data$binance_time %>% as.character(.) %>% ymd_hms(.)

    ## Create green and red
    rect_data <- data.frame(xmin = min(graph_data$binance_time),
                            xmax = max(graph_data$binance_time),
                            ymin = c(ifelse(min(graph_data$metric) < 0, min(graph_data$metric), 0), 0),
                            ymax = c(0, ifelse(max(graph_data$metric) > 0, max(graph_data$metric), 0)),
                            col = c("red","green"))

    plot <- ggplot(data = graph_data, aes(x = binance_time, y = metric)) +
        geom_line(color = "black") +
        labs(title = paste("Top Two Trades Profit Margin for", this_market, sep = " "), x = "Time", y = "Profit Margin after Fees") +
        geom_rect(data = rect_data, inherit.aes=FALSE, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=col), alpha=0.4) +
        scale_fill_identity()
    return(plot)
}

graph_neg_spread <- function(data = full_data,
                         this_market = "NEOETH",
                         function_type = top_n_margin){

    one_market <- filter(full_data, market == this_market)

    graph_data = by(data = one_market, INDICES = one_market[,"binance_time"], FUN = top_n_neg_spread) %>%
        cbind(.) %>% as.data.frame(.) %>% cbind(row.names(.), .)
    names(graph_data) = c("binance_time", "metric")
    graph_data$binance_time <- graph_data$binance_time %>% as.character(.) %>% ymd_hms(.)

    ## Create green and red
    rect_data <- data.frame(xmin = min(graph_data$binance_time),
                            xmax = max(graph_data$binance_time),
                            ymin = c(ifelse(min(graph_data$metric) < 0, min(graph_data$metric), 0), 0),
                            ymax = c(0, ifelse(max(graph_data$metric) > 0, max(graph_data$metric), 0)),
                            col = c("red","green"))

    plot <- ggplot(data = graph_data, aes(x = binance_time, y = metric)) +
        geom_line(color = "black") +
        labs(title = paste("Top Two Trades Negative Spread for", this_market, sep = " "), x = "Time", y = "Negative Spread") +
        geom_rect(data = rect_data, inherit.aes=FALSE, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=col), alpha=0.4) +
        scale_fill_identity()
    return(plot)
}




