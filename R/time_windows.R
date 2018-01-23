##'
##' @export

time_windows = function(data,
                       this_market) {

    one_market = filter(data, market == this_market)
    one_market = one_market %>% arrange(binance_time) %>% group_by(binance_time) %>% top_n(1, pot_prof_aft_fee) %>% ungroup()

    values = one_market$pot_prof_aft_fee
    times = one_market$binance_time
    l = list()
    indices = which(values > 0)
    if(length(indices) == 0) {
        stop("There are no profit opportunities")
    }
    if(length(indices) == 1){
        l[[1]] = 1
    } else {
        this_length = 1

        for(i in 1:(length(indices)-1)) {
            if(indices[i+1] == indices[i] + 1) {
                this_length = this_length + 1
                if(i == length(indices)-1) {
                    l[[i]] = this_length
                }
            } else {
                l[[i]] = this_length
                this_length = 1
            }

        }
    }

    ## Time difference
    diffs = numeric()
    times = one_market$binance_time
    for(i in 1:length(times)-1) {
        this_diff = difftime(times[i+1], times[i], units = "mins")
        this_diff = as.numeric(this_diff)
        diffs = c(diffs, this_diff)
    }


    #######
    ## Find top element of list
    l_num <- as.numeric(as.character(l))
    three <- sort(l_num)

    ## bottom three numbers
    bottom <- three[1]
    top <- three[length(three)]

    ## match bottom three to indices
    bottom_ <- which(l_num == bottom) #%>% .[1:3] - bottom_three
    bottom_index <- bottom_[1]
    top_ <- which(l_num == top) #%>% .[1:3] - top_three
    top_index <- top_[1] - (top - 1)

    ## profit values
    profit_values <- one_market$pot_prof_aft_fee

    a = indices[(top_index + 1):(top_index + 1 + top)]
    b = indices[bottom_index]

    ## want time period of 30
    num <- round((30 - length(a)) / 2)

    c = a[1] - num
    d = a[length(a)] + num

    e = b[1] - 15
    f = b[1] + 15

    bottoms <- data.frame(pot_prof_aft_fee = profit_values[e:f],
                          binance_time = times[e:f])
    tops <- data.frame(pot_prof_aft_fee = profit_values[c:d],
                       binance_time = times[c:d])

    plot1 = ggplot(data = tops, aes(x = binance_time, y = pot_prof_aft_fee)) + geom_line() +
        labs(title = paste("Longest Streak for", this_market, sep = " "),
             x = "Time", y = "Potential Profit After Fees ($)")

    plot2 = ggplot(data = bottoms, aes(x = binance_time, y = pot_prof_aft_fee)) + geom_line() +
        labs(title = paste("Shortest Streak for", this_market, sep = " "),
             x = "Time", y = "Potential Profit After Fees ($)")


    l = Filter(Negate(is.null), l) %>% unlist(.)
    time_values = l * mean(diffs)


    df = data.frame(times = time_values)
    p1 = ggplot(data = df, aes(x = times, fill = "#F8766D")) + geom_histogram(bins = 10) +
        labs(title = paste("Profit after Fees Opportunity \nTimeframes for", this_market, sep = " "),
             x = "Minutes", y = "Count") + guides(fill=FALSE)

    return(list("values" = time_values, "plot" = p1, "p1" = plot1, "p2" = plot2))
}
