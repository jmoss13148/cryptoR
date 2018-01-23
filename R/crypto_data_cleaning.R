library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(gridExtra)

## Workflow
data <- read_data()
full_data <- clean_data(full_data)

## Initial graphs
possible <- possibilities(full_data = full_data)
possible

markets = unique(full_data$market) %>% as.character()
for(i in c(3,5)) {
    temp_plot <- graph_profit(data = full_data,
                       this_market = markets[i])
    temp <- paste("p", i, sep = "")
    assign(temp, temp_plot)
}
grid.arrange(p3, p5, nrow = 1)

markets = unique(full_data$market) %>% as.character()
for(i in 1:4) {
    temp_plot <- graph_margin(data = full_data,
                              this_market = markets[i])
    temp <- paste("p", i, sep = "")
    assign(temp, temp_plot)
}
grid.arrange(p1, p2, p3, p4, nrow = 2)


## Find markets where there is profit
profit_data <- filter(full_data, pot_prof_aft_fee > 0)
new_markets <- as.character(unique(profit_data$market))

dat = list()
for(i in c(3,5)) {
    times = time_windows(data = full_data,
                         this_market = new_markets[i])

    dat[[i]] = times[["values"]]

    temp_plot <- times[["plot"]]
    temp <- paste("p", i, sep = "")
    assign(temp, temp_plot)
}
grid.arrange(p3, p5, nrow = 1)

#######
times1 = time_windows(data = full_data,
                      this_market = "VENETH")
times2 = time_windows(data = full_data,
                     this_market = "POEETH")

grid.arrange(times1[["p1"]], times1[["p2"]], times2[["p1"]], times2[["p2"]], nrow = 2)





## Function to find the top five alts in the data

## Make table - number of times profit after fees is positive
#dt <- data.table(full_data)
#test = dt[, sum(pot_prof_aft_fee > 0)]


#

#


## Show profit over time for top few markets

## Want to sum up profit that could be had at the same time period
# dat <- select(one_market, binance_time, pot_profit_dollars)
# DT <- data.table(dat, key = "binance_time")
# dt <- DT[, list(pot_profit_dollars = sum(pot_profit_dollars)), by = binance_time]

## if profit persists for more than a few seconds, we could actually do this


# ggplot(dt, aes(x = binance_time, y = pot_profit_dollars)) + geom_point() + labs(title = "Potential Profit ($) over Time", x = "Time", y = "Potential Profit ($)")


# ## Transaction times
# transaction_times = data.frame("BCD" = ?,
#                                "NEO" = 20,
#                                "NEBL" = 1 * 60 * 60
#                                "BCPT" = ?,
#                                "VEN" = 10 * 60,
#                                "LTC" = 2.5 * 60,
#                                "QSP" = ? eth,
#                                "POE" = ? eth,
#                                    )







