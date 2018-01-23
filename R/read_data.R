##'
##' @export

read_data <- function(path = "/Users/rkmoss/Desktop/crypto/Data/all_markets") {
    options(digits.secs = 3)
    filelist <- list.files(path = path)
    data = list()
    for(i in 1:length(filelist)) {
        data[[i]] = fread(input = paste(path, "/", filelist[i], sep = ""))
        print(i)
    }
    full_data = rbindlist(l = data)
    return(full_data)
}




