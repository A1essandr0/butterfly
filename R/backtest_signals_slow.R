#' Slow backtester with loop. Use it for debug purposes
#'
#' @param X xts
#' @param signals_column character
#' @param verbose integer
#' @param simple_result logical
#' @param starting_capital integer
#' @param starting_stock integer
#' @param comission double
#'
#' @return double
#' @export
#'
#' @examples
backtest_signals_slow = function(X, signals_column = "signal_inter", verbose = 0, simple_result = FALSE,
                                 starting_capital = 1000, starting_stock = 0, comission = 0.0007) {
    l = length(X$Close)
    capital = starting_capital
    stock = starting_stock
    total_comissions = 0
    if (verbose) cat("Capital: ", capital, "comission: ", comission, "\n")
    
    # Using signals to trade
    # verbose = 1 or 2 outputs more debug info
    for (i in 1:l) {
        
        # Buy transaction
        if (X[i, signals_column] == 1)
        {
            stock = stock + 1
            capital = capital - as.numeric(X$Close[i]) - as.numeric(X$Close[i]*comission)
            total_comissions = total_comissions + as.numeric(X$Close[i]*comission)
            if (verbose>1)
                cat("\n", zoo::index(X)[i], "Bought 1 stock for ", as.numeric(X$Close[i]), "; capital = ", capital, "; stocks in portfolio = ", stock,"; total comission payed ", total_comissions)
        }
        
        # Sell transaction
        if (X[i, signals_column] == -1)
        {
            stock = stock - 1
            capital = capital + as.numeric(X$Close[i]) - as.numeric(X$Close[i]*comission)
            total_comissions = total_comissions + as.numeric(X$Close[i]*comission)
            if (verbose>1)
                cat("\n", zoo::index(X)[i], "Sold 1 stock for ", as.numeric(X$Close[i]), "; capital = ", capital, "; stocks in portfolio = ", stock, "; total comission payed ", total_comissions)
        }
        
    }
    
    resulting_capital = capital + as.numeric(X$Close[l]*stock - X$Close[l]*stock*comission)
    if (verbose) cat("\nResulting capital:", resulting_capital, "stocks:", stock, "comission:", comission, "total comission payed:", total_comissions, "\n")
    
    if (simple_result)
        return(resulting_capital - starting_capital)
    else
        return(data.frame(
            result = resulting_capital - starting_capital,
            total_comissions = total_comissions
        ))
}
