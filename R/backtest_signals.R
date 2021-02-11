#' Basic fast backtester 
#'
#' @param X xts
#' @param signals_column character
#' @param simple_result logical
#' @param starting_capital integer
#' @param starting_stock integer
#' @param comission double
#'
#' @return double
#' @export
#'
#' @examples
#' signals = na.omit(make_macd_signals(AFLT2014, nFast = 50, nSlow = 200, nSig = 9))
#' data = na.omit(cbind(AFLT2014, signals))
#' backtest_signals(data)
backtest_signals = function(X, signals_column = "signal_inter", simple_result = FALSE,
                            starting_capital = 1000, starting_stock = 0, comission = 0.0007) {
    D = X[,c('Close', signals_column)]
    l = length(D$Close)
    
    # Using deltas, or capital changes due to buy/sell signals' execution
    D$delta = - D$Close*D[,signals_column]
    D$comis = abs(D$delta)*comission
    
    # TODO amount of deals
    
    
    # Result is the sum of deltas minus comissions minus position at the end of period
    # TODO indicate whether capital reaches negative value
    result = as.numeric(
        sum(D$delta) - sum(D$comis) + D$Close[l] * sum(D[,signals_column])
    )
    if (simple_result)
        return(result)
    else
        return(data.frame(result = result, total_comissions = sum(D$comis)))
}