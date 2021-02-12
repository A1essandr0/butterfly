#' Factory for fixing dataset and backtesting strategy with different parameters.
#' Useful for parameter optimisation
#'
#' @param X xts
#' @param signal_maker function
#' @param backtester function
#' @param simplistic logical
#'
#' @return function
#' @export
#'
#' @examples
backtest_parameters = function(X, signal_maker = make_macd_signals,
                               backtester = backtest_signals,
                               simplistic = TRUE) {
    # Factory returns function, which takes only strategy parameters and returns profits
    return(
        function(maker_parameters, tester_parameters) {
            maker_params = maker_parameters # Parameters go as an argument
            maker_params$X = X # Dataset is fixed at the moment of function creation
            maker_params$simple_result = simplistic
            signals = stats::na.omit(do.call(signal_maker, maker_params))
            
            tester_params = tester_parameters
            tester_params$simple_result = simplistic
            tester_params$X = stats::na.omit(cbind(maker_params$X, signals))
            do.call(backtester, tester_params)
        })
}
