#' Factory for fixing parameters and backtesting strategy on different periods
#'
#' @param signal_maker function
#' @param maker_parameters list
#' @param backtester function
#' @param tester_parameters list
#' @param simplistic logical
#'
#' @return function
#' @export
#'
#' @examples
backtest_period = function(signal_maker = make_macd_signals, maker_parameters,
                           backtester = backtest_signals, tester_parameters,
                           simplistic = TRUE) {
    # Factory returns function, which takes only dataset and returns profits
    return(
        function(X) {
            maker_params = maker_parameters # Parameters are fixed at the moment of function creation
            maker_params$X = X # Dataset goes as an argument to returned function
            maker_params$simple_result = simplistic
            signals = stats::na.omit(do.call(signal_maker, maker_params))
            
            tester_params = tester_parameters
            tester_params$simple_result = simplistic
            tester_params$X = stats::na.omit(cbind(maker_params$X, signals))
            do.call(backtester, tester_params)
        })
}
