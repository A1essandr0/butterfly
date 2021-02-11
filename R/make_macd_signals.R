#' Making MACD signals
#'
#' @param X xts
#' @param nFast integer 
#' @param nSlow integer
#' @param nSig integer
#' @param simple_result logical
#' @param volume_threshold double
#' @param macd_threshold double
#'
#' @return xts
#' @export
#'
#' @examples
#' signals = make_macd_signals(AFLT2014, nFast = 50, nSlow = 200, nSig = 9)
make_macd_signals = function(X, nFast=12, nSlow=26, nSig=9, simple_result=FALSE,
                             volume_threshold=0, macd_threshold=0) {

    D = TTR::MACD(quantmod::Cl(X), nFast = nFast, nSlow = nSlow, nSig = nSig)
    D$signal_lag = xts::lag.xts(D$signal, 1)
    
    # Accounting the fact that signal can be used on the next step only
    D$signal_inter = xts::lag.xts(
        sign(sign(D$signal) - sign(D$signal_lag)), 1
    )
    # TODO Account for macd_threshold and volume_threshold
    
    if (simple_result)
        return(D$signal_inter)
    else {
        D$ema_fast = TTR::EMA(quantmod::Cl(X), n = nFast)
        D$ema_slow = TTR::EMA(quantmod::Cl(X), n = nSlow)
        return(D[, c(5,6,1,2,3,4)])
    }
}