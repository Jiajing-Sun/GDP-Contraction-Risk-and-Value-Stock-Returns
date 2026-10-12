qkey <-
function (q) 
{
    as.integer(substr(q, 1, 4)) * 4L + as.integer(substr(q, 6, 
        6))
}
quarter_string <-
function (year, quarter) 
{
    paste0(as.integer(year), "Q", as.integer(quarter))
}
pre_2009 <-
structure(c(6, 5, 4, 3, 2, 1, 0, -1, -2, -Inf, Inf, 6, 5, 4, 
3, 2, 1, 0, -1, -2), dim = c(10L, 2L))
era_2009 <-
structure(c(6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -Inf, Inf, 6, 5, 
4, 3, 2, 1, 0, -1, -2, -3), dim = c(11L, 2L))
era_2020 <-
structure(c(16, 10, 7, 4, 2.5, 1.5, 0, -3, -6, -12, -Inf, Inf, 
16, 10, 7, 4, 2.5, 1.5, 0, -3, -6, -12), dim = c(11L, 2L))
era_2024 <-
structure(c(9, 7, 5.5, 4, 2.5, 1.5, 0, -1.5, -3, -5.0999999999999996, 
-Inf, Inf, 9, 7, 5.5, 4, 2.5, 1.5, 0, -1.5, -3, -5.0999999999999996
), dim = c(11L, 2L))
layout_for <-
function (q) 
{
    k <- qkey(q)
    if (k < qkey("1992Q1")) 
        stop("The real-GDP sample begins in 1992Q1.")
    if (k < qkey("2009Q2")) {
        return(list(cols = paste0("PRGDP", 11:20), bounds = pre_2009, 
            regime = "1992Q1-2009Q1"))
    }
    if (k < qkey("2020Q2")) {
        return(list(cols = paste0("PRGDP", 12:22), bounds = era_2009, 
            regime = "2009Q2-2020Q1"))
    }
    if (k < qkey("2024Q2")) {
        return(list(cols = paste0("PRGDP", 12:22), bounds = era_2020, 
            regime = "2020Q2-2024Q1"))
    }
    list(cols = paste0("PRGDP", 12:22), bounds = era_2024, regime = "2024Q2-present")
}
representatives <-
function (bounds, open_fraction = 0.5) 
{
    out <- numeric(nrow(bounds))
    for (i in seq_len(nrow(bounds))) {
        lo <- bounds[i, 1]
        hi <- bounds[i, 2]
        if (is.infinite(hi)) {
            width <- bounds[i + 1, 2] - bounds[i + 1, 1]
            out[i] <- lo + open_fraction * width
        }
        else if (is.infinite(lo)) {
            width <- bounds[i - 1, 2] - bounds[i - 1, 1]
            out[i] <- hi - open_fraction * width
        }
        else {
            out[i] <- (lo + hi)/2
        }
    }
    out
}
dependent_wild_multipliers <-
function (n, draws, bandwidth) 
{
    innovations <- matrix(rnorm((n + bandwidth - 1L) * draws), 
        nrow = n + bandwidth - 1L, ncol = draws)
    cumulative <- apply(innovations, 2, cumsum)
    if (draws == 1L) 
        cumulative <- matrix(cumulative, ncol = 1L)
    cumulative <- rbind(0, cumulative)
    (cumulative[(bandwidth + 1L):(bandwidth + n), , drop = FALSE] - 
        cumulative[seq_len(n), , drop = FALSE])/sqrt(bandwidth)
}
