
Filter.isInteger <- function(x) {
    floor(x) == ceiling(x)
}

Filter.quantile <- function(x, n, ties.method = "average") {
    jam.assert(length(n) == 1 && Filter.isInteger(n) && n >= 2L)

    result <- floor(Filter.rank(x, 1.0, 1.0 + n - 1.0E-12, ties.method))
    result
}

Filter.rank <- function(x, lo = 0.0, hi = 1.0, ties.method = "average") {
    k <- which(is.finite(x))

    result <- rep(NA, length(x))
    result[k] <- (rank(x[k], ties.method = ties.method) - 1) / (length(k) - 1)

    result <- lo + (hi - lo) * result
    result
}

Filter.replaceNA <- function(x, default) {
    x[is.na(x)] <- default
    x
}
