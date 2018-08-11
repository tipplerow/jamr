
Filter.replaceNA <- function(x, default) {
    x[is.na(x)] <- default
    x
}
