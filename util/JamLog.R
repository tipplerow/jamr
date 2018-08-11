
JamLog.LEVELS <-
    list(SILENT  = 0,
         ERROR   = 1,
         WARNING = 2,
         INFO    = 3,
         DEBUG   = 4)

JamLog.DEFAULT_LEVEL <- "INFO"

## ---------------------------------------------------------------------

jam.assert <- function(cond, fmt, ...) {
    if (!isTRUE(cond)) {
        if (missing(fmt))
            msg <- "ASSERTION ERROR"
        else
            msg <- sprintf(fmt, ...)

        .jamLogWrite("ERROR", as.character(sys.call(-1))[1], msg)
        stop(call. = FALSE)
    }
}

## ---------------------------------------------------------------------

JamLog.getLevel <- function() {
    if (exists("Global.JamLog.levelCode"))
        Global.JamLog.levelCode
    else
        JamLog.DEFAULT_LEVEL
}

JamLog.isLevel <- function(levelCode) {
    levelCode %in% names(JamLog.LEVELS)
}

JamLog.setLevel <- function(levelCode) {
    oldLevel <- JamLog.getLevel()

    if (JamLog.isLevel(levelCode))
        assign("Global.JamLog.levelCode", levelCode, envir = globalenv())
    else
        warning(sprintf("Unknown logging level [%s]!", levelCode))

    oldLevel
}

## ---------------------------------------------------------------------

JamLog.debug <- function(fmt, ...) {
    .jamLogWrite("DEBUG", as.character(sys.call(-1)[1]), fmt, ...)
}

JamLog.error <- function(fmt, ...) {
    .jamLogWrite("ERROR", as.character(sys.call(-1)[1]), fmt, ...)
    stop(call. = FALSE)
}

JamLog.info <- function(fmt, ...) {
    .jamLogWrite("INFO", as.character(sys.call(-1)[1]), fmt, ...)
}

JamLog.warning <- function(fmt, ...) {
    .jamLogWrite("WARNING", as.character(sys.call(-1)[1]), fmt, ...)
}

## ---------------------------------------------------------------------

.jamLogWrite <- function(levelCode, func, fmt, ...) {
    if (.jamLogShouldWrite(levelCode))
        cat(sprintf("%s [%s] (%s) %s\n", levelCode, func, Sys.time(), sprintf(fmt, ...)))
}

.jamLogShouldWrite <- function(levelCode) {
    .jamLogAsNumeric(levelCode) <= .jamLogAsNumeric(JamLog.getLevel())
}

.jamLogAsNumeric <- function(levelCode) {
    value <- JamLog.LEVELS[[levelCode]]

    if (is.null(value))
        value <- +Inf

    value
}
