
JamIO.load <- function(fileName, verbose = TRUE) {
    if (!file.exists(fileName))
        JamLog.stop("File does not exist [%s].", fileName)

    if (verbose)
        JamLog.info("Loading [%s]...", fileName)

    get(load(fileName))
}

## ---------------------------------------------------------------------

JamIO.save <- function(object, fileName, verbose = TRUE) {
    save(object, file = fileName)

    if (verbose)
        JamLog.info("Wrote [%s].", fileName)
}
