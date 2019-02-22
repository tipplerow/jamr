
JamEnv.getOptional <- function(name, default) {
    value <- Sys.getenv(name, unset = NA)

    if (is.na(value))
        value <- default

    value
}

JamEnv.getRequired <- function(name) {
    value <- Sys.getenv(name, unset = NA)

    if (is.na(value))
        JamLog.error("Environment variable %s is not set.", name)

    value

}
