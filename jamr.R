
jamr.load <- function(rootDir, verbose = TRUE) {
  require(MASS)
  
  if (missing(rootDir))
    rootDir <- jamr.resolveRoot()
  
  jamr.loadDir(file.path(rootDir, "math"), recursive = TRUE, verbose = verbose)
  jamr.loadDir(file.path(rootDir, "plot"), recursive = TRUE, verbose = verbose)
  jamr.loadDir(file.path(rootDir, "util"), recursive = TRUE, verbose = verbose)

  options(stringsAsFactors = FALSE)
  options(width = 120)
}

jamr.loadDir <- function(dirName, recursive = TRUE, verbose = TRUE) {
  fileNames <- dir(dirName, pattern = "\\.[rR]$", full.name = TRUE, recursive = recursive)

  for (fileName in fileNames)
    jamr.loadFile(fileName, verbose = verbose)
}

jamr.loadFile <- function(fileName, verbose = TRUE) {
  if (verbose)
    cat(sprintf("Loading [%s] %s\n", gsub("-", "", as.character(Sys.time())), fileName))
  
  source(fileName)
}

jamr.resolveRoot <- function() {
  file.path(Sys.getenv("JAMR_HOME"), file.path(Sys.getenv("HOME"), "GitHub", "jamr"))
}

