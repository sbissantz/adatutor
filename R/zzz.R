.onLoad <- function(libname, pkgname) {}

.onAttach <- function(libname, pkg) {
  intro <- "adatutor: Introducing the Boosting Framework with AdaBoost"
  version <- utils::packageVersion(pkg)
  packageStartupMessage(paste0(intro, "\n", "Version: ", version))
}
