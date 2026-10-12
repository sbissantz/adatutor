# lazy data used as a default (tutplot_weightone), and the `x` that curve()
# creates for its expression (tutplot_updatefactor)
utils::globalVariables(c("altmejd_splits", "x"))

.onLoad <- function(libname, pkgname) {}

.onAttach <- function(libname, pkg) {
  intro <- "adatutor: Introducing the Boosting Framework with AdaBoost"
  version <- utils::packageVersion(pkg)
  packageStartupMessage(paste0(intro, "\n", "Version: ", version))
}
