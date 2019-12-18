##
.onLoad <- function(libname, pkgname) {
  options("hydrocode.parallel" = FALSE)
  options("hydrocode.alloddorzero" = FALSE)
}

.onUnload <- function(libname, pkgname) {
  options("hydrocode.parallel" = NULL)
  options("hydrocode.alloddorzero" = NULL)
}
