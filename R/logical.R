##  logical.R part of R add-on package HydrCode
##  Copyright (C) 2019  Thorsten Pohlert
##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
#' @name is.odd
#' @title Number Type
#' @description
#' \code{is.odd} tests
#' whether a number is odd. \code{is.even}
#' returns the opposite
#'
#' @param x numeric object to be tested
#'
#' @return
#' a vector of type logical
#' @keywords math
#' @note
#' if \code{x} contains \code{NA} the function returns \code{NA}
#' @examples
#' x <- seq(1, 9, 2)
#' is.odd(x)
#' y <- seq(2, 8, 2)
#' is.even(y)
#' # NA values
#' x[1] <- NA
#' is.odd(x)
#'
#' @export
is.odd <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  ifelse(x %% 2 == 0,
         FALSE,
         TRUE)
}

#' @rdname is.odd
#' @export
is.even <- function(x) {
  !is.odd(x)
}


## not to be exported
all.odd <- function(x) {
  ## Are trailing digits all odd?
  ## The regex is more efficient.
  ## Runtime is decreased between 12 to 50 %.
  allOddOrZero <- getOption("hydrocode.alloddorzero")
  pattern <- ifelse(allOddOrZero,
                    paste0("[013579]{", nchar(x), "}"),
                    paste0("[13579]{", nchar(x), "}"))
  #pattern <- paste0("[13579]{", nchar(x), "}")
  grepl(x, pattern = pattern)
}
