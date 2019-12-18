##  upstream.R part of R add-on package HydrCode
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
## 23263070 almost gauge Mainz Rhein for
## hybas_eu_lev08_v1c
## AEO : 98206 km2
## UP_AREA: 98873.8
#' @name is.upstream
#' @title Comparison of Pfafstetter Coded Basins
#' @description
#' One point (river segment or subbasin) with Pfafstetter code \bold{a}
#' on the water system can be queried for beeing upstream or downstream
#' as compared to one or many points \bold{b} on the water system.
#'
#' @param a numeric, Pfafstetter coded point
#' @param b numeric, one or more Pfaffstetter coded points
#'
#' @details
#' For the scientific background of the Pfafstetter coding system see
#' the literature reference.
#'
#' The following option(s) can be set via,
#' e.g. \code{options("hydrocode.X" = XX)}:
#'
#' \describe{
#'  \item{\code{hydrocode.parallel}:}{logical. Defaults to \code{FALSE}.
#'  The runtime may or may not decrease for very large vectors (length > 50000)
#'  on multi-threading processors.}
#'  \item{\code{hydrocode.alloddorzero}:}{logical. Defaults to \code{FALSE}.
#'  This refers to the original Pfafstetter coding system and tests, whether
#'  all \code{n} numbers are odd, \code{[13579]\{n\}}. Hydrocodes that contain
#'  the digit \code{0} are interpreted as closed catchments (endorheic basins).
#'  This setting works, e.g. with the Pfafstetter coded table \code{riversegments} of
#' \emph{CCM River and Catchment Database, v2.1} (European Commission - JRC, 2007;
#' Vogt et al., 2007).
#'  For decoding the Pfafstetter coded tables of the \emph{HydroBASIN, v1c} dataset
#'  (Lehner and Grill, 2013), this option should be set to
#'  \code{options("hydrocode.alloddorzero" = TRUE)}. This tests for all \code{n}
#'  numbers are odd or zero, \code{[013579]\{n\}}.}
#' }
#'
#' @return
#' a vector of type logical
#'
#' @examples
#' ## example from Wikipedia
#' ## 8835 is upstream of segments 8833 and 8811,
#' ## but not segments 8832, 8821 or 9135
#' dwn <- c(8833, 8811, 8832, 8821, 9135)
#' is.upstream(8835, dwn)
#'
#' ## works with operator
#' 8835 %up% dwn
#'
#' ## this will produce errors
#' \donttest{
#' is.upstream("12h", 123)
#' is.upstream(12.4, 123)
#' }
#'
#' ## example from Verdin and Verdin, 1999, p. 10
#' b <- c(8883, 8881, 8879, 8877, 8875, 8873, 8871, 8859,
#'        8857, 8855, 8853, 8851, 8839, 8837, 8835, 8833,
#'        8831, 8819, 8817, 8815, 8813, 8811, 8886, 8887,
#'        8888, 8889)
#' a <- 8885
#' a %down% b
#'
#' ## example from Vogt et al. (2007),
#' ## pollution source at 464.
#' ## Which subcatchments are
#' ## affected? Mixed levels of subdivisions.
#' a <- 464
#' b <- c(465, 466, 467, 47, 48, 49,
#'        463, 461, 41, 43, 45, 452,
#'        454)
#' a %up% b
#'
#' ## same hydro codes return TRUE per default
#' 112 %up% 112
#' 112 %down% 112
#'
#' @seealso \code{\link{options}}
#' @references
#' A. L. de Jager and J. V. Vogt, 2010,
#' Development and Demonstration of a
#' Structured Hydrological Feature Coding
#' System for Europe,
#' \emph{Hydrological Sciences Journal}, \bold{55},
#' 661--75, \doi{10.1080/02626667.2010.490786}.
#'
#' B. Lehner and G. Grill, 2013, Global river hydrography and network
#' routing: baseline data and new approaches to study the world’s
#' large river systems, \emph{Hydrological Processes}, \bold{27},
#' 2171--2186, \doi{10.1002/hyp.9740}.
#'
#' K. L. Verdin and J. P. Verdin, 1999,
#' A topological system for delineation and codification
#' of the Earth’s river basins,
#' \emph{Journal of Hydrology}, \bold{218}, 1--12,
#' \doi{10.1016/S0022-1694(99)00011-6}.
#'
#' J. Vogt, P. Soille, A. de Jager, E. Rimavičiūtė,
#' W. Mehl, S. Foisneau, K. Bódis, et al., 2007,
#' \emph{A Pan-European River and Catchment Database},
#' Publications Office: Luxembourg, \doi{10.2788/35907}.
#'
#' Wikipedia contributors, 2019,
#' Pfafstetter Coding System,
#' In \emph{Wikipedia, The Free Encyclopedia},
#' Retrieved 13:23, November 9, 2019,
#' \url{https://en.wikipedia.org/w/index.php?title=Pfafstetter_Coding_System&oldid=907987995}
#'
#' @keywords math logic
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel parSapply
#' @importFrom parallel detectCores
#' @export
is.upstream <- function(a, b) {
  ## check input
  pattern <- "^[[:digit:]]{1,}$"
  cond1 <- grepl(pattern, a)
  cond2 <- grepl(pattern, b)
  if (any(!cond1, any(!cond2))) {
    stop("a, b must only contain digits")
  }
  if (any(anyNA(a), anyNA(b))) {
    stop("a, b must not contain NA")
  }
  if (length(a) > 1) {
    stop("only one a can be compared with one or many b")
  }

  ## could be done via option?
  paral <- getOption("hydrocode.parallel")
  if (paral) {
    ## try parallel
    cores <- detectCores()
    cl <- makeCluster(getOption("cl.cores", cores))
    result <- parSapply(cl, b, function(x) {
      is.updwn(a, x, upstream = TRUE)
    })
    stopCluster(cl)

  } else {
    result <- sapply(b, function(x)
      is.updwn(a, x, upstream = TRUE))

  }
  return(result)

}

#' @rdname is.upstream
#' @export
`%up%` <- function(a, b)
  is.upstream(a, b)

#' @rdname is.upstream
#' @export
is.downstream <- function(a, b) {
  ## check input
  pattern <- "^[[:digit:]]{1,}$"
  cond1 <- grepl(pattern, a)
  cond2 <- grepl(pattern, b)
  if (any(!cond1, any(!cond2))) {
    stop("a, b must only contain digits")
  }

  if (any(anyNA(a), anyNA(b))) {
    stop("a, b must not contain NA")
  }
  if (length(a) > 1) {
    stop("only one a can be compared with one or many b")
  }

  paral <- getOption("hydrocode.parallel")
  if (paral) {
    ## try parallel
    cores <- detectCores()
    cl <- makeCluster(getOption("cl.cores", cores))
    result <- parSapply(cl, b, function(x) {
      is.updwn(a, x, upstream = FALSE)
    })
    stopCluster(cl)

  } else {
    result <- sapply(b, function(x)
      is.updwn(a, x, upstream = FALSE))
  }

  return(result)
}

#' @rdname is.upstream
#' @export
`%down%` <- function(a, b)
  is.downstream(a, b)

## internal function, not exported to the user
is.updwn <- function(a, b, upstream = TRUE) {
  ## need to be casted to character
  A <- as.character(a)
  B <- as.character(b)

  ## length of character vector
  nCharA <- nchar(A)
  nCharB <- nchar(B)

  ## nCharB may be larger then nCharA
  ## or vice versa
  if (nCharB > nCharA) {
    warning(
      paste0(
        "Higher level A is compared to lower level B.",
        " Truncate B to the same level as A."
      )
    )
    B <- substring(B, 1, nCharA)
    nCharB <- nCharA

  } else if (nCharA > nCharB) {
    warning(
      paste0(
        "Lower level A is compared to higher level B.",
        " Truncate A to the same level as B."
      )
    )
    A <- substring(A, 1, nCharB)
    nCharA <- nCharB
  }

  if (identical(A, B)) {
    ## return TRUE if both are equal
    return(TRUE)
  }

  ##  this shortens the number of tests
  if (upstream) {
    ## Greater
    cond02 <- as.numeric(A) > as.numeric(B)

    if (!cond02) {
      ## exit with FALSE
      return(FALSE)
    }

  } else {
    ## is less?
    cond02 <- as.numeric(A) < as.numeric(B)

    if (!cond02) {
      ## exit with FALSE
      return(FALSE)
    }
  }

  ## Verdin & Verdin 1999, p. 10
  ## basin part of same upper division basin?
  ## get the number of n digits?
  for (i in 1:nCharA) {
    leadA <- substring(A, 1, i)
    basinOK <- startsWith(B, leadA)
    if (!basinOK) {
      ##
      break
    }
  }

  ## get the trailing digits
  trailA <- substring(A, first = i)
  trailB <- substring(B, first = i)

  ## all odd?
  cond01 <- ifelse(upstream,
                   all.odd(trailB),
                   all.odd(trailA))
  return(cond01)

}
