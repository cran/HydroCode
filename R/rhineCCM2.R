#' @name rhineCCM2
#' @docType data
#' @title
#'  CCM2 River and Catchment of the Rhine Catchment, Rivers only
#'
#' @description
#' This spatial dataset is a subset of the data layer \code{RIVERSEGMENTS}
#' of the tile \code{WGS84_W2003} from the CCM River and
#' Catchment Database v2.1 (Vogt et al. 2007). The original data format was
#' PGeo (ESRI Personal GeoDatabase).
#' The subset consists of rivers within the Rhine basin, i.e.
#' \code{WSO_ID = 291110}. In order to save disk space,
#' the fields of the attribute table were shortened.
#'
#' @format
#'  An Object of class SpatialLinesDataFrame with 30343 features.
#'  The attribute table comprises 2 fields:
#'  \describe{
#'    \item{WSO1_ID}{int, unique river segment identifier}
#'    \item{PFAFSTETTER}{num, Pfafstetter code according to CCM2}
#'  }
#'  The PROJ.4 string
#'  is \code{+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0}.
#' @source
#' CCM River and Catchment Database
#' (C) European Commission - JRC, 2007.
#'
#' @references
#' J. Vogt, P. Soille, A. de Jager, E. Rimavičiūtė,
#' W. Mehl, S. Foisneau, K. Bódis, et al., 2007,
#' \emph{A Pan-European River and Catchment Database},
#' Publications Office: Luxembourg, \doi{10.2788/35907}.
#'
#' @keywords datasets
#' @import sp
#' @examples
#' \donttest{
#' ## get the flowpath from a head catchment to
#' ## the sea
#' ## Be patient, this might take a while.
#' system.time(fps <- 973842223 %up% rhineCCM2$PFAFSTETTER)
#' flowpath <- subset(rhineCCM2, subset = fps)
#' sp::plot(flowpath)
#'
#' ## does parallel threats increase the speed
#' ## and provide the same results?
#' op.old <- options()
#' options("hydrocode.parallel" = TRUE)
#' system.time(fpp <- 973842223 %up% rhineCCM2$PFAFSTETTER)
#' all.equal(fps, fpp)
#'
#' ## reset previous settings
#' options(op.old)
#' }
NULL
