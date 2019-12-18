#' @name rhineHYBASlev08
#' @docType data
#' @title
#'  HydroBASINS Level 08 Data of the Rhine Catchment
#'
#' @description
#' This spatial dataset is a subset of the data layer
#' \code{hybas_eu_lev08_v1c} from the HydroBASINS v1.c database
#' (Lehner and Grill, 2013).
#' The original data format was SHP (ESRI Shape File).
#' The subset consists of subbasins within the Rhine basin, i.e.
#' \code{MAIN_BAS = 2080023010}.
#'
#' @format
#'  An Object of class SpatialPolygonsDataFrame with 289 features.
#'  The attribute table comprises 13 fields:
#'  \describe{
#'    \item{HYBAS_ID}{factor, unique basin identifier}
#'    \item{NEXT_DOWN}{factor, Hybas_id of the next downstream polygon}
#'    \item{NEXT_SINK}{factor, Hybas_id of the next downstream sink}
#'    \item{MAIN_BAS}{factor, Hybas_id of the most downstream sink, i.e. the outlet of the main river basin}
#'    \item{DIST_SINK}{numeric, Distance from polygon outlet to the next
#'    downstream sink along the river network, in kilometers}
#'    \item{DIST_MAIN}{numeric, Distance from polygon outlet to
#'    the most downstream sink, i.e. the outlet of the
#'    main river basin along the river network, in kilometers}
#'    \item{SUB_AREA}{numeric, Area of the individual polygon (i.e. sub-basin), in square kilometers}
#'    \item{UP_AREA}{numeric, Total upstream area, in square kilometers,
#'    calculated from the headwaters to the polygon location (including the polygon)}
#'    \item{PFAF_IF}{int, the Pfafstetter code}
#'    \item{ENDO}{int, Indicator for endorheic (inland) basins without
#'    surface flow connection to the ocean: 0 = not part of an endorheic basin;
#'    1 = part of an endorheic basin; 2 = sink (i.e. most downstream polygon)
#'    of an endorheic basin.}
#'    \item{COAST}{int, indicator for lumped coastal basins: 0 = no; 1 = yes}
#'    \item{ORDER}{int, Indicator of river order (classical ordering system):
#'    order 1 represents the main stem river from sink to source;
#'    order 2 represents all tributaries that flow into a 1 st order
#'    river; order 3 represents all tributaries that flow into a 2 nd order river;
#'    etc.; order 0 is used for conglomerates of small coastal watersheds}
#'    \item{SORT}{int, Indicator showing the record number (sequence)
#'    in which the original polygons are stored in the shapefile
#'    (i.e. counting upwards from 1 in the original shapefile).}
#'  }
#'  The PROJ.4 string is
#'  \code{+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84+towgs84=0,0,0}
#'
#' @source
#' This product rhineHYBASlev08 incorporates data from the HydroSHEDS database which is (C) World
#' Wildlife Fund, Inc. (2006-2013) and has been used herein under license. WWF has not evaluated the data as altered and
#' incorporated within rhineHYBASlev08, and therefore gives no warranty regarding its accuracy,
#' completeness, currency or suitability for any particular purpose. Portions of the HydroSHEDS database incorporate data
#' which are the intellectual property rights of (C) USGS (2006-2008), NASA (2000-2005), ESRI (1992-1998), CIAT (2004-2006),
#' UNEP-WCMC (1993), WWF (2004), Commonwealth of Australia (2007), and Her Royal Majesty and the British Crown and
#' are used under license. The HydroSHEDS database and more information are available at http://www.hydrosheds.org.
#'
#' @references
#' B. Lehner and G. Grill, 2013, Global river hydrography and network
#' routing: baseline data and new approaches to study the world’s
#' large river systems, \emph{Hydrological Processes}, \bold{27},
#' 2171--2186, \doi{10.1002/hyp.9740}.
#'
#' @keywords datasets
#' @examples
#' \donttest{
#' ## Catchment of gauge Mainz / Rhein
#' ## HYBAS uses a modified version of Pfafstetter
#' op.old <- options()
#' options("hydrocode.alloddorzero" = TRUE)
#' ok <- is.downstream(23263070, rhineHYBASlev08$PFAF_ID)
#' MzBas <- subset(rhineHYBASlev08, subset = ok)
#' plot(rhineHYBASlev08)
#' plot(MzBas, add = TRUE, col = "red")
#'
#' ## get catchment size
#' ## AEO : 98206 km2 according to website
#' sum(MzBas$SUB_AREA)
#'
#' ## reset previous settings
#' options(op.old)
#' }
NULL
