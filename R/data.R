#' Miami-Dade County House Prices
#'
#' @description
#' The dataset contains information on 13,932 single-family homes sold in
#' Miami-Dade County in 2016.
#' Besides publicly available information, the dataset creator Steven C. Bourassa has
#' added distance variables, aviation noise as well as latitude and longitude.
#'
#' More information can be found open-access on <https://www.mdpi.com/1595920>.
#'
#' The dataset can also be downloaded via `miami <- OpenML::getOMLDataSet(43093)$data`.
#'
#' @format A data frame with 13,932 rows and 17 columns:
#' \describe{
#'   \item{PARCELNO}{unique identifier for each property. About 1% appear multiple times.}
#'   \item{SALE_PRC}{sale price ($)}
#'   \item{LND_SQFOOT}{land area (square feet)}
#'   \item{TOT_LVG_AREA}{floor area (square feet)}
#'   \item{SPEC_FEAT_VAL}{value of special features (e.g., swimming pools) ($)}
#'   \item{RAIL_DIST}{distance to the nearest rail line (an indicator of noise) (feet)}
#'   \item{OCEAN_DIST}{distance to the ocean (feet)}
#'   \item{WATER_DIST}{distance to the nearest body of water (feet)}
#'   \item{CNTR_DIST}{distance to the Miami central business district (feet)}
#'   \item{SUBCNTR_DI}{distance to the nearest subcenter (feet)}
#'   \item{HWY_DIST}{distance to the nearest highway (an indicator of noise) (feet)}
#'   \item{age}{age of the structure}
#'   \item{avno60plus}{dummy variable for airplane noise exceeding an acceptable level}
#'   \item{structure_quality}{quality of the structure}
#'   \item{month_sold}{sale month in 2016 (1 = jan)}
#'   \item{LATITUDE, LONGITUDE}{Coordinates}
#' }
"miami"
