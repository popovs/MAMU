#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Radar site groupings
#'
#' A small data lookup containing radar survey sites that should be renamed
#' during the data cleaning process in [process_radar_data()].
#'
#' @name rs
#' @docType data
#' @keywords data
#' @format A dataframe with 15 rows and 2 variables:
#' \describe{
#'  \item{original_name}{Original site name as appears in the 'Name$' column in the raw ECCC-FLNR MAMU radar data.}
#'  \item{new_name}{New, cleaned site name. This cleaned name can then be used in later analyses to group radar sites that need to be consolidated.}
#' }
"rs"
