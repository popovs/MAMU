# Functions for processing and managing ECCC-FLNR MAMU radar data

#' Import and Clean ECCC-FLNR MAMU Radar Data
#'
#' @description This function will read in the Excel file from the provided path and
#' perform a standardized series of cleaning steps that always need to be performed
#' on ECCC-FLNR MAMU radar survey data prior to any analyses.
#'
#' @details The function first performs a few housekeeping tasks:
#' * Make R-friendly column names
#' * Create the `survey_date` column from the year, month, and day columns
#' * Re-calculate day-of-year (`doy`)
#' * Fix minor typos, e.g. "Skwakwa" -> "Skwawka"
#'
#' It then filters down the data with the following criteria:
#' * Include only "Complete" surveys
#' * Include only pre-dawn surveys (i.e., after 2 am and before 8 am)
#' * Exclude August surveys
#' * Exclude Alaska border region surveys
#'
#' Finally, it creates the `new_name` column, which groups sites with slightly
#' different names that should really be a single site. The user can either rely
#' on the default radar site name changes that are bundled in this package (see `?rs`),
#' or they can specify their own as a dataframe in the `site_groupings` param.
#'
#' # Site groupings
#' Site groupings must be provided as a dataframe with two columns: `original_name`
#' and `new_name`, where the `original_name` column contains site names found in the
#' raw data and the `new_name` column contains name you want the site to be renamed to.
#' Run `data(rs)` for an example of the default name changes bundled with this package.
#'
#' @param path File path to radar data Excel file.
#' @param site_groupings (optional) Dataframe of site name groups with two columns: 1) `original_name` and 2) `new_name`.
#'
#' @return A dataframe of cleaned radar data.
#' @export
#'
#' @examples
#' # Clean using function defaults
#' process_radar_data("~/Documents/path/to/radar/data.xlsx")
#'
#' # Provide your own site groupings
#' sg <- data.frame(original_name = c("Fairfax 2", "Coleman Boat", "Nekite 1", "Nekite 2"),
#'                  new_name = c("Fairfax", "Coleman", "Nekite", "Nekite"))
#' process_radar_data("~/Documents/path/to/radar/data.xlsx",
#'                    site_groupings = sg)
#'
process_radar_data <- function(path,
                               site_groupings) {
  if (!missing(site_groupings)) {
    stopifnot("site_groupings must be a dataframe." = inherits(site_groupings, "data.frame"))
    stopifnot("site_groupings must contain the following two columns: 1. `original_name` and `new_name`. See `?process_radar_data()` for details." = all(names(rs) %in% c("original_name", "new_name")))
  }

  rd <- readxl::read_excel(path) # read in

  # Basic dataframe tidying
  rd <- janitor::clean_names(rd)
  rd$survey_date <- lubridate::make_date(year = rd$year, month = rd$month, day = rd$day)
  lubridate::date(rd$sunrise) <- rd$survey_date
  rd$doy <- lubridate::yday(rd$survey_date)
  rd$name <- stringr::str_trim(rd$name)
  rd$name <- stringr::str_squish(rd$name)

  # Dataset-specific fixes
  # Use only complete surveys
  rd <- rd[which(rd$status == "Complete"),]
  # Use only pre-dawn surveys
  rd <- rd[which(rd$start_hr < 8), ]
  # Exclude August surveys
  rd <- rd[which(rd$month != 8), ]
  # Exclude Alaska border region
  rd <- rd[which(rd$region != "AS"), ]
  # Fix one region typo
  rd[["region"]][rd$obs_id == "CC98010061"] <- "CC"
  # Fix "Skwakwa" typo
  rd[["name"]][rd$name == "Skwakwa"] <- "Skwawka"

  # Consolidate a few sites
  # If user provided site groupings, use that. Otherwise use default
  if (missing(site_groupings)) {
    sites_cons <- MAMU::rs
  } else {
    sites_cons <- site_groupings
  }
  # Now merge to rd
  rd <- merge(rd, sites_cons, by.x = "name", by.y = "original_name", all.x = TRUE)
  rd$new_name <- ifelse(is.na(rd$new_name), rd$name, rd$new_name)

  return(rd)
}
