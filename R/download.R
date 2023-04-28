# Download MAMU relevant data, including:
# - 2021 VRI data
# - BC 25m resolution DEM data

# Code modelled off tidyhydat:
# https://github.com/ropensci/tidyhydat

#' @title Output path to the VRI database
#'
#' @description Prints the VRI database storage location, regardless of OS.
#' E.g., for Mac:
#'
#' `~Library/Application Support/VRI`
#'
#' If on Windows, either:
#'
#' `C:\\Documents and Settings\<User>\\Application Data\\Local Settings\<AppAuthor>\\VRI`
#'
#' Or:
#'
#' `C:\\Documents and Settings\<User>\\Application Data\<AppAuthor>\\VRI`
#'
#' @param ... arguments potentially passed to \code{rappdirs::user_data_dir}.
#'
#' @export
#'
#' @examples \dontrun{
#' bpfa_dir()
#' }
vri_dir <- function(...) {
  path.expand(
    rappdirs::user_data_dir(appname = "MAMU", ...)
  )
}


# TODO HERE: some sort of function to download the VRI database for you
# Download and save 2021 VRI database
# https://catalogue.data.gov.bc.ca/dataset/vri-2021-forest-vegetation-composite-layers-all-layers-

#' Return letterblock (tile) download URLs
#'
#' For a given letterblock (aka tile), scrape all the associated download URLs.
#'
#' @param url FTP URL containing all parent DEM download directories (i.e., https://pub.data.gov.bc.ca/datasets/175624/)
#' @param tile Name of letterblock that you want to download DEMs from (e.g., '95d', '103p', '115a', etc.)
#'
#' @return A character vector of all download URLs associated with a given BC DEM letterblock.
tile_urls <- function(url = "https://pub.data.gov.bc.ca/datasets/175624/",
                      tile) {
  baseurl <- paste0(url, tile)
  x <- rvest::read_html(baseurl) %>%
    rvest::html_node("pre") %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    paste0(baseurl, .)
  x <- x[grepl("dem.zip$", x)]
  return(x)
}

#' Download zipped DEM file from a given URL
#'
#' @param url Download link to a zipped DEM file (typically as returned by [tile_urls])
#' @param dir Output directory to save the downloaded DEM ZIP file to
#'
#' @return
download_zip <- function(url, dir) {
  basename <- basename(url)
  download.file(url = url, destfile = file.path(dir, basename))
  unzip(file.path(dir, basename),
        exdir = file.path(dir, "DEM"))
}

#' Download British Columbia Digital Elevation Model data
#'
#' @param letterblock Character vector of map tiles of DEM data to download, e.g. `'92b'` for a single tile or `c('92b', '92g')` for multiple tiles. See the BC Maps & Orthos Base Map Online Store here for a visual: \url{https://a100.gov.bc.ca/ext/mtec/public/products/mapsheet}
#' @param save_output Logical (T/F). Should the downloaded DEM be saved to your local disk? Default TRUE.
#' @param overwrite Logical (T/F). Should the downloaded DEM overwrite any existing BC DEM data on the disk, if already present? Default FALSE.
#' @param output_dir Output directory to save downloaded DEM to. Defaults to current working directory.
#' @param filename Filename of saved DEM. By default, the filename is either 1) the letterblock names, seperated by underscores (e.g., "92b_92g.dem"), if there are fewer than 5 letterblocks were downloaded; or 2) simply "BC_DEM.dem", if greater than 5 letterblocks were downloaded.
#'
#' @return A RasterSpat object.
#' @export
#'
#' @examples
#' \dontrun {
#' BC_DEM("92b", save_output = F) # download Victoria area DEM
#' BC_DEM(c("92b", "92g", "92f", "92c"), output_dir = "temp/bc_dem") # download SW Vancouer island + lower mainland DEM and save output to the "temp/bc_dem" directory. The generated file will be "temp/bc_dem/92b_92c_92f_92g.dem"
#' BC_DEM(c("103k", "103j", "103f", "103g", "103c", "103b", "102o"), output_dir = "temp/bc_dem", filename = "haida_gwaii.dem") # download Haida Gwaii DEM and save output to "temp/bc_dem/haida_gwaii.dem"
#' }
BC_DEM <- function(letterblock,
                   save_output = TRUE,
                   overwrite = FALSE,
                   output_dir = NA,
                   filename = NA) {
  # Check and clean up provided letterblocks
  tiles_to_download <- letterblock
  stopifnot("`letterblock` must be a character vector, e.g. '103k' for a single letterblock or 'c('103k', '103j', '92n')' for multiple letterblocks." =
              inherits(tiles_to_download, "character"))
  tiles_to_download <- tolower(tiles_to_download)
  tiles_to_download <- unique(tiles_to_download)

  # Prepare data download links
  # Note I refer to the letterblocks as "tiles" throughout the code..
  url <- "https://pub.data.gov.bc.ca/datasets/175624/"
  h <- rvest::read_html(url)

  tiles <- h %>%
    rvest::html_node("pre") %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href")

  tiles <- tiles[grepl("^\\d.*/$", tiles)] # filter to only URL suffixes that actually correspond to tiles

  # Check that provided letterblock(s) (aka tiles_to_download) actually exist in 'tiles' list
  if (any(!(tiles_to_download %in% gsub("/", "", tiles)))) {
    stop("One or more of your supplied letterblocks does not actually exist. See https://pub.data.gov.bc.ca/datasets/175624/ for a list of available DEM letterblocks.")
  }

  # Filter down 'tiles' to only include the tiles_to_download values
  tiles <- tiles[grepl(paste(tiles_to_download, collapse = "|"), tiles)]

  # Grab all the download links using `tile_urls` function
  dem.zip <- lapply(tiles, function(x) tile_urls(tile = x))
  names(dem.zip) <- tiles # useful for troubleshooting...
  dem.zip <- unlist(dem.zip)
  dem.zip <- unname(dem.zip)

  # Now actually download
  tmp <- tempdir() # Create temporary directory
  dir.create(file.path(tmp, "DEM_ZIP"), showWarnings = FALSE) # Create DEM_ZIP dir within tmp

  # Download all DEM ZIP using `download_zip` function
  sapply(dem.zip, download_zip, dir = file.path(tmp, "DEM_ZIP"))

  # Grab file locations of all downloaded zip files
  tiles_ls <- list.files(file.path(tmp, "DEM_ZIP", "DEM"), full.names = TRUE)

  # Now work terra magic
  message("Stitching together your DEMs...")
  # Now read rasters with terra
  rlist <- lapply(tiles_ls, terra::rast)
  # Create SpatRasterCollection
  rlist <- terra::sprc(rlist)
  # Merge them
  # TODO: FIX THIS COMPUTER KILLING STEP!!
  m <- terra::mosaic(rlist) # this will destroy your computer's ram if it's for many tiles

  # Unlink temp dir
  unlink(file.path(tmp, "DEM_ZIP"), recursive = TRUE)

  # Save and return raster output
  if (is.na(output_dir)) output_dir <- getwd()
  if (!dir.exists(output_dir)) {
    message("Could not find supplied output dir '", output_dir, "'. Instead defaulting to current working directory '", getwd(), "'.")
    output_dir <- getwd()
  }

  if (is.na(filename)) {
    filename <- ifelse(length(tiles_to_download) > 5,
                       "BC_DEM.dem",
                       paste0(paste0(sort(tiles_to_download), collapse = "_"), ".dem"))
  } else {
    if (!grepl(".dem$", filename)) filename <- paste0(filename, ".dem")
  }

  if (save_output) {
    message("Saving DEM...")
    terra::writeRaster(m,
                       overwrite = overwrite,
                       filetype = "USGSDEM",
                       file.path(output_dir, filename))
  }

  return(m)

}
