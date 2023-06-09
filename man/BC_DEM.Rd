% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{BC_DEM}
\alias{BC_DEM}
\title{Download British Columbia Digital Elevation Model data}
\usage{
BC_DEM(
  letterblock,
  save_output = TRUE,
  overwrite = FALSE,
  output_dir = NA,
  filename = NA
)
}
\arguments{
\item{letterblock}{Map tiles of DEM data to download, e.g. \code{'92b'}. See the BC Maps & Orthos Base Map Online Store here for a visual: \url{https://a100.gov.bc.ca/ext/mtec/public/products/mapsheet}}

\item{save_output}{Logical (T/F). Should the downloaded DEM be saved to your local disk? Default TRUE.}

\item{overwrite}{Logical (T/F). Should the downloaded DEM overwrite any existing BC DEM data on the disk, if already present? Default FALSE.}

\item{output_dir}{Output directory to save downloaded DEM to. Defaults to current working directory.}

\item{filename}{Filename of saved DEM. By default, the filename is either 1) the letterblock names, seperated by underscores (e.g., "92b_92g.dem"), if there are fewer than 5 letterblocks were downloaded; or 2) simply "BC_DEM.dem", if greater than 5 letterblocks were downloaded.}
}
\value{
A RasterSpat object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Download \href{https://www2.gov.bc.ca/gov/content/data/geographic-data-services/topographic-data/elevation/digital-elevation-model}{BC DEM data}
in gridded Canadian Digital Elevation Data (CDED) format.

\emph{WARNING}: this function is still unstable, and will cause your R Session to abort
if you download a large number of tiles. But do not despair! If you download many
tiles and your R Session aborts while attempting to stitch your rasters together, the
downloaded and unzipped .dem files are somewhere on your machine - R just ran out of
memory when actually trying to stitch them together. If you open a new R Session and
enter \code{tempdir()} into the console, your raster data is somewhere in that vicinity, in
one of the \verb{Rtmp*/DEM_ZIP/DEM} directories on your computer. No need go through the time
consuming step of re-downloading all your rasters.
}
\examples{
\dontrun{
BC_DEM("92b", save_output = F) # download Victoria area DEM, don't save it
BC_DEM("103f", output_dir = "temp/bc_dem", filename = "haida_gwaii.dem") # download one tile of Haida Gwaii DEM and save output to "temp/bc_dem/haida_gwaii.dem"
}
}
