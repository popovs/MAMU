# Radar site consolidation
rs <- read.csv("data-raw/radar_site_consolidation.csv")
usethis::use_data(rs, overwrite = TRUE)

# Catchment names - shapefile ID combos
catch_combos <- read.csv("data-raw/radarsurvey_catchment_combos.csv")
usethis::use_data(catch_combos, overwrite = TRUE)

# All catchments geopackage
all_catchments <- sf::st_read("data-raw/all_catchments.gpkg",
                              fid_column_name = "fid")
all_catchments <- all_catchments[,c("fid", "region", "region_2", "code", "catchment", "version", "coastal_buff_500m", "area")]
usethis::use_data(all_catchments, overwrite = TRUE)
