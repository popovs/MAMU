# Radar site consolidation

rs <- read.csv("data-raw/radar_site_consolidation.csv")

usethis::use_data(rs, overwrite = TRUE)
