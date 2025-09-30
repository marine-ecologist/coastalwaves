# library(httr)
# library(rvest)
# library(future.apply)
#
# base <- "https://www.ncei.noaa.gov/data/blended-global-sea-surface-wind-products/access/reprocessed/wind"
# years <- 1987:2025
# dest  <- "/Users/rof011/oceanwinds/data/winds_nc"
#
# if(!dir.exists(dest)) dir.create(dest, recursive = TRUE)
#
# # function to download all .nc for one year
# download_year <- function(y) {
#   year_url <- sprintf("%s/%d/daily/", base, y)
#   cat("Scanning:", year_url, "\n")
#   page <- try(read_html(year_url), silent = TRUE)
#   if(inherits(page, "try-error")) return(NULL)
#
#   nc_files <- html_attr(html_nodes(page, "a"), "href")
#   nc_files <- nc_files[grepl("\\.nc$", nc_files)]
#
#   year_dir <- file.path(dest, as.character(y))
#   if(!dir.exists(year_dir)) dir.create(year_dir)
#
#   for (f in nc_files) {
#     file_url <- paste0(year_url, f)
#     outfile <- file.path(year_dir, f)
#     if(!file.exists(outfile)) {
#       cat("Downloading:", file_url, "\n")
#       try(GET(file_url, write_disk(outfile, overwrite = TRUE), timeout(300)))
#     }
#   }
# }
#
# plan(multisession, workers = 8)  # set number of parallel workers
# future_lapply(years, download_year)
#

library(terra)
library(sf)

sf_obj <- st_point(c(2246766, 7300870)) |> st_sfc(crs = 20353) |> st_transform(4326) #st_sf()

dest  <- "/Users/rof011/oceanwinds/data/winds_nc"
files <- list.files(dest, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)

cropped_list <- lapply(files, function(f) {
  r <- try(terra::rast(f), silent = TRUE)
  if (inherits(r, "try-error")) {
    message("Skipping: ", f)
    return(NULL)
  }
  terra::crop(r, terra::vect(sf_obj) |> terra::buffer(50000))
})

cropped_list <- Filter(Negate(is.null), cropped_list)


# combine per variable
uwind <- do.call(c, lapply(cropped_list, \(r) r[["u_wind_zlev=10"]]))
vwind <- do.call(c, lapply(cropped_list, \(r) r[["v_wind_zlev=10"]]))
wspeed <- do.call(c, lapply(cropped_list, \(r) r[[4]]))

# writeRaster(uwind, "/Users/rof011/oceanwaves/data/uwind.tiff")
# writeRaster(vwind, "/Users/rof011/oceanwaves/data/vwind.tiff")
# writeRaster(wspeed, "/Users/rof011/oceanwaves/data/wspeed.tiff")

# extract all values with time, x, y
uwind_df <- as.data.frame(uwind, xy = TRUE, cells = TRUE, na.rm = FALSE, time=TRUE, wide=FALSE) |> drop_na(values) |> rename(u = values) |> select(-layer, -cell)
vwind_df <- as.data.frame(vwind, xy = TRUE, cells = TRUE, na.rm = FALSE, time=TRUE, wide=FALSE) |> drop_na(values) |> rename(v = values) |> select(-layer, -cell)
wspeed_df <- as.data.frame(wspeed, xy = TRUE, cells = TRUE, na.rm = FALSE, time=TRUE, wide=FALSE) |> drop_na(values) |> rename(wspeed = values) |> select(-layer, -cell)

df_whitsundays <- left_join(uwind_df, vwind_df) %>% left_join(., wspeed_df) |>
  group_by(time) |>
  summarise(u = mean(u),
            v = mean(v),
            wspeed = mean(wspeed)) %>%
  mutate(
    # bearing in degrees, 0 = north, clockwise positive
    bearing = (atan2(u, v) * 180 / pi + 360) %% 360
  ) |>
  mutate(bearing = (bearing + 180) %% 360)



# write to CSV
write.csv(df_whitsundays, "/Users/rof011/coastalwaves/data/df_sgbr.csv", row.names = FALSE)


