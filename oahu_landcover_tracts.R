# Oʻahu land cover -> tract-level tables (NOAA C-CAP)
# Outputs:
#   oahu_tract_new_impervious_2005_2011.csv   (high-res change file)
#   oahu_tract_developed_1992_2005.csv        (30 m mosaics)

library(tidyverse)
library(terra)
library(sf)
library(tigris)
library(exactextractr)

# 0. Download the 30 m mosaics if they aren't here yet
base <- "https://chs.coast.noaa.gov/htdata/raster1/landcover/bulkdownload/30m_lc/"
files <- c("1992" = "hi_1992_mosaic_ccap_land_cover.tif",
           "2001" = "hi_2001_mosaic_ccap_land_cover.tif",
           "2005" = "hi_2005_mosaic_ccap_land_cover.tif")
for (f in files) {
  if (!file.exists(f)) download.file(paste0(base, f), f, mode = "wb")
}

# 1. Quick looks at the high-res land cover
# 2005
oahu05 <- rast("hi_oahu_2005_ccap_hr_land_cover20140619.img")
oahu05          # prints resolution, extent, projection
plot(oahu05)    # quick look
freq(oahu05)    # cell counts per land cover class

# 2011
oahu11 <- rast("hi_oahu_2011_ccap_hr_land_cover20140619.img")
oahu11
plot(oahu11)
freq(oahu11)

# 2. Decode the 2005-2011 change file
chg <- rast("hi_oahu_2005_2011_ccap_hr_change20140619.img")
head(cats(chg)[[1]], 30)        # optional: see what columns the table has

levels(chg) <- NULL             # drop labels, keep the raw 1-625 codes
NAflag(chg) <- 0                # treat background as missing
from <- (chg - 1) %/% 25 + 1    # 2005 class
to   <- (chg - 1) %%  25 + 1    # 2011 class

changed        <- from != to
new_impervious <- (to == 2) & (from != 2)

plot(changed, main = "Any change, 2005–2011")
plot(new_impervious, main = "Became impervious, 2005–2011")

# 3. Census tracts (needed by both tables below)
tracts <- tracts("HI", "Honolulu", year = 2020) |>
  st_transform(crs(new_impervious))

# 4. Table 1: % of tract newly impervious, 2005-2011
tracts$pct_new_imp_0511 <- exact_extract(new_impervious * 1, tracts, "mean") * 100

tracts |> st_drop_geometry() |>
  select(GEOID, NAMELSAD, pct_new_imp_0511) |>
  arrange(desc(pct_new_imp_0511)) |>
  write_csv("oahu_tract_new_impervious_2005_2011.csv")

# 5. Table 2: % of tract developed, 1992 / 2001 / 2005 (30 m)
dev <- map_dfr(names(files), function(yr) {
  r <- rast(files[[yr]])
  levels(r) <- NULL
  NAflag(r) <- 0                          # background as missing
  developed <- (r >= 2 & r <= 5) * 1      # classes 2-5 = developed
  tr <- st_transform(tracts, crs(r))
  tibble(GEOID = tr$GEOID, year = as.integer(yr),
         pct_developed = exact_extract(developed, tr, "mean") * 100)
})

write_csv(dev, "oahu_tract_developed_1992_2005.csv")

# 6. Table 3: % of tract impervious in 2005, 2011, 2021 (+ canopy 2021)
# 2021 layers are NOAA's newer 1 m product (0 = not impervious / not canopy, 1 = yes).
# Check with freq() on a small crop before trusting the numbers.
base21 <- paste0("https://ocmgeodatastor1.blob.core.windows.net/ccap/bulk_download/",
                 "C-CAP_High-Resolution_Data/Initial_C-CAP_High-Resolution_Land_Cover_Layers/")
f_imp21 <- "hi_honolulu_2021_ccap_v2_hires_impervious.tif"   # ~42 MB
f_can21 <- "hi_honolulu_2021_ccap_v2_hires_canopy.tif"       # ~66 MB
if (!file.exists(f_imp21)) download.file(paste0(base21, "Impervious/Pacific/", f_imp21), f_imp21, mode = "wb")
if (!file.exists(f_can21)) download.file(paste0(base21, "Canopy/Pacific/", f_can21), f_can21, mode = "wb")

# Shoreline-clipped tracts, so ocean doesn't dilute coastal tracts
tracts_cb <- tracts("HI", "Honolulu", year = 2020, cb = TRUE)

share_of_tract <- function(file, value = NULL) {
  r <- rast(file)
  levels(r) <- NULL
  if (!is.null(value)) r <- (r == value) * 1     # 2005/2011: impervious is class 2
  tr <- st_transform(tracts_cb, crs(r))
  exact_extract(r, tr, "mean") * 100
}

imp <- tibble(
  GEOID           = tracts_cb$GEOID,
  pct_imp_2005    = share_of_tract("hi_oahu_2005_ccap_hr_land_cover20140619.img", 2),
  pct_imp_2011    = share_of_tract("hi_oahu_2011_ccap_hr_land_cover20140619.img", 2),
  pct_imp_2021    = share_of_tract(f_imp21),      # slow: 1 m cells
  pct_canopy_2021 = share_of_tract(f_can21)       # slow: 1 m cells
)

write_csv(imp, "oahu_tract_impervious_2005_2011_2021.csv")

