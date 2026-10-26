library(terra)

# 2005
oahu05 <- rast("hi_oahu_2005_ccap_hr_land_cover20140619.img")
oahu05          # prints resolution, extent, projection
plot(oahu05)    # quick look
freq(oahu05)    # cell counts per land cover class

#2011
oahu11 <- rast("hi_oahu_2011_ccap_hr_land_cover20140619.img")
oahu11          # prints resolution, extent, projection
plot(oahu11)    # quick look
freq(oahu11)    # cell counts per land cover class

# difference
chg <- rast("hi_oahu_2005_2011_ccap_hr_change20140619.img")
head(cats(chg)[[1]], 30)        # optional: see what columns the table has

levels(chg) <- NULL             # drop labels, keep the raw 1–625 codes
NAflag(chg) <- 0   # treat background as missing
from <- (chg - 1) %/% 25 + 1    # 2005 class
to   <- (chg - 1) %%  25 + 1    # 2011 class

changed        <- from != to
new_impervious <- (to == 2) & (from != 2)

plot(changed, main = "Any change, 2005–2011")
plot(new_impervious, main = "Became impervious, 2005–2011")

# grab a couple more years
files <- c("1992" = "hi_1992_mosaic_ccap_land_cover.tif",
           "2001" = "hi_2001_mosaic_ccap_land_cover.tif",
           "2005" = "hi_2005_mosaic_ccap_land_cover.tif")

dev <- map_dfr(names(files), function(yr) {
  r <- rast(files[[yr]])
  levels(r) <- NULL
  NAflag(r) <- 0                          # background as missing
  developed <- (r >= 2 & r <= 5) * 1      # classes 2–5 = developed
  tr <- st_transform(tracts, crs(r))
  tibble(GEOID = tr$GEOID, year = as.integer(yr),
         pct_developed = exact_extract(developed, tr, "mean") * 100)
})

write_csv(dev, "oahu_tract_developed_1992_2005.csv")

# let's turn this into data!
library(tidyverse); library(tigris); library(sf); library(exactextractr)

tracts <- tracts("HI", "Honolulu", year = 2020) |>
  st_transform(crs(new_impervious))

tracts$pct_new_imp_0511 <- exact_extract(new_impervious * 1, tracts, "mean") * 100

tracts |> st_drop_geometry() |>
  select(GEOID, NAMELSAD, pct_new_imp_0511) |>
  arrange(desc(pct_new_imp_0511)) |>
  write_csv("oahu_tract_new_impervious_2005_2011.csv")
