# Oʻahu urban heat: build the tract-by-year analysis table
#
# Inputs (all in the working directory):
#   oahu_tract_year.csv                       Earth Engine export: GEOID, year, lst_c, ndvi, n_clear
#   oahu_tract_elevation.csv                  Earth Engine export: GEOID, elev_m
#   oahu_tract_developed_1992_2005.csv        from oahu_landcover_tracts.R (section 5)
#   oahu_tract_impervious_2005_2011_2021.csv  from oahu_landcover_tracts.R (section 6)
#   oahu_tract_new_impervious_2005_2011.csv   from oahu_landcover_tracts.R (section 4)
#   tract_groups.csv                          Amara's hand-made file: GEOID, group
#                                             group = "urban_core", "newly_developed" or "rural"
# Output:
#   oahu_analysis_table.csv                   one row per tract per year

library(tidyverse)
library(sf)
library(tigris)

# GEOID must be read as text everywhere, or joins silently fail
read_geo <- function(f) read_csv(file.path("data", f), col_types = cols(GEOID = col_character()))

drop_tracts <- c("15003981200",   # NA in every land-cover table
                 "15003990001")   # water-only tract

# ---------- 1. Earth Engine exports (the outcome) ----------
lst  <- read_geo("oahu_tract_year.csv")
elev <- read_geo("oahu_tract_elevation.csv")

# ---------- 2. Land-cover tables ----------
developed <- read_geo("oahu_tract_developed_1992_2005.csv")          # long: GEOID, year, pct_developed
impervious <- read_geo("oahu_tract_impervious_2005_2011_2021.csv")
new_imp   <- read_geo("oahu_tract_new_impervious_2005_2011.csv") |>
  select(GEOID, pct_new_imp_0511)

# Checkpoint-year measures, long format so they line up with `year`
imp_long <- impervious |>
  select(GEOID, starts_with("pct_imp_")) |>
  pivot_longer(-GEOID, names_to = "year", names_prefix = "pct_imp_",
               names_transform = list(year = as.integer), values_to = "pct_impervious")

# Fixed tract descriptors (one value per tract), used for grouping and as controls
tract_fixed <- developed |>
  pivot_wider(names_from = year, values_from = pct_developed, names_prefix = "dev_") |>
  mutate(dev_change_92_05 = dev_2005 - dev_1992) |>
  left_join(impervious |> select(GEOID, pct_canopy_2021), by = "GEOID") |>
  left_join(new_imp, by = "GEOID")

# Guard against the 2021 canopy problem: a share can't exceed 100
if (any(tract_fixed$pct_canopy_2021 > 100, na.rm = TRUE)) {
  warning("pct_canopy_2021 has values above 100 - rerun section 6 of ",
          "oahu_landcover_tracts.R after fixing the coding. Setting canopy to NA for now.")
  tract_fixed$pct_canopy_2021 <- NA_real_
}

# ---------- 3. Comparison groups ----------
groups <- if (file.exists("tract_groups.csv")) {
  read_geo("tract_groups.csv")
} else {
  message("tract_groups.csv not found yet - group columns will be NA")
  tibble(GEOID = character(), group = character())
}

# ---------- 4. Join ----------
dat <- lst |>
  filter(!GEOID %in% drop_tracts, !is.na(lst_c)) |>
  left_join(elev, by = "GEOID") |>
  left_join(tract_fixed, by = "GEOID") |>
  left_join(developed |> rename(pct_developed_cy = pct_developed),
            by = c("GEOID", "year")) |>                       # filled only in 1992/2001/2005
  left_join(imp_long, by = c("GEOID", "year")) |>             # filled only in 2005/2011/2021
  left_join(groups, by = "GEOID")

# Join checks: if these print 0 or tiny numbers, the GEOIDs don't match
cat("Tracts in Earth Engine export:", n_distinct(lst$GEOID), "\n")
cat("  ...with land-cover data:      ", sum(unique(dat$GEOID) %in% tract_fixed$GEOID), "\n")
cat("  ...with a group assigned:     ", sum(unique(dat$GEOID) %in% groups$GEOID), "\n")

# Urban excess: each tract's temperature minus the rural mean that year.
# Removes hot years, cool years, and image-to-image differences.
if (nrow(groups) > 0) {
  dat <- dat |>
    group_by(year) |>
    mutate(lst_excess = lst_c - mean(lst_c[group == "rural"], na.rm = TRUE)) |>
    ungroup()
}

write_csv(dat, "oahu_analysis_table.csv")
glimpse(dat)

# ---------- 5. First looks ----------
# How many clear observations sit behind each year? Thin years are less trustworthy.
dat |>
  group_by(year) |>
  summarise(median_n_clear = median(n_clear, na.rm = TRUE)) |>
  print(n = Inf)

# Trend by group
if ("lst_excess" %in% names(dat)) {
  dat |>
    filter(!is.na(group)) |>
    group_by(group, year) |>
    summarise(lst_excess = mean(lst_excess, na.rm = TRUE),
              ndvi = mean(ndvi, na.rm = TRUE), .groups = "drop") |>
    pivot_longer(c(lst_excess, ndvi)) |>
    ggplot(aes(year, value, colour = group)) +
    geom_line() +
    facet_wrap(~name, scales = "free_y", ncol = 1) +
    labs(x = NULL, y = NULL, colour = NULL)
}

# Map of one year
tracts_cb <- tracts("HI", "Honolulu", year = 2020, cb = TRUE)
tracts_cb |>
  left_join(filter(dat, year == 2023), by = "GEOID") |>
  filter(!is.na(lst_c)) |>
  ggplot() +
  geom_sf(aes(fill = lst_c), colour = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Surface temp (°C)") +
  coord_sf(xlim = c(-158.3, -157.6), ylim = c(21.2, 21.75))

# Sanity check on the development measures: greener tracts should be less built
dat |>
  filter(!is.na(pct_impervious)) |>
  ggplot(aes(pct_impervious, ndvi)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~year)

# ---------- 6. Model sketch ----------
# Within tracts over time: does lower greenness go with higher surface temperature?
# library(lme4)
# m <- lmer(lst_excess ~ ndvi + elev_m + (1 | GEOID), data = dat)
# summary(m)
