library(here)
library(tidycensus)
library(tidyverse)
library(sf)
library(ggplot2)
library(tigris)

options(tigris_use_cache = TRUE)

# FIPS codes for 50 states + DC
state_fips <- c(
  sprintf("%02d", 1:56)
) |> setdiff(c("03", "07", "14", "43", "52"))

#List of states 
state_fips <- c(
  "01","02","04","05","06","08","09","10","11","12",
  "13","15","16","17","18","19","20","21","22","23",
  "24","25","26","27","28","29","30","31","32","33",
  "34","35","36","37","38","39","40","41","42","44",
  "45","46","47","48","49","50","51","53","54","55","56"
)

all_counties_2015 <- counties(year = 2015, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2016 <- counties(year = 2016, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2017 <- counties(year = 2017, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2018 <- counties(year = 2018, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2019 <- counties(year = 2019, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2020 <- counties(year = 2020, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2021 <- counties(year = 2021, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2022 <- counties(year = 2022, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2023 <- counties(year = 2023, cb = TRUE) |> filter(STATEFP %in% state_fips)
all_counties_2024 <- counties(year = 2024, cb = TRUE) |> filter(STATEFP %in% state_fips)


# Filter all years
years <- list(
  "2015" = all_counties_2015, "2016" = all_counties_2016,
  "2017" = all_counties_2017, "2018" = all_counties_2018,
  "2019" = all_counties_2019, "2020" = all_counties_2020,
  "2021" = all_counties_2021, "2022" = all_counties_2022,
  "2023" = all_counties_2023, "2024" = all_counties_2024
)

# Number of counties by year
sapply(years, nrow)

#2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 
#3142 3142 3142 3142 3142 3143 3143 3144 3144 3144


#Compare GEOIDs that changed 
geoids <- lapply(years, function(x) sort(x$GEOID))

for (i in 2:length(geoids)) {
  added   <- setdiff(geoids[[i]], geoids[[i - 1]])
  removed <- setdiff(geoids[[i - 1]], geoids[[i]])
  if (length(added) > 0 | length(removed) > 0) {
    cat(names(geoids)[i - 1], "->", names(geoids)[i], "\n")
    if (length(added) > 0)   cat("  Added:", added, "\n")
    if (length(removed) > 0) cat("  Removed:", removed, "\n")
  }
}

# Boundaries for 2015 to 2021, before CT boundary changes
allcounties_2015_2021 <- all_counties_2019 |> 
  filter(STATEFP %in% state_fips) |> 
  mutate(
    # SD: Oglala Lakota -> Shannon
    GEOID = ifelse(GEOID == "46102", "46113", GEOID),
    NAME  = ifelse(GEOID == "46113", "Shannon", NAME),
    
    # AK: Kusilvak -> Wade Hampton
    GEOID = ifelse(GEOID == "02158", "02270", GEOID),
    NAME  = ifelse(GEOID == "02270", "Wade Hampton", NAME),
    
    # AK: Chugach & Copper River -> Valdez-Cordova (in 2019)
    GEOID = ifelse(GEOID %in% c("02063", "02066"), "02261", GEOID),
    NAME  = ifelse(GEOID == "02261", "Valdez-Cordova", NAME)
  ) |> 
  group_by(GEOID, NAME, STATEFP) |> 
  summarise(geometry = st_union(geometry), .groups = "drop")


allcounties_2015_2021 <- all_counties_2019 |> 
  filter(STATEFP %in% state_fips) |> 
  mutate(
    # SD: Oglala Lakota -> Shannon (if 2019 already uses new code)
    GEOID = ifelse(GEOID == "46102", "46113", GEOID),
    NAME  = ifelse(GEOID == "46113", "Shannon", NAME),
    
    # AK: Kusilvak -> Wade Hampton (if 2019 already uses new code)
    GEOID = ifelse(GEOID == "02158", "02270", GEOID),
    NAME  = ifelse(GEOID == "02270", "Wade Hampton", NAME)
  )

# Plot boundaries
ggplot(shift_geometry(allcounties_2015_2021)) +
  geom_sf(fill = NA, color = "black", linewidth = 0.1) +
  theme_void()


# Boundaries for 2022 to 2024, new CT boundaries
allcounties_2022_2024 <- all_counties_2019 |> 
  filter(STATEFP %in% state_fips) |> 
  mutate(
    # SD: Oglala Lakota -> Shannon (if 2019 already uses new code)
    GEOID = ifelse(GEOID == "46102", "46113", GEOID),
    NAME  = ifelse(GEOID == "46113", "Shannon", NAME),
    
    # AK: Kusilvak -> Wade Hampton (if 2019 already uses new code)
    GEOID = ifelse(GEOID == "02158", "02270", GEOID),
    NAME  = ifelse(GEOID == "02270", "Wade Hampton", NAME)
  )


st_write(all_counties_2015, here("data", "2_intermediate", "ctyboundaries", "counties_2015.shp"))
st_write(all_counties_2016, here("data", "2_intermediate", "ctyboundaries", "counties_2016.shp"))
st_write(all_counties_2017, here("data", "2_intermediate", "ctyboundaries", "counties_2017.shp"))
st_write(all_counties_2018, here("data", "2_intermediate", "ctyboundaries", "counties_2018.shp"))
st_write(all_counties_2019, here("data", "2_intermediate", "ctyboundaries", "counties_2019.shp"))
st_write(all_counties_2020, here("data", "2_intermediate", "ctyboundaries", "counties_2020.shp"))
st_write(all_counties_2021, here("data", "2_intermediate", "ctyboundaries", "counties_2021.shp"))
st_write(all_counties_2022, here("data", "2_intermediate", "ctyboundaries", "counties_2022.shp"))
st_write(all_counties_2023, here("data", "2_intermediate", "ctyboundaries", "counties_2023.shp"))
st_write(all_counties_2024, here("data", "2_intermediate", "ctyboundaries", "counties_2024.shp"))


st_write(allcounties_2015_2021, here("data", "2_intermediate", "ctyboundaries", "counties_2015_2021.shp"))
st_write(allcounties_2022_2024, here("data", "2_intermediate", "ctyboundaries", "counties_2022_2024.shp"))


###########################

# csv with list of counties


years <- 2015:2024

fips_by_year <- data.frame()

for (yr in years) {
  cty <- counties(year = yr, cb = TRUE)
  cty_df <- st_drop_geometry(cty)
  cty_df <- cty_df[cty_df$STATEFP %in% state_fips, ]
  cty_df$year <- yr
  fips_by_year <- rbind(fips_by_year, cty_df[, c("year", "GEOID", "NAME", "STATEFP")])
}


write.csv(fips_by_year, here("data", "2_intermediate", "ctylist", "county_fips_by_year.csv"), row.names = FALSE)

###########


# Boundary in 2020 


years <- 2020

fips_2020 <- data.frame()

for (yr in years) {
  cty <- counties(year = yr, cb = TRUE)
  cty_df <- st_drop_geometry(cty)
  cty_df <- cty_df[cty_df$STATEFP %in% state_fips, ]
  cty_df$year <- yr
  fips_2020 <- rbind(fips_2020, cty_df[, c("year", "GEOID", "NAME", "STATEFP")])
}


write.csv(fips_2020, here("data", "2_intermediate", "ctylist", "county_fips_by_2020.csv"), row.names = FALSE)

### Boundary in 2022
years <- 2022

fips_2022 <- data.frame()

for (yr in years) {
  cty <- counties(year = yr, cb = TRUE)
  cty_df <- st_drop_geometry(cty)
  cty_df <- cty_df[cty_df$STATEFP %in% state_fips, ]
  cty_df$year <- yr
  fips_2022 <- rbind(fips_2022, cty_df[, c("year", "GEOID", "NAME", "STATEFP")])
}

write.csv(fips_2022, here("data", "2_intermediate", "ctylist", "county_fips_by_2022.csv"), row.names = FALSE)







