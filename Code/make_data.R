yes_no <- askYesNo(msg = "This code takes a very long time to compute. It was run on the Scientific Computation Server of the University. Are you sure you want to run this code now?")

if (yes_no == TRUE) {
  # from now on only look at 2006!

  data_sqf <- data_sqf |>
    filter(year == 2006)

  # generate random points (non-SQFs) ####

  nyc_shape <-
    nyc_zip_shape_ll |>
    st_simplify(dTolerance = 50) |>
    st_union()

  days <- unique(data_sqf$datestop)
  n_days <- length(days)

  for (i in 1:n_days) {
    day <- days[i]

    non_sqfs <- st_sample(x = nyc_shape, size = 500, type = "random", exact = TRUE)

    for (j in seq_along(non_sqfs)) {
      data_non_sqf$xcoord[(i - 1) * 500 + j] <-
        st_coordinates(non_sqfs[j])[, 1]

      data_non_sqf$ycoord[(i - 1) * 500 + j] <-
        st_coordinates(non_sqfs[j])[, 2]

      data_non_sqf$datestop[(i - 1) * 500 + j] <- day
    }
  }

  data_all <- bind_rows(data_sqf, data_non_sqf)

  # Compute arrests and other SQFs in Radius around ####

  days <- 1 # example for day 1 and -1 and a radius of 1000m
  dist <- 1000

  ## Functions ####

  data_arrest <-
    data_arrest |>
    filter(
      !is.na(ARREST_DATE),
      !is.na(Lon_Lat)
    ) |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs("WGS84"))

  data_all_sf <-
    data_all |>
    st_as_sf(coords = c("xcoord", "ycoord"), crs = st_crs("WGS84"))

  arrests_before_fct <- function(day) {
    y <- data_arrest |>
      filter(ARREST_DATE == day - days)
    y
  }
  arrests_before_fct <- compiler::cmpfun(arrests_before_fct)

  arrests_after_fct <- function(day) {
    y <- data_arrest |>
      filter(ARREST_DATE == day + days)
    y
  }
  arrests_after_fct <- compiler::cmpfun(arrests_after_fct)

  data_other_sqfs <- data_all_sf |>
    filter(is_sqf == 1)

  other_sqfs_before_fct <- function(day) {
    y <- data_other_sqfs |>
      filter(datestop == day - days)
    y
  }
  other_sqfs_before_fct <- compiler::cmpfun(other_sqfs_before_fct)

  other_sqfs_after_fct <- function(day) {
    y <- data_other_sqfs |>
      filter(datestop == day + days)
    y
  }
  other_sqfs_after_fct <- compiler::cmpfun(other_sqfs_after_fct)

  ## computation ####


  data_all$arrests_b1 <-
    data_all_sf |>
    mutate(
      arrests_b1 = st_is_within_distance(
        x = geometry, y = arrests_before_fct(day = datestop),
        dist = dist
      ),
      arrests_b1 = sapply(arrests_b1, length)
    ) |>
    pull(arrests_b1)


  data_all$arrests_a1 <-
    data_all_sf |>
    mutate(
      arrests_a1 = st_is_within_distance(
        x = geometry, y = arrests_after_fct(day = datestop),
        dist = dist
      ),
      arrests_a1 = sapply(arrests_a1, length)
    ) |>
    pull(arrests_a1)


  data_all$other_sqfsb1 <-
    data_all_sf |>
    mutate(
      other_sqfsb1 = st_is_within_distance(
        x = geometry, y = other_sqfs_before_fct(day = datestop),
        dist = dist
      ),
      other_sqfsb1 = sapply(other_sqfsb1, length)
    ) |>
    pull(other_sqfsb1)


  data_all$other_sqfsa1 <-
    data_all_sf |>
    mutate(
      other_sqfsa1 = st_is_within_distance(
        x = geometry, y = other_sqfs_after_fct(day = datestop),
        dist = dist
      ),
      other_sqfsa1 = sapply(other_sqfsa1, length)
    ) |>
    pull(other_sqfsa1)

  # This process was repeated for distances of 5000m and 10000m and all days from -10 to 10.

  # determine regions ####

  contains <- st_contains(x = nyc_shape_ll$geometry, y = data_all_sf$geometry)

  data_all_reg <-
    data_all |>
    mutate(reg = case_when(
      id %in% contains[[1]] ~ nyc_shape_ll$BoroName[1],
      id %in% contains[[2]] ~ nyc_shape_ll$BoroName[2],
      id %in% contains[[3]] ~ nyc_shape_ll$BoroName[3],
      id %in% contains[[4]] ~ nyc_shape_ll$BoroName[4],
      id %in% contains[[5]] ~ nyc_shape_ll$BoroName[5]
    ))

  plot(nyc_shape_ll$geometry)
  data_all_reg |>
    filter(is.na(reg)) |>
    st_as_sf(coords = c("xcoord", "ycoord"), crs = st_crs("WGS84")) |>
    select(geometry) |>
    plot(add = TRUE)

  # Problem: Points on the edges won't be accounted to be contained!
  # Thus: look for closest Polygon.

  tmp <-
    data_all_reg |>
    filter(is.na(reg))

  tmp_sf <-
    st_as_sf(tmp, coords = c("xcoord", "ycoord"), crs = st_crs("WGS84"))


  near <- st_nearest_feature(tmp_sf$geometry, nyc_shape_ll$geometry)

  tmp <-
    tmp |>
    mutate(reg = nyc_shape_ll$BoroName[near]) |>
    select(id, reg)

  data_all_reg <-
    left_join(data_all_reg, tmp, by = "id") |>
    mutate(reg = case_when(
      is.na(reg.x) ~ reg.y,
      TRUE ~ reg.x
    )) |>
    select(-c(reg.x, reg.y))


  data_all <- data_all_reg
  
  # Save the data ####
  
  saveRDS(data_all, file = "Data/Clean/data_final.Rds")
  
  tmp1km <-
    data_all |>
    select(id, is_sqf, reg, starts_with("arrests") & ends_with("_1km")) |>
    pivot_longer(cols = ends_with("_1km"), names_to = "time_to_sqf",
                 values_to = "arrests") |>
    mutate(time_to_sqf = time_to_sqf |>
             str_remove("arrests_") |>
             str_remove("_1km") |>
             str_replace("a", "+") |>
             str_replace("b", "-") |>
             as.numeric(),
           neg = if_else(time_to_sqf <0, 0, 1) |>
             as.factor(),
           is_sqf = as.factor(is_sqf),
           dist = 1
    )
  
  tmp5km <-
    data_all |>
    select(id, is_sqf, reg, starts_with("arrests") & ends_with("_5km")) |>
    pivot_longer(cols = ends_with("_5km"), names_to = "time_to_sqf",
                 values_to = "arrests") |>
    mutate(time_to_sqf = time_to_sqf |>
             str_remove("arrests_") |>
             str_remove("_5km") |>
             str_replace("a", "+") |>
             str_replace("b", "-") |>
             as.numeric(),
           neg = if_else(time_to_sqf <0, 0, 1) |>
             as.factor(),
           is_sqf = as.factor(is_sqf),
           dist = 5
    )
  
  tmp10km <-
    data_all |>
    select(id, is_sqf, reg, starts_with("arrests") & ends_with("_10km")) |>
    pivot_longer(cols = ends_with("_10km"), names_to = "time_to_sqf",
                 values_to = "arrests") |>
    mutate(time_to_sqf = time_to_sqf |>
             str_remove("arrests_") |>
             str_remove("_10km") |>
             str_replace("a", "+") |>
             str_replace("b", "-") |>
             as.numeric(),
           neg = if_else(time_to_sqf <0, 0, 1) |>
             as.factor(),
           is_sqf = as.factor(is_sqf),
           dist = 10
    )
  
  
  tmp1km_1 <-
    data_all |>
    select(id, is_sqf, reg, starts_with("other_sqf") & ends_with("_1km")) |>
    pivot_longer(cols = ends_with("_1km"), names_to = "time_to_sqf",
                 values_to = "other_sqfs") |>
    mutate(time_to_sqf = time_to_sqf |>
             str_remove("other_sqfs") |>
             str_remove("_1km") |>
             str_replace("a", "+") |>
             str_replace("b", "-") |>
             as.numeric(),
           neg = if_else(time_to_sqf <0, 0, 1) |>
             as.factor(),
           is_sqf = as.factor(is_sqf),
           dist = 1
    )
  
  tmp5km_1 <-
    data_all |>
    select(id, is_sqf, reg, starts_with("other_sqf") & ends_with("_5km")) |>
    pivot_longer(cols = ends_with("_5km"), names_to = "time_to_sqf",
                 values_to = "other_sqfs") |>
    mutate(time_to_sqf = time_to_sqf |>
             str_remove("other_sqfs") |>
             str_remove("_5km") |>
             str_replace("a", "+") |>
             str_replace("b", "-") |>
             as.numeric(),
           neg = if_else(time_to_sqf <0, 0, 1) |>
             as.factor(),
           is_sqf = as.factor(is_sqf),
           dist = 5
    )
  
  tmp10km_1 <-
    data_all |>
    select(id, is_sqf, reg, starts_with("other_sqf") & ends_with("_10km")) |>
    pivot_longer(cols = starts_with("other_sqf"), names_to = "time_to_sqf",
                 values_to = "other_sqfs") |>
    mutate(time_to_sqf = time_to_sqf |>
             str_remove("other_sqfs") |>
             str_remove("_10km") |>
             str_replace("a", "+") |>
             str_replace("b", "-") |>
             as.numeric(),
           neg = if_else(time_to_sqf <0, 0, 1) |>
             as.factor(),
           is_sqf = as.factor(is_sqf),
           dist = 10
    )
  
  data_all_long <- bind_rows(tmp1km, tmp5km, tmp10km)
  
  data_all_long_1 <- bind_rows(tmp1km_1, tmp5km_1, tmp10km_1)
  
  data_all_long <- left_join(data_all_long, data_all_long_1)
  
  saveRDS(data_all_long, "Data/Clean/data_final_long.Rds")
}
