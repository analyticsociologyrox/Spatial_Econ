source("Code/Makefile.R")
# Explore ####
nyc_shape_ll <-
  nyc_shape_ll |>
  st_simplify()

## SQF Missings ####

data_sqf |>
  group_by(year) |>
  summarise(n_complete_coord = dplyr::n()) |>
  merge(n_sqf_all) |>
  transmute(
    year = year,
    `N with coords` = n_complete_coord,
    `N all SQFs` = n_all,
    `missing` = round(1-n_complete_coord/n_all, 4)*100,
         `missing` = paste(`missing`, "%")) |>
  gt() |>
  tab_options(
    column_labels.padding.horizontal = 15,
    data_row.padding.horizontal = 15,
    stub.border.width = 2,
    table_body.hlines.color = "black",
    stub.border.color = "black",
    table_body.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table.border.top.color = "black"
  ) |>
  tab_header("SQFs: Included Data and Missings")

## Arrest Missings ####

n_arrests_all <-
  data_arrest |>
  group_by(year(ARREST_DATE)) |>
  summarise(n_all = dplyr::n())
  
data_arrest <-
  data_arrest |>
  filter(!is.na(Latitude),
         !is.na(Longitude),
         Latitude <= 41
         )

data_arrest |>
  group_by(year(ARREST_DATE)) |>
  summarise(n_complete_coord = dplyr::n()) |>
  merge(n_arrests_all) |>
  transmute(
    year = `year(ARREST_DATE)`,
    `N with coords` = n_complete_coord,
    `N all Arrests` = n_all,
    `missing` = round(1-n_complete_coord/n_all, 4)*100,
    `missing` = paste(`missing`, "%")) |>
  gt() |>
  tab_options(
    column_labels.padding.horizontal = 15,
    data_row.padding.horizontal = 15,
    stub.border.width = 2,
    table_body.hlines.color = "black",
    stub.border.color = "black",
    table_body.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table.border.top.color = "black"
  ) |>
  tab_header("Arrests: Included Data and Missings")

## SQF Data ####

points_sqf_ll <- 
  data_sqf |>
  # filter(year == 2006) |>
  sample_n(800*6) |>
  mutate(across(.cols = ends_with("coord"),.fns = as.numeric),
         geometry = st_multipoint(as.matrix(cbind(xcoord, ycoord)))
         ) |>
  # select(geometry) |>
  st_as_sf(coords = c("xcoord","ycoord"), crs = st_crs("EPSG:2263")) |>
  st_transform(crs = st_crs("WGS84"))

points_sqf_2006_ll <- 
  data_sqf |>
  filter(year == 2006) |>
  sample_n(800) |>
  mutate(across(.cols = ends_with("coord"),.fns = as.numeric),
         geometry = st_multipoint(as.matrix(cbind(xcoord, ycoord)))
  ) |>
  # select(geometry) |>
  st_as_sf(coords = c("xcoord","ycoord"), crs = st_crs("EPSG:2263")) |>
  st_transform(crs = st_crs("WGS84"))


# st_crs(points_sqf_2006_ll)

gif <-
  points_sqf_ll |>
  mutate(year = as.numeric(year)) |>
  ggplot() +
  geom_sf(data = nyc_shape_ll, geometry = geometry) +
  geom_sf() +
  coord_sf(crs = "WGS84") +
  theme_bw() +
  transition_time(time = year) +
  labs(subtitle = "Year: {frame_time}")

animate(gif, renderer = gifski_renderer(), fps = 3, end_pause = 2 )

# source_crs = st_crs("NAD83")

# shape <- read_sf(dsn = "nyc_election_districts/nyc_election_districts.shp")
# nyc <- vector()
# for (i in 1:nrow(shape)) {
#
#   nyc[i] <- sf::st_is_valid(shape$geometry[i])
#
# }
#
# shape <- cbind(shape, nyc)
#
# shape <- shape |>
#   filter(nyc == TRUE)

#
# X2006 |>
#   select(xcoord, ycoord) |>
#   mutate(across(.fns = as.numeric)) |>
#   sample_n(500) |>
#   na.omit() |>
#   as.matrix() |>
#   st_multipoint() |>
#   ggplot() +
#   geom_sf() +
#   geom_sf(data = shape$geometry)
#
# plot(shape$geometry)

## Arrest Data ####

points_arrest_ll <-
  data_arrest |>
  select(Longitude, Latitude) |>
  na.omit() |>
  as.matrix() |>
  st_multipoint() |>
  st_sfc(crs = st_crs("WGS84"))

arrests_2006_ll <-
  data_arrest |>
  filter(ARREST_DATE < "2007-01-01") |>
  select(Longitude, Latitude) |>
  na.omit() |>
  as.matrix() |>
  st_multipoint() |>
  st_sfc(crs = st_crs("WGS84"))
  
# arrests_2006_ll |>
#   ggplot() +
#   geom_sf(color = "blue", alpha = 0.5) +
#   geom_sf(data = points_sqf_2006_ll, color = "red", alpha = 0.5) + 
#   geom_sf(data = nyc_shape_ll)
# # theme_void()

ggplot(nyc_shape_ll) +
  geom_sf(aes(fill = BoroName)) +
  geom_sf(data = arrests_2006_ll, color = "red", alpha = 0.3) + 
  # geom_sf(data = points_sqf_2006_ll, color = "blue", alpha = 0.3) +
  # theme_map() +
  scale_fill_brewer(palette = 6) +
  labs(fill = "Boroughs") +
  coord_sf(crs = "WGS84") +
  theme(legend.position = "bottom")
# ggplotly()

## WGS 84 = ll
## NAD 83 = EPSG:2263
## http://projfinder.com/

## look into 2009-2011 ####

points_sqf_2009_ll <- 
  data_sqf |>
  filter(year == 2009) |>
  sample_frac(0.002) |>
  mutate(across(.cols = ends_with("coord"),.fns = as.numeric),
         geometry = st_multipoint(as.matrix(cbind(xcoord, ycoord)))
  ) |>
  # select(geometry) |>
  st_as_sf(coords = c("xcoord","ycoord"), crs = st_crs("EPSG:2263")) |>
  st_transform(crs = st_crs("WGS84"))

points_arrests_2009_ll <-
  data_arrest |>
  filter(ARREST_DATE < paste0(2009+1,"-01-01"),
         Latitude < 45) |>
  select(Longitude, Latitude) |>
  sample_frac(0.002) |>
  na.omit() |>
  as.matrix() |>
  st_multipoint() |>
  st_sfc(crs = st_crs("WGS84"))

ggplot(nyc_shape_ll) +
  geom_sf(aes(fill = BoroName)) +
  geom_sf(data = points_arrests_2009_ll, color = "red", alpha = 0.2) + 
  geom_sf(data = points_sqf_2009_ll, color = "blue", alpha = 0.2) +
  # theme_map() +
  scale_fill_brewer(palette = 6) +
  labs(fill = "Boroughs", title = paste(2009)) +
  coord_sf(crs = "WGS84") +
  theme(legend.position = "none")

## plot all years ####

plot_police_year <- function(year = 2006) {
  
  year_i <- year
  
  points_sqf_ll <- 
    data_sqf |>
    filter(year == year_i) |>
    sample_frac(0.0025) |>
    mutate(across(.cols = ends_with("coord"),.fns = as.numeric),
           geometry = st_multipoint(as.matrix(cbind(xcoord, ycoord)))
    ) |>
    # select(geometry) |>
    st_as_sf(coords = c("xcoord","ycoord"), crs = st_crs("EPSG:2263")) |>
    st_transform(crs = st_crs("WGS84"))
  
  arrests_ll <-
    data_arrest |>
    filter(ARREST_DATE < paste0(year_i+1,"-01-01"),
           Latitude < 41) |>
    select(Longitude, Latitude) |>
    sample_frac(0.0025) |>
    na.omit() |>
    as.matrix() |>
    st_multipoint() |>
    st_sfc(crs = st_crs("WGS84"))
  
  subtitle <- paste0('Only 0.0025% of cases are displayed. <br> <span style="color:red;"> Arrests ', '&nbsp;(N = ', nrow(arrests_ll[[1]]),')</span>,  <span style="color:blue;"> SQFs (N = ', 
                     nrow(points_sqf_ll),')</span>')
  
  ggplot() +
    geom_sf(data = nyc_shape_ll, aes(fill = "white")) +
    geom_sf(data = arrests_ll, color = "red", alpha = 0.125) + 
    geom_sf(data = points_sqf_ll, color = "blue", alpha = 0.125) +
    # theme_map() +
    theme_minimal() +
    scale_fill_brewer(palette = 6) +
    labs(fill = "Boroughs", title = paste(year_i)) +
    theme(legend.position = "none") +
    labs(subtitle = subtitle) +
    # coord_sf(crs = "WGS84") +
    theme(plot.subtitle = element_markdown(hjust = 0, size = 8))
}



plot_years <-
  plot_grid(
    plot_police_year(2006),
    plot_police_year(2007),
    plot_police_year(2008),
    plot_police_year(2009),
    plot_police_year(2010),
    plot_police_year(2011)
    )
plot_years


## Arrests over time ####

plot_arrests_years <-
  data_arrest |>
  transmute(year = dmy(paste0("01-", format(ARREST_DATE, "%m-%y")))) |>
  ggplot(aes(x = year)) +
  geom_line(stat = "count") +
  scale_y_continuous(labels = \(x){format(x, scientific = FALSE)}, limits = c(0, NA)) +
  labs(x = "Month", y = "Number of Arrests") +
  theme_minimal()

plot_arrests_years


data_arrest_year <-
  data_arrest |>
  transmute(year = dmy(paste0("01-", format(ARREST_DATE, "%m-%y")))) |>
  table() |>
  as.data.frame() |>
  mutate(type = "Arrests")

data_sqf_year <-
  data_sqf |>
  transmute(year = dmy(paste0("01-", format(datestop, "%m-%y")))) |>
  table() |>
  as.data.frame() |>
  mutate(type = "SQFs")


plot_arrests_sqf_years <-
  rbind(data_arrest_year, data_sqf_year) |>
  as.data.frame() |>
  mutate(year = as.Date(year),
         type = as.factor(type)) |>
  filter(year >= "2006-01-01") |>
  ggplot(aes(x = year, y = Freq, color = type)) +
  geom_line(stat = "identity",) +
  geom_smooth(formula = y~x,
              method = "loess",
              fill = "lightgray") +
  geom_point() +
  scale_y_continuous(labels = \(x){format(x, scientific = FALSE)}, limits = c(0, NA)) +
  labs(x = "months of years 2006-2011", y = "number of arrests/SQFs") +
  scale_color_discrete(name = element_blank()) +
  theme_bw()

plot_arrests_sqf_years

## Correlation ####
arrests_2006 <-
  data_arrest |>
  filter(ARREST_DATE < "2007-01-01") |>
  st_as_sf(coords = c("Latitude","Longitude"), crs = st_crs("EPSG:2263")) |>
  st_transform(crs = 6345) |>
  as.data.frame()

# g3 <- st_polygon( list(
#   rbind(
#     c(-74.316523, 40.486110),
#     c(-73.678308, 40.486110),
#     c(-73.678308, 40.932913),
#     c(-74.316523, 40.932913),
#     c(-74.316523, 40.486110) )
# ) )

ny_window <-
  nyc_zip_shape_ll |>
  st_simplify() |>
  st_union() |>
  st_transform(crs = 6345) |>
  as_Spatial() |>
  as.owin()
     
data_arrest_ppp <- ppp(x = arrests_2006$Longitude, y = arrests_2006$Latitude, window = ny_window)
quadcount_arrests_2006 <- quadratcount(data_arrest_ppp, nx=100, ny=100)

# plot(quadcount_arrests_2006)
# points(data_arrest_ppp, pch=16, col=grey(.5, .5))

# plot(density.ppp(data_arrest_ppp,))

sqf_2006 <- 
  data_sqf |>
  filter(year == 2006) |>
  mutate(across(.cols = ends_with("coord"),.fns = as.numeric),
         geometry = st_multipoint(as.matrix(cbind(xcoord, ycoord)))
  ) |>
  st_as_sf(coords = c("xcoord","ycoord"), crs = st_crs("EPSG:2263")) |>
  st_transform(crs = 6345) |>
  as.data.frame()

data_sqf_ppp <- ppp(x = sqf_2006$X, y = sqf_2006$Y, window = ny_window)
quadcount_sqf_2006 <- quadratcount(data_sqf_ppp, nx=100, ny=100)


# plot(quadcount_sqf_2006)

df_quadcount_sqf_2006 <- as.data.frame(quadcount_sqf_2006) |>
  rename(Freq.sqf = Freq)

df_quadcount_arrests_2006 <- as.data.frame(quadcount_arrests_2006) |>
  rename(Freq.arrests = Freq)

df_quadcounts_2006 <-
  left_join(df_quadcount_sqf_2006, df_quadcount_arrests_2006) |>
  filter(Freq.arrests > 0 | Freq.sqf > 0)

cor.test(df_quadcounts_2006$Freq.sqf, df_quadcounts_2006$Freq.arrests)

quadcount_dim <- function(quadcount) {
  coords <- quadcount |>
    as.data.frame() |>
    filter(row_number() == 1) |>
    transmute(y1 = str_extract(y, "[0-9]+.[0-9]+"),
           y2 = str_extract(y, ",[0-9]+.[0-9]+"),
           y2 = str_extract(y2, "[0-9]+.[0-9]+"),
           x1 = str_extract(x, "-[0-9]+.[0-9]+"),
           x2 = str_extract(x, ",-[0-9]+.[0-9]+"),
           x2 = str_extract(x2, "-[0-9]+.[0-9]+")) |>
    mutate(across(.fns = as.numeric))
  
  lengths <- spDists(x = matrix(c(coords$y1, coords$x1, coords$y2, coords$x2), nrow = 2, ncol = 2, byrow = TRUE), y = matrix(c(coords$y2, coords$x1), nrow = 1, ncol = 2),
                     longlat = TRUE) * 1000
  return(lengths)
}

quadcount_dim(quadcount_arrests_2006)

## Weitere Tests ####

Kest_arrest <- Kest(data_arrest_ppp)
plot.fv(Kest_arrest)

match_ppp <- pppmatching(X = data_arrest_ppp, Y = data_sqf_ppp)

## Zip Codes and Avg. Income ####

shape_zip_income_ll <-
  left_join(nyc_zip_shape_ll, data_income_zipcode, by = c("ZIPCODE" = "Zip Code")) |>
  st_as_sf() |>
  st_simplify()

g <-
  ggplot() +
  geom_sf(data = shape_zip_income_ll, aes(geometry = geometry, fill = Income)) +
  scale_fill_viridis(direction = -1) +
  coord_sf(crs = "WGS84") +
  theme_bw()
ggplotly(g)

## Buffer and Arrests in Buffer +-5 days ####

buffer_sqf <- st_buffer(points_sqf_2006_ll, dist = 1000)
buffer_sqf <- filter(buffer_sqf, row_number(buffer_sqf) == 2)

ggplot() +
  geom_sf(data = nyc_zip_shape_ll) +
  geom_sf(data = buffer_sqf, aes(geometry = geometry), color = "red") 

int_2006 <- st_intersection(buffer_sqf, arrests_2006_ll)

ggplot() +
  geom_sf(data = nyc_zip_shape_ll) +
  geom_sf(data = buffer_sqf, aes(geometry = geometry), color = "red") +
  geom_sf(data = int_2006)

st_coordinates(int_2006) |>
  nrow()


this_sqf <- points_sqf_2006_ll |>
  filter(row_number(points_sqf_2006_ll) == 2)

day <- this_sqf$datestop


