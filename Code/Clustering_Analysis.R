# source("Code/Makefile.R")

if (file.exists("Data/Clean/Ks.R")) {
  load("Data/Clean/Ks.R")
} else if(askYesNo(msg = "This code takes a very long time to compute. It was run on the Scientific Computation Server of the University. Are you sure you want to run this code now?")){
  
    nyc_shape_owin <-
      nyc_zip_shape_ll |>
      st_transform(crs = 6345) |>
      st_simplify(dTolerance = 5) |>
      as.data.frame()

    n <- length(2006:2011)
    Ks <- vector("list", n)

    for (year in 2006:2011) {
      data_arrest_test <-
        data_arrest |>
        filter(ARREST_DATE < paste0(year + 1, "-01-01")) |>
        select(Longitude, Latitude) |>
        na.omit() |>
        sample_n(10000) |>
        as.matrix() |>
        st_multipoint() |>
        st_sfc(crs = st_crs("WGS84")) |>
        st_transform(crs = 6345) |>
        st_coordinates() |>
        as.data.frame() |>
        select(-L1)

      arrests.ppp <- as.ppp(data_arrest_test, W = nyc_shape_owin$geometry)

      r <- seq(0, 5000, length.out = 20)

      K1 <- Kest(arrests.ppp, r = r, correction = "Ripley")
      # plot(K1)

      message(paste(Sys.time(), "K1 for year", year, "done"))

      data_sqf_test <-
        data_sqf |>
        filter(year == as.numeric(year)) |>
        select(xcoord, ycoord) |>
        na.omit() |>
        sample_n(10000) |>
        as.matrix() |>
        st_multipoint() |>
        st_sfc(crs = st_crs("EPSG:2263")) |>
        st_transform(crs = 6345) |>
        st_coordinates() |>
        as.data.frame() |>
        select(-L1)

      sqf.ppp <- as.ppp(data_sqf_test, W = nyc_shape_owin$geometry)


      K2 <- Kest(sqf.ppp, r = r, correction = "Ripley")
      # plot(K2$iso)

      message(paste(Sys.time(), "K2 for year", year, "done"))

      Ks[[year - 2005]] <- list(
        K1,
        K2
      )

      names(Ks[[year - 2005]]) <- c(paste(year, "Arrests"), paste(year, "SQFs"))
    }

    save(Ks, file = "Data/Clean/Ks.R")
  
}

# gg <- list()
# 
r <- seq(0, 5000, length.out = 20)
# 
# for (i in 1:6) {
#   K1 <- Ks[[i]][[1]]
#   K2 <- Ks[[i]][[2]]
#   year <- 2005 + i
# 
#   gg[[i]] <-
#     ggplot(mapping = aes(x = r)) +
#     geom_line(mapping = aes(y = K1$iso, color = "Arrests")) +
#     geom_line(mapping = aes(y = K1$theo, color = "pois."), linetype = "dashed") +
#     geom_line(mapping = aes(y = K2$iso, color = "SQFs")) +
#     scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
#     xlab("Distance in meters") +
#     ylab("K-hat") +
#     scale_color_manual(values = c("Arrests" = "blue", "SQFs" = "red", "pois." = "black"), name = "") +
#     theme_bw() +
#     labs(title = paste0("Cluster Analysis ", year)) +
#     theme(legend.position = c(0.3, 0.8), legend.title = element_blank())
# }
# 
# clustering_plot <-
#   plot_grid(
#   gg[[1]],
#   gg[[2]],
#   gg[[3]],
#   gg[[4]],
#   gg[[5]],
#   gg[[6]]
# )

if(file.exists("Data/Clean/Kcross_save.R")){
  load("Data/Clean/Kcross_save.R")
} else if(askYesNo(msg = "This code takes a very long time to compute. It was run on the Scientific Computation Server of the University. Are you sure you want to run this code now?")){
  marked.ppp <- vector("list", 6)
  Kcross <- vector("list", 6)
  
  message(paste0(Sys.time(), ": Beginning for loop"))
  for (year in 2006:2011) {
    data_arrest_test <-
      data_arrest |>
      filter(ARREST_DATE < paste0(year + 1, "-01-01")) |>
      select(Longitude, Latitude) |>
      na.omit() |>
      sample_n(10000) |>
      as.matrix() |>
      st_multipoint() |>
      st_sfc(crs = st_crs("WGS84")) |>
      st_transform(crs = 6345) |>
      st_coordinates() |>
      as.data.frame() |>
      mutate(marks = "arrest") |>
      select(-L1)
    
    data_sqf_test <-
      data_sqf |>
      filter(year == as.numeric(year)) |>
      select(xcoord, ycoord) |>
      na.omit() |>
      sample_n(10000) |>
      as.matrix() |>
      st_multipoint() |>
      st_sfc(crs = st_crs("EPSG:2263")) |>
      st_transform(crs = 6345) |>
      st_coordinates() |>
      as.data.frame() |>
      mutate(marks = "sqf") |>
      select(-L1)
    
    data_marked_test <- rbind(data_arrest_test, data_sqf_test)
    
    marked.ppp[[year - 2005]] <-
      ppp(
        x = data_marked_test$X,
        y = data_marked_test$Y,
        window = as.owin(nyc_shape_owin$geometry),
        marks = as.factor(data_marked_test$marks)
      )
    
    Kcross[[year - 2005]] <-
      Kcross(
        X = marked.ppp[[year - 2005]],
        i = "sqf",
        j = "arrest",
        r = r,
        correction = "Ripley"
      )
    
    save(marked.ppp, Kcross, file = "Data/Clean/Kcross_save.R")
    message(paste0(Sys.time(), ": Saved Kcross of year", year))
    
  }
}
