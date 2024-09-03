read_geo <- function(id, offset = NULL, file_type = "csv") {

  message("Finding ", file_type, " file for bird ", id)

  f <- fs::dir_ls("Data/Raw", recurse = TRUE) |>
    str_subset(regex(id, ignore_case = TRUE)) |>
    str_subset(paste0("\\.", file_type, "$"))

  if(length(f) > 1) {
    stop("Found more than one csv file for this individual:\n",
         paste("-", f, collapse = "\n"), call. = FALSE)
  }

  message("Loading data from: ", f)

  # Read data
  raw <- read_csv(f, show_col_types = FALSE, progress = FALSE) |>
    rename("Date" = "datetime", "Light" = "light") |>
    mutate(Date = ymd_hms(Date, tz = "UTC"))

  # if(max(raw$Light, na.rm = TRUE) > 100) {
  #   message("Log transforming light levels")
  #   # Log-transform because these record the full light spectrum
  #   # https://geolocationmanual.vogelwarte.ch/loadingData.html
  #   raw <- mutate(raw, Light_orig = Light, Light = log10(Light))
  # }

  raw
}

check_date_range <- function(raw) {

  r <- raw |>
    slice_sample(prop = 0.05) |>
    arrange(Date) |>
    mutate(Date = as_date(Date)) |>
    mutate(daily_max = max(Light), .by = "Date")

  d1 <- arrange(r, desc(Date)) |>
    filter(daily_max < quantile(Light, 0.1)) |>
    slice(1) |>
    pull(Date)

  d2 <- arrange(r, Date) |>
    filter(daily_max < quantile(Light, 0.1)) |>
    slice(1) |>
    pull(Date)

  if(length(d1) == 0) d1 <- NULL
  if(length(d2) == 0) d2 <- NULL


  if(!is.null(d1) && !is.null(d2) && d1 >= d2) {
    if(d1 > mean(raw$Date)) {
      d1 <- NULL
    } else if (d2 < mean(raw$Date)) d2 <- NULL
    t <- "Full date range with proposed cutoff dates"
  } else if(is.null(d1) && is.null(d2)) {
    t <- "Full date range (no proposed cutoff dates)"
  }

  g <- ggplot(data = r, aes(x = Date, y = Light))
  if(!is.null(d1)) g <- g + geom_vline(xintercept = d1, colour = "forestgreen", linewidth = 2)
  if(!is.null(d2)) g <- g + geom_vline(xintercept = d2, colour = "orange", linewidth = 2)
  g <- g + geom_point() + labs(title = t)

  # Look at the end of the range
  if(!is.null(d1)) {
    g1 <- ggplot(data = raw, aes(x = Date, y = Light)) +
      geom_point() +
      coord_cartesian(xlim = c(as_datetime(d1) - days(1), as_datetime(d1) + days(2))) +
      geom_vline(xintercept = as_datetime(d1), colour = "forestgreen", linewidth = 2) +
      labs(title = "First date")
  }

  if(!is.null(d2)) {
    g2 <- ggplot(data = raw, aes(x = Date, y = Light)) +
      geom_point() +
      coord_cartesian(xlim = c(as_datetime(d2) - days(1), as_datetime(d2) + days(2))) +
      geom_vline(xintercept = as_datetime(d2), colour = "orange", linewidth = 2) +
      labs(title = "Last date")
  }

  if(!is.null(d1) & !is.null(d2)) {
    g <- g / (g1 + g2)
  } else if(!is.null(d1)) {
    g <- g / g1
  } else if(!is.null(d2)) {
    g <- g / g2
  }
  print(g)

  if(is.null(d1)) d1 <- "Start of date range"
  if(is.null(d2)) d2 <- "End of date range"
  message("The proposed cutoff dates are: ", d1, " to ", d2)
}

filter_date <- function(raw, first = NULL, last = NULL) {
  if(is.null(first)) first <- min(raw$Date)
  if(is.null(last)) last <- max(raw$Date)
  filter(raw, Date >= first, Date <= last)
}


get_offset <- function(location) {

  tz <- lutz::tz_lookup_coords(location[["latitude"]], location[["longitude"]], warn = FALSE)

  # The offset we expect from UTC is the number of hours difference there is between
  # our location and the time in the UTC timezone.
  test_date <- as_datetime("2020-07-15", tz = "UTC")
  24 + (test_date - force_tz(test_date, tz))  # Difference between local and UTC
}

light_plot <- function(d, start_only = FALSE, twl = NULL, max_light = 100) {
  loc <- getOption("geo_location")

  offset <- get_offset(loc) |>
    as.numeric()

  light_lim <- c(min(d$Light), max(d$Light))
  if(!is.null(max_light)) light_lim[2] <- max_light

  if(start_only) {
    start_date <- getOption("geo_start_date")
    d <- filter(d, yday(Date) <= start_date, year(Date) == year(Date)[1])
  }

  lightImage(tagdata = d,
             offset = offset,
             zlim = light_lim)

  tsimageDeploymentLines(
    d$Date, lon = loc[["longitude"]], lat = loc[["latitude"]],
    offset = offset, lwd = 3, col = adjustcolor("orange", alpha.f = 0.5))

  if(!is.null(twl)) {
    tsimagePoints(twl$Twilight ,
                  offset = offset,
                  pch = 19, cex = 1.2,
                  col = ifelse(twl$Rise, "firebrick", "cornflowerblue"))
  }
}

check_adjust <- function(d, adjust = 90) {
  cols <- length(adjust) + 1
  p0 <- par(mfrow = c(cols, 1), mar = c(2,1,2,1))
  light_plot(d, start_only = TRUE)
  title("No adjustment")

  for(a in adjust) {
    d1 <- mutate(d, Date = Date + minutes(a))
    light_plot(d1, start_only = TRUE)
    title(paste0(a, "-min adjustment"))
  }
  par(p0)
}

apply_adjust <- function(d, adjust = NULL) {
  mutate(d, Date = Date + minutes(adjust))
}

nighttime_light <- function(d, q = 0.5, days = 5) {
  night <- quantile(d$Light, q)

  # Check that it looks good
  dd <- d |>
    arrange(Date) |>
    mutate(month = month(Date),
           Night = Light <= .env$night) |>
    mutate(season = case_match(
      month,
      7 ~ "Breeding",
      11 ~ "Post-Breeding Migration",
      1 ~ "Non-Breeding",
      4 ~ "Pre-Breeding Migration"),
      season = factor(season, levels = unique(season))) |>
    filter(month %in% c(7, 11, 1, 4), day(Date) < days) |>
    filter(year(Date) == min(year(Date)), .by = "season")

  g <- ggplot(data = dd, aes(x = Date, y = Light)) +
    theme_bw() +
    geom_line() +
    geom_point(size = 2, aes(colour = Night)) +
    geom_hline(yintercept = night) +
    scale_colour_viridis_d(option = "inferno", end = 0.8) +
    coord_cartesian(ylim = c(min(dd$Light), max(dd$Light)/2)) +
    facet_wrap(~season, ncol = 1, scales = "free_x") +
    labs(caption = paste0("Using threshold of ", round(night, 4)))
  print(g)

  night
}


twl <- function(d, threshold) {
  i <- d$Date
  i <- i[hour(i) == 7]
  i <- floor_date(i, unit = "hour")
  i <- unique(i)

  t <- findTwilights(d, threshold = threshold, include = i)

  light_plot(d, twl = t)

  t
}

twl_edit <- function(t, outlier = 45, stationary = 15) {

  offset <- get_offset(getOption("geo_location")) |>
    as.numeric()

  t_adjust <- twilightEdit(
    twilights = t,
    offset = offset,
    outlier.mins = outlier,       # difference in mins
    stationary.mins = stationary, # are the other surrounding twilights within X mins of one another
    plot = TRUE)

  t_adjust[!t_adjust$Deleted,]
}

twl_smooth <- function(t, smoother = 1/6) {

  offset <- get_offset(getOption("geo_location")) |>
    as.numeric()

  t0 <- mutate(
    t,
    time = as.numeric(Twilight),
    Twilight_offset = Twilight - hours(offset),
    twl = hour(Twilight_offset) + minute(Twilight_offset)/60 + second(Twilight_offset)/60/60)

  # ggplot(data = t0, aes(x = as_date(Twilight), y = twl, colour = Rise)) +
  #   geom_point()

  t0$smooth[!t0$Rise] <- smooth(t0[!t0$Rise, c("time", "twl")], smoother)
  t0$smooth[t0$Rise] <- smooth(t0[t0$Rise, c("time", "twl")], smoother)

  t0 |>
    mutate(Twilight_orig = Twilight,
           Twilight = as_datetime(paste0(as_date(Twilight_offset), hms::hms(hours = smooth))),
           Twilight = Twilight + hours(offset),
           diff = as.numeric(difftime(Twilight, Twilight_orig, units = "hours"))) |>
    dplyr::select(Twilight, Rise, any_of(names(t)), Twilight_orig)
  #  filter(abs(diff) > 6) |>
}

smooth <- function(d, s) {
  smoother <- loess(d[,2] ~ d[,1], span = s)
  predict(smoother, d)
}

calibration <- function(d, t, dates) {

  loc <- getOption("geo_location")

  par0 <- par(mfrow = c(2, 1))
  light_plot(d, twl = t)
  dts <- as.POSIXct(c(dates[1], dates[2]), tz = "UTC")
  abline(v = dts, lwd = 2, lty = 2, col = "orange")

  d_calib <- dplyr::filter(t, Twilight  >= dates[1] & Twilight <= dates[2])

  calib <- thresholdCalibration(d_calib$Twilight, d_calib$Rise,
                                lon = loc[["longitude"]],
                                lat = loc[["latitude"]],
                                method = "gamma")
  par(par0)

  list("zenith" = calib[1], "zenith0" = calib[2], "alpha" = calib[3:4])
}

initial_path <- function(t, calib, tol = 0.15, first_known = TRUE, last_known = TRUE) {

  path <- thresholdPath(t$Twilight, t$Rise, zenith = calib[["zenith"]], tol = tol)

  # Steffi's workflow for visualizing
  coords <- data.frame(time = with_tz(path$time, "UTC"),
                       lon = path$x[, 1],
                       lat = path$x[, 2]) %>%
    mutate(year = year(time))

  path_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326) %>%
    mutate(equinox = time %within% interval("2020-09-11", "2020-09-30") |
             time %within% interval("2021-03-11", "2021-03-30"))

  path_lines_sf <- path_sf %>%
    group_by(year) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")

  g <- ggplot(data = path_sf) +
    geom_sf(data = north_america) +
    geom_sf(aes(colour = time)) +
    geom_sf(data = path_lines_sf) +
    scale_colour_viridis_c(transform = "time") +
    #facet_wrap(~year) +
    geom_sf(data = path_sf[1,], colour = "red") +
    geom_sf(data = st_sfc(st_point(geo_location), crs = 4326), colour = "blue") +
    labs(caption = "Blue dot is Kamloops; Red dot is first calculated location")

  print(g)

  coords$fixed <- FALSE
  if(first_known) coords$fixed[1:2] <- TRUE   # After deployment
  if(last_known) coords$fixed[(nrow(coords) - 1):nrow(coords)] <- TRUE   # Upon return

  # Update the lat/lon for those points
  coords$lat[coords$fixed] <- getOption("geo_location")[["latitude"]]
  coords$lon[coords$fixed] <- getOption("geo_location")[["longitude"]]

  coords
}

run_estelle_model <- function(t, coords, calib, beta = c(2.2, 0.08), iters = 1000, thin = 20) {

  # Setup
  matplot(0:100, dgamma(0:100, beta[1], beta[2]),
          type = "l", col = "orange", lty = 1,lwd = 2,ylab = "Density", xlab = "km/h")
  title(paste0("beta mean = ", beta[1], "; beta sd = ", beta[2]))

  # Model - Initial, relaxed model to get starting values
  x0 <- as.matrix(coords[, c("lon", "lat")])
  z0 <- trackMidpts(x0)

  model <- thresholdModel(twilight = t$Twilight,
                          rise = t$Rise,
                          twilight.model = "ModifiedGamma",
                          alpha = calib[["alpha"]],  # From calibration
                          beta = beta,    # From movement model
                          x0 = x0,
                          z0 = z0,
                          zenith = calib[["zenith0"]],
                          fixedx = coords$fixed)

  ## Error dist for each location
  proposal_x <- mvnorm(S = diag(c(0.0025, 0.0025)), n = nlocation(x0))
  proposal_z <- mvnorm(S = diag(c(0.0025, 0.0025)), n = nlocation(z0))

  fit <- estelleMetropolis(model, proposal_x, proposal_z, iters = iters, thin = thin)

  # Model - Short runs to tune proposal
  x0 <- chainLast(fit$x)
  z0 <- chainLast(fit$z)

  model <- thresholdModel(twilight = t$Twilight,
                          rise = t$Rise,
                          twilight.model = "Gamma",
                          alpha = calib[["alpha"]],
                          beta = beta,
                          x0 = x0,
                          z0 = z0,
                          zenith = calib[["zenith0"]],
                          fixedx = coords$fixed)

  proposal_x <- mvnorm(S = diag(c(0.005, 0.005)), n = nrow(t))
  proposal_z <- mvnorm(S = diag(c(0.005, 0.005)), n = nrow(t) - 1)

  for (k in 1:3) {
    fit <- estelleMetropolis(model, proposal_x, proposal_z, x0 = chainLast(fit$x),
                             z0 = chainLast(fit$z), iters = 300, thin = 20)

    proposal_x <- mvnorm(chainCov(fit$x), s = 0.2)
    proposal_z <- mvnorm(chainCov(fit$z), s = 0.2)
  }

  # Model - Final Run
  proposal_x <- mvnorm(chainCov(fit$x), s = 0.25)
  proposal_z <- mvnorm(chainCov(fit$z), s = 0.25)

  estelleMetropolis(model, proposal_x, proposal_z, x0 = chainLast(fit$x),
                    z0 = chainLast(fit$z), iters = iters, thin = thin)
}

check_estelle_model <- function(fit, coords) {
  p0 <- par(mfrow = c(2, 1), mar = c(3, 5, 2, 1) + 0.1)
  matplot(t(fit$x[[1]][!coords$fixed, 1, ]), type = "l", lty = 1, col = "dodgerblue", ylab = "Lon")
  matplot(t(fit$x[[1]][!coords$fixed, 2, ]), type = "l", lty = 1, col = "firebrick", ylab = "Lat")
  par(p0)
}


model_map_tiles <- function(fit, coords, min_prob = 1) {

  xlim <- range(coords$lon + c(-35, 35))
  ylim <- range(coords$lat + c(-15, 15))
  r <- raster::raster(nrows = 2 * diff(ylim), ncols = 2 * diff(xlim), xmn = xlim[1]-5,
                      xmx = xlim[2]+5, ymn = ylim[1]-5, ymx = ylim[2]+5, crs = 4326)
  s <- SGAT::slices(type = "intermediate", breaks = "week", mcmc = fit, grid = r)
  sk <- SGAT::slice(s, sliceIndices(s))

  # Convert to starts to sf for plotting
  sk_st <- stars::st_as_stars(sk) %>%
    st_as_sf() %>%
    mutate(layer = if_else(layer < min_prob, NA_real_, layer)) |> # If less than 1, don't plot
    st_transform(crs = 3347)

  ggplot(drop_na(sk_st, layer)) +
    theme_bw() +
      geom_sf(aes(fill = layer), colour = NA, na.rm = TRUE) +
      geom_sf(data = north_america, fill = NA, colour = "black", linewidth = 0.15) +
      scale_fill_viridis_c(name = "Probability", na.value = NA)
}

model_map_points <- function(fit) {

  sm <- locationSummary(fit$z, time = fit$model$time)
  points <- dplyr::select(sm, lon = "Lon.50%", lat = "Lat.50%", Lat.sd, Lon.sd, "Time1") %>%
    mutate(year = year(Time1),
           conf = case_when(Lat.sd > 6 | Lon.sd > 6 ~ "Low (SD > 6)",
                            Lat.sd > 2 | Lon.sd > 2 ~ "Med (SD 2-6)",
                            TRUE ~ "High (SD <= 2)")) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(crs = 3347)

  ggplot() +
    theme_bw() +
    geom_sf(data = north_america, fill = NA, colour = "black", linewidth = 0.15) +
    geom_sf(data = points, aes(alpha = conf, colour = Time1)) +
    scale_alpha_manual(name = "Confidence (roughly) \nbased on SD",
                       values = c("High (SD <= 2)" = 1, "Med (SD 2-6)" = 0.5, "Low (SD > 6)" = 0.1)) +
    scale_colour_viridis_c(name = "Time", trans = "time")
}

model_plot_coords <- function(fit, lon_lim = c(-150, -100), lat_lim = c(30, 60)) {

  sm <- locationSummary(fit$z, time = fit$model$time) |>
    pivot_longer(cols = -c("Time1", "Time2"), names_to = c("Type", "Stat"),
                 values_to = "Value", names_pattern = "(Lat|Lon)\\.(.+)") %>%
    pivot_wider(names_from = Stat, values_from = Value) |>
    rename("50th Percentile" = "50%")

  g1 <- ggplot(data = filter(sm, Type == "Lon"), aes(x = Time1, y = `50th Percentile`)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2) +
    geom_line() +
    geom_point(size = 1)
  g2 <- g1 %+% filter(sm, Type == "Lat") +
    coord_cartesian(ylim = lat_lim) +
    labs(title = "Latitude", x = "Time")

  g1 <- g1 + coord_cartesian(ylim = lon_lim) + labs(title = "Longitude")

  g1 / g2
}
