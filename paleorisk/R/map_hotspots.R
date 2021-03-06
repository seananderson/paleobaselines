#' Make global intrinsic extinction probability hotspot maps
#'
#' This function plots overall intrinsic extinction probability and can add
#' hotspots of contemporary risk.
#'
#' @param er_dat A data frame with the data to plot. Should have
#'   \code{\link[ggplot2]{fortify}} data from a
#'   \code{\link[maptools]{readShapePoly}} version of the EcoRegion data. Also
#'   should have columns named \code{Halpern}, \code{Burrows},
#'   \code{log_OBIS_records}, \code{N.gen}, \code{mean.ext}. \code{mean.ext}
#'   should contain *log*-transformed extinction probability. (note that you can
#'   chose to plot a different column by specifying \code{plot_column}
#' @param min_prov_genera The minimum contemporary genera per province required
#'   to plot.
#' @param hotspot_thresh Percentile threshold to declare a contemporary hotspot.
#' @param col_limits \code{NULL} to calculate colour limits internally. Set to
#'   a numeric vector of length 2 to set lower and upper limits for the colour
#'   scale. All values outside this range will be compressed to the min and max
#'   values.
#' @param col_pal The colour palette to plot with. Should be approximately a
#'   vector of length 9.
#' @param plot_column The name of the column to plot.
#' @param hotspots Show the contemporary hotspots?
#' @param add_legend Add a colour legend? You may need to increase the right
#' margin in \code{par} to see this.
#' @param at Locations for ticks on the legend.
#' @param at.labels Labels for colour legend ticks.
#' @export

map_hotspots <- function(er_dat, min_prov_genera = 50, hotspot_thresh = 0.8,
  col_pal = RColorBrewer::brewer.pal(9, "YlOrRd"), plot_column = "mean.ext",
  hotspots = TRUE, col_limits = NULL,
  add_legend = FALSE, at = c(-1, 1), at.labels = c(-1, 1)) {

  if(add_legend) {
    lo <- t(matrix(c(rep(1, 45), 3, 2, 4, 4, 4, 4, 4)))
    layout(lo)
  }

  if(!is.null(col_limits)) {
    col_limits <- log(col_limits)
  }

  er.df.all <- er_dat
  # Create a box/oval to outline the map:
  N <- 100
  x.s <- seq(-180, 180, length.out = N)
  y.s <- seq(-90, 90, length.out = N)
  square <- data.frame(x = c(x.s, rep(x.s[N], N), rev(x.s), rep(x.s[1],
    N)), y = c(rep(y.s[1], N), y.s, rep(y.s[N], N), rev(y.s)))
  oval <- mapproj::mapproject(list(x = square$x, y = square$y),
    proj = "mollweide")

  plot_col_n <- (1:ncol(er.df.all))[names(er.df.all) %in% plot_column]

  er.df.all$ext.plot <- er.df.all[, plot_col_n]
  halp.thresh <- as.numeric(quantile(er.df.all$Halpern, probs = hotspot_thresh,
    na.rm = TRUE))
  bur.thresh <- as.numeric(quantile(er.df.all$Burrows, probs = hotspot_thresh,
    na.rm = TRUE))
  er.df <- transform(er.df.all,
    halp.hot = ifelse(Halpern >= halp.thresh, TRUE, FALSE),
    bur.hot = ifelse(Burrows >= bur.thresh, TRUE, FALSE))

  # remove provinces with fewer than a specified number of genera:
  er.df <- droplevels(subset(er.df,er.df$N.gen >= min_prov_genera))

  er.df <- transform(er.df, base.col = ifelse(bur.hot == FALSE & halp.hot ==
      FALSE, "grey85", NA))

  # drop black sea - very low outlier that distorts colour
  #black_sea_remove <- FALSE
  #if(nrow(subset(er.df, er.df$PROV_CODE == 7))) {
    #black_sea_remove <- TRUE
    #black_sea <- subset(er.df, er.df$PROV_CODE == 7)
    #black_sea$col.pal.ext.plot <- "grey64"
  #}
  er.df <- subset(er.df,er.df$PROV_CODE != 7)

  if(is.null(col_limits)) {
  er.df$col.pal.ext.plot <- col_pal[findInterval(er.df$ext.plot,
    seq(min(er.df$ext.plot)-0.00001, max(er.df$ext.plot) + 0.00001,
      length.out = 10))]
  } else {
  # compress those outside our limits to our limits:
  er.df$ext.plot[er.df$ext.plot <= col_limits[1]] <- col_limits[1] + 0.00001
  er.df$ext.plot[er.df$ext.plot >= col_limits[2]] <- col_limits[2] - 0.00001
  # and find colours:
  er.df$col.pal.ext.plot <- col_pal[findInterval(er.df$ext.plot,
    seq(col_limits[1], col_limits[2], length.out = 10))]
  }

  #if(black_sea_remove) {
    #er.df <- rbind(er.df, black_sea)
  #}
  er.df.mol <- mapproj::mapproject(list(x = er.df$long, y = er.df$lat))
  er.df.m <- er.df
  er.df.m$x <- er.df.mol$x
  er.df.m$y <- er.df.mol$y

  land.mol <- mapproj::mapproject(list(x = land$long, y = land$lat))
  land.m <- land
  land.m$x <- land.mol$x
  land.m$y <- land.mol$y

  maps::map("world", proj = "mollweide",  col = "grey69", fill = TRUE, lwd = 0.9,
    myborder = c(0, 0), wrap = FALSE, resolution = 0, xlim = c(-178, 178),
    plot = TRUE, border = "grey69", type = "n")

  # grey background:
  polygon(oval, col = "grey92", border = NA, lwd = 1.2)

  # extinction risk:
  plyr::d_ply(er.df.m, "group", function(x) {
    with(x, polygon(x, y, border = col.pal.ext.plot, col = col.pal.ext.plot,
      lwd = .8))
  })

  if(hotspots) {
    # Plot province borders only; Halpern et al.
    plyr::d_ply(er.df.m, "PROV_CODE", function(x) {
      PROV <- unique(x$PROV_CODE)
      prov.dat <- ggplot2::fortify(prov_SpatialPolygons[[PROV]])
      prov.dat.m <- mapproj::mapproject(list(x = prov.dat$long, y = prov.dat$lat))
      prov.dat$x <- prov.dat.m$x
      prov.dat$y <- prov.dat.m$y
      halp.hot <- unique(subset(er.df, PROV_CODE == PROV)$halp.hot)
      if(length(halp.hot) > 1) warnings("More than one Halpern value per province")
      plyr::d_ply(prov.dat, "group", function(z) {
        with(z, polygon(x, y, border = c(NA, "grey20")[halp.hot+1], lwd = 2.2))
      })
    })


    # Burrows et al.
    plyr::d_ply(er.df.m, "group", function(x) {
      with(x, polygon(x, y, border = NA, col = c(NA, "grey20")[bur.hot+1],
        lwd = 1.5, density = c(NA, 23)[bur.hot+1], angle = 45))})
  }

  #maps::map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey64", fill =
      #TRUE, lwd = .95, myborder = c(0, 0), border = "grey64", wrap =
      #FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)


  if(hotspots) {
    par(xpd = NA)
    legend(1.7, -.75, fill = c("white", "grey50"), col = c(
      "black", "black"), legend = c("Human impact",
        "Climate velocity"), bty = "n", density = c(NA, 25),
      angle = c(NA, 45), border = c("black", NA), cex = 1.4,
      text.col = "grey30")
  }

  # again to make it clean:
  maps::map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey64", fill =
      TRUE, lwd = .95, myborder = c(0, 0), border = "grey64", wrap =
      FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)
  # patches, from the package data:
  with(ant_patch, polygon(x, y, col = "grey64", border = FALSE))
  with(russia_patch, polygon(x, y, col = "white", border = FALSE))

  # outer oval:
  lines(oval, col = "grey64", lwd = 2.8)

 if(add_legend) {
   #at <- range(er.df$ext.plot)

   if(is.null(col_limits)) {
     limits <- range(er.df$ext.plot)
     col.regions <- with(er.df, seq(min(ext.plot),
         max(ext.plot), length.out = length(col_pal)+1))
   } else {
     limits <- col_limits
     col.regions <- with(er.df, seq(col_limits[1],
         col_limits[2], length.out = length(col_pal)+1))
   }

   col_box_key(col.pal = col_pal, limits = limits, xpos = 0.1,
     width = 0.01, col.regions = col.regions, bg = "grey85", border.col =
     "grey60", at = at, at.labels = at.labels, add = FALSE, limit_pad = 0.55)
 }
}
