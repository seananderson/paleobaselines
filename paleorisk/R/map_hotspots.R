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
#'   should contain *log*-transformed extinction probability.
#' @param min_prov_genera The minimum contemporary genera per province required 
#'   to plot.
#' @param hotspot_thresh Percentile threshold to declare a contemporary hotspot.
#' @param col_pal The colour palette to plot with. Should be approximately a
#'   vector of length 9.
#' @param legend Show the legend?
#' @param hotspots Show the contemporary hotspots?
#' @export

map_hotspots <- function(er_dat, min_prov_genera = 100, hotspot_thresh = 0.8, 
  col_pal = RColorBrewer::brewer.pal(9, "YlOrRd"), legend = TRUE, 
  hotspots = TRUE) {
  
  er.df.all <- er_dat
  # Create a box/oval to outline the map:
  N <- 100
  x.s <- seq(-180, 180, length.out = N)
  y.s <- seq(-90, 90, length.out = N)
  square <- data.frame(x = c(x.s, rep(x.s[N], N), rev(x.s), rep(x.s[1],
    N)), y = c(rep(y.s[1], N), y.s, rep(y.s[N], N), rev(y.s)))
  oval <- mapproj::mapproject(list(x = square$x, y = square$y), 
    proj = "mollweide")
  
  er.df.all$ext.plot <- er.df.all$mean.ext
  halp.thresh <- as.numeric(quantile(er.df.all$Halpern, probs = hotspot_thresh,
    na.rm = TRUE))
  bur.thresh <- as.numeric(quantile(er.df.all$Burrows, probs = hotspot_thresh,
    na.rm = TRUE))
  er.df <- transform(er.df.all,
    halp.hot = ifelse(Halpern >= halp.thresh, TRUE, FALSE),
    bur.hot = ifelse(Burrows >= bur.thresh, TRUE, FALSE))
  
  # remove provinces with fewer than a specified number of genera:
  er.df <- gdata::drop.levels(subset(er.df,er.df$N.gen >= min_prov_genera))
  
  er.df <- transform(er.df, base.col = ifelse(bur.hot == FALSE & halp.hot ==
      FALSE, "grey85", NA))
  
  er.df$col.pal.ext.plot <- col_pal[findInterval(er.df$ext.plot,
    seq(min(er.df$ext.plot)-0.00001, max(er.df$ext.plot) + 0.00001,
      length.out = 10))]
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
        lwd = 1.5, density = c(NA, 23)[bur.hot+1], angle = 45))
    })
  }
  
  maps::map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey64", fill =
      TRUE, lwd = .95, myborder = c(0, 0), border = "grey64", wrap =
      FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)
  
  # patches, from the package data:
  with(ant_patch, polygon(x, y, col = "grey64", border = FALSE))
  with(russia_patch, polygon(x, y, col = "white", border = FALSE))
  
  # outer oval:
  lines(oval, col = "grey64", lwd = 2.8)
  
  if(legend) {
    par(xpd = NA)
    legend(1.2, -.75, fill = c(col_pal[6],NA, "grey50"), col = c(col_pal[6],
      "black", "black"), legend = c("Intrinsic risk", "Human impact",
        "Climate velocity"), bty = "n", density = c(NA, NA, 25),
      angle = c(NA, NA, 45), border = c(NA, "black", NA), cex = 0.9)
  }
}
