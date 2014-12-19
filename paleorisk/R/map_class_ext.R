#' Make class-level extinction probability maps
#'
#' This function makes a multipanel figure showing extinction probability (or
#' other measure) by phylogenetic class.
#'
#' @param er_dat A data frame with the data to plot. Should have
#'   \code{\link[ggplot2]{fortify}} data from a
#'   \code{\link[maptools]{readShapePoly}} version of the EcoRegion data. Also
#'   should have columns \code{N.gen}, \code{mean.ext}. \code{mean.ext} should
#'   contain *log*-transformed extinction probability.
#' @param min_prov_genera The minimum contemporary genera per province *per
#' class* required to plot.
#' @param col_pal The colour palette to plot with. Should be approximately a
#'   vector of length 9.
#' @param plot_column A character value giving the name of the column to plot.
#' @param plot_order A vector of character giving the order of the class panels
#' to plot. Should contain \code{c("Mammalia", "Elasmobranchii", "Anthozoa",
#' "Gastropoda","Bivalvia", "Decapoda", "Echinoidea")}.
#' @param yticks Locations of y-axis labels.
#' @param ylabel Text to label the colour axis with.
#' @param col_range If \code{TRUE} the colour range will be set
#'   (on a multiplicative scale) to the same for each panel. This can
#'   help to avoid overemphasizing small differences in some panels. If set to
#'   \code{FALSE} then each panel will have a colour scale that range from the
#'   minimum to maximum value for a given panel.
#' @param exact_limits Vector of length 2 giving the exact colour limits.
#'   Ignored if \code{NULL}
#' @param log_yticks Should the log tick values be log transformed when placing
#'   them?
#' @param fixed_range Should the colour range be fixed through all panels?
#' @param picture_files A vector of file paths containing silhouettes to be
#'   added to the panels. Should be in a format for \code{readPicture} in the
#'   \pkg{grImport} package.
#' @param silhouette_coords A list of numeric vectors of length 4 giving the
#' coordinates to pass to \code{readPicture} in the \pkg{grImport} package. x1,
#' y1, x2, y2. One list element per panel.
#' @param max_range_given A value giving the maximum multiplicative range of the
#'   colour scale. If left to \code{NULL} then this will be calculated internally.
#' @param make_plot Logical: should the maps be made?
#' @param ... Anything extra to pass to \code{col_box_key}.
#' @export

map_class_ext <- function(er_dat, min_prov_genera = 20,
  col_pal = RColorBrewer::brewer.pal(9, "YlOrRd"),
  plot_column = "mean.ext", plot_order = c("Mammalia", "Elasmobranchii",
    "Echinoidea", "Gastropoda", "Anthozoa", "Bivalvia"),
  yticks = c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),
  ylabel = "Intrinsic extinction risk", col_range = FALSE,
  exact_limits = NULL, log_yticks = FALSE, fixed_range = FALSE,
  picture_files = NULL, silhouette_coords = list(c(-2.2, -0.75, -1.9, -0,86)),
  max_range_given = NULL, make_plot = TRUE, ...) {

  plot_order <- data.frame(class = plot_order, plot_order = 1:length(plot_order))

  if(length(silhouette_coords) == 1)
    for(i in 1:nrow(plot_order)) {
      silhouette_coords[[i]] <- silhouette_coords[[1]]
    }

  er.df <- er_dat
  plot_col_n <- (1:ncol(er_dat))[names(er_dat) %in% plot_column]
  er.df$value.to.plot <- NULL
  er.df$value.to.plot <- er.df[, plot_col_n]

  # remove provinces with fewer than a specified number of genera:
  er.df <- droplevels(subset(er.df, er.df$N.gen >= min_prov_genera))

  if(!col_range) { # colour range goes from min to max value:
    # transform values to a 0 to 1 range for cutting into colours:
    er.df <- plyr::ddply(er.df, "class", transform, value.to.plot.01 =
        range01(value.to.plot))
    # get lower and upper values to place colour ramp on axis later:
    er.df <- plyr::ddply(er.df, "class", transform, lower.col.cut =
        min(value.to.plot), upper.col.cut = max(value.to.plot))
  } else { # colour range goes from +/- some fixed multiplicative value
    er.df <- plyr::ddply(er.df, "class", mutate, range.val =
        abs(diff(range(value.to.plot))))
    if(is.null(max_range_given)) {
      max_range <- max(er.df$range.val)
    } else {
      max_range <- log(max_range_given)
    }
    er.df <- plyr::ddply(er.df, "class", transform, value.to.plot.01.temp =
        range01(value.to.plot))
    er.df$prop_range <- er.df$range.val / max_range
    er.df$value.to.plot.01 <- er.df$value.to.plot.01.temp * er.df$prop_range +
      (1 - er.df$prop_range) / 2
    er.df <- plyr::ddply(er.df, "class", transform,
      mean.value.to.plot = mean(value.to.plot))
    er.df$lower.col.cut <- er.df$mean.value.to.plot - max_range / 2
    er.df$upper.col.cut <- er.df$mean.value.to.plot + max_range / 2
  }

  if(fixed_range) {
    # transform values to a 0 to 1 range for cutting into colours:
    er.df <- transform(er.df, value.to.plot.01 = range01(value.to.plot))
    # get lower and upper values to place colour ramp on axis later:
    er.df <- transform(er.df, lower.col.cut = min(value.to.plot),
      upper.col.cut = max(value.to.plot))
  }

  # get colours to plot:
  er.df$col.pal.ext.risk <- col_pal[findInterval(er.df$value.to.plot.01,
    seq(-0.0001, 1.0001, length.out = 10))]

  # add projected x and y values to plot:
  er.df.mol <- mapproject(list(x = er.df$long, y = er.df$lat), proj = "mollweide")
  er.df.m <- er.df
  er.df.m$x <- er.df.mol$x
  er.df.m$y <- er.df.mol$y

  # Create a box/oval to outline the map:
  N <- 100
  x.s <- seq(-180, 180, length.out = N)
  y.s <- seq(-90, 90, length.out = N)
  square <- data.frame(x = c(x.s, rep(x.s[N], N), rev(x.s), rep(x.s[1],
    N)), y = c(rep(y.s[1], N), y.s, rep(y.s[N], N), rev(y.s)))
  oval <- mapproj::mapproject(list(x = square$x, y = square$y), proj = "mollweide")

  er.df.m <- plyr::join(er.df.m, plot_order, by = "class")

  # layout:
  mw <- 88 # map width
  mg <- 1 # map gap
  kw <- 3 # key width
  kg <- 8 # key gap
  nrow <- 3
  N <- nrow*2*2

  lo <- matrix(ncol = mw*2 + kw*2 + mg*2 + kg*2, nrow = nrow)
  for(row.i in seq(1, nrow)) {
    i <- (row.i - 1) * 4 + 1
    lo[row.i, ] <-
      c(rep(i+0, mw), # map width
        rep(N+i+0, mg), # map gap
        rep(i+1, kw), # key width
        rep(N+i+1, kg), # key gap
        rep(i+2, mw), # map width
        rep(N+i+2, mg), # map gap
        rep(i+3, kw), # key width
        rep(N+i+3, kg)) # key gap
  }

  if(make_plot) {
  layout(lo)

  par(mar = c(0,0,0, 0), oma = c(0,0,1,3.5))
  par(cex = 0.5)
  par(tck = -0.15)
  par(mgp = c(3, 0.35, 0))

  ii <<- 0 # for panel counting

  plyr::d_ply(er.df.m, "plot_order",
    function(class.dat) {
      ii <<- ii + 1
      message(paste("Plotting", unique(class.dat$class)))

      # remove provinces with fewer than a specified number of genera:
      class.dat <- droplevels(subset(class.dat, class.dat$N.gen >=
          min_prov_genera))

      # Set up a blank map:
      maps::map("world", proj = "mollweide",  col = "grey69", fill = TRUE, lwd =
          0.9, myborder = c(0, 0), type = "n", wrap = FALSE, resolution = 0,
        xlim = c(-178, 178), plot = TRUE, border = "grey69", mar = c(0,
          0, 0, 0))

      polygon(oval, col = "grey85", border = NA, lwd = 1.2)

      # Add the eco provinces:
      plyr::d_ply(class.dat, "group", function(class.group.dat) {
        with(class.group.dat, polygon(x, y, border = NA, col =
            col.pal.ext.risk, lwd = 1.5))
      })

      # Add the land on top:
      maps::map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey60", fill =
          TRUE, lwd = .55, myborder = c(0, 0), border = "grey60", wrap =
          FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE, add =
          TRUE)

      # patches:
      with(ant_patch, polygon(x, y, col = "grey60", border = FALSE))
      with(russia_patch, polygon(x-0.01, y, col = "white", border = FALSE))

      # border:
      lines(oval, col = "grey60", lwd = 1.2)

      # Label each panel:
      label <- unique(class.dat$class)
      if(label == "Malacostraca") label <- "Decapoda"
      if(label == "Anthozoa") label <- "Scleractinia"
      if(label == "Elasmobranchii") label <- "Sharks"

      mtext(substitute(paste(phantom("g"), bold(let), " ", lab, phantom("g")),
        list(let = LETTERS[ii], lab = as.character(label))),
        line = -1.5, cex = 0.8, col = "grey30")

      usr <- par("usr")

      # Colour key:
      par(las = 1)
      col.regions <- with(class.dat, seq(unique(lower.col.cut),
        unique(upper.col.cut), length.out = 10))
      #loc.limits <- with(class.dat, c(min(value.to.plot), max(value.to.plot)))
      loc.limits <- with(class.dat, c(lower.col.cut, upper.col.cut))
      add_locator <- FALSE

      if(is.null(exact_limits)) {
        ll <- exp(min(er.df$value.to.plot))
        uu <- exp(max(er.df$value.to.plot))
        yrange <- yticks[yticks >= ll & yticks <= uu]
        limits <- c(min(er.df$value.to.plot), max(er.df$value.to.plot))
      } else {
        yrange <- yticks
        limits <- log(exact_limits)
      }

      if(log_yticks) yrange_show <- log(yrange)
      if(!log_yticks) yrange_show <- yrange

      col_box_key(col.pal = col_pal, limits = limits, width = .3, col.regions =
          col.regions, bg = "grey85", border.col = "grey60", at =
          log(yrange), at.labels = yrange_show,
        add_locator = add_locator, loc_limits =
          loc.limits, loc_col = "black", loc_width = 2, ...)

      #points(-2.2, -0.78, cex = 2) # testing
      if(!is.null(picture_files)) {
        p <- grImport::readPicture(picture_files[ii])
        grImport::picture(p, silhouette_coords[[ii]][1], silhouette_coords[[ii]][2],
          silhouette_coords[[ii]][3], silhouette_coords[[ii]][4])
      }
    })
  mtext(ylabel, side = 4, outer = TRUE, line = 2.0, las = 0, cex = 0.8, col = "grey30")
  }

  invisible(max_range)
}


#' Put data on range from 0 to 1 and assign colours
#'
#' @param x A numeric vector to scaled between 0 and 1.
#'
#' @export
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
