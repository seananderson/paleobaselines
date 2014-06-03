#' Create a colour box-plot like colour key
#'
#' @param col.pal Vector of colours to use in legend
#' @param limits Numeric vector of length 2 giving the upper and lower postion
#' limits to place the legend.
#' @param xpos X position for left limit of legend box.
#' @param width The width of the legend bar. Goes from 0 to this value.
#' @param col.regions Y positions to place colour divisions. Should be of length
#' \code{length(col.pal) + 1}
#' @param bg Background colour for any region of the rectangle without colour.
#' @param border.col Colour of the legend border.
#' @param at Positions of the ticks.
#' @param add_locator Currently ignored.
#' @param loc_limits Currently ignored.
#' @param loc_col Currently ignored.
#' @param loc_width Currently ignored.
#' @param limit_pad Amount to pad y limits from \code{limits}
#' @param at.labels Labels to appyl at \code{at} vector.
#' @param add Add to existing plot?
#'
#' @export
#'
#' @examples
#' col_box_key(at = c(-1, 1), limits = c(-1.5, 1.2), limit_pad = 0)

col_box_key <- function(col.pal = RColorBrewer::brewer.pal(9, "YlOrRd"),
  limits = c(-1, 1), xpos = 0, width = 1, col.regions = seq(-1, 1, length.out = 10),
  bg = "grey60", border.col = "grey30", at = c(-1, 1),
  add_locator = FALSE, loc_limits = c(-999, 999), loc_col = "grey60",
  loc_width = 1.5, limit_pad = 0.55,
  at.labels = c(-1, 1), add = FALSE, ...) {

  add_locator <- FALSE # so this does nothing

  N <- length(col.regions)
  par(xpd = NA)
  if(!add) {
  plot(1, 1, type = "n", xlim = c(xpos, xpos + width), ylim =
      c(limits[1]-limit_pad, limits[2] + limit_pad), axes = FALSE, xlab
    = "", ylab = "", xaxs = "i", yaxs = "i", ...)
  }
  rect(xpos, limits[1], xpos + width, limits[2], col = bg, border = NA)
  axis(4, col = border.col, col.axis = "grey40", at = at, labels = at.labels, cex.axis = 1.1)

  # the colours:
  for(i in 1:(N-1)) {
    if(col.regions[i+1] > limits[1] & col.regions[i] < limits[2]) {
      rect(xpos, col.regions[i], xpos + width, col.regions[i+1], col = col.pal[i],
        border = NA)
    }
  }

  # border on the whole box:
  rect(xpos, limits[1], xpos + width, limits[2], border = border.col, lwd = 0.9)

  # add locator box?
  if(add_locator) {
    rect(0-0.01 + xpos, loc_limits[1], xpos + width+0.01,loc_limits[2], border =
        loc_col, lwd = loc_width)
    par(xpd = TRUE)
  }

}
