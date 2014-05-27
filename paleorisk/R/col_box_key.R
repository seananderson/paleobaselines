#' Create a colour box-plot like colour key
#'
#' @export

col_box_key <- function(col.pal = brewer.pal(9, "YlOrRd"), limits =
    c(-3, 0), width = 1, col.regions = seq(-1, -0.5, length.out = 10), N
  = 10, bg = "grey60", border.col = "grey30", at = c(-3, -1.5, 0),
  add_locator = FALSE, loc_limits = c(-999, 999), loc_col = "grey60",
  loc_width = 1.5, col.border.col = "grey60", limit_pad = 0.55,
  at.labels = c(-999, 999), ...) {

  par(xpd = NA)
  plot(1, 1, type = "n", xlim = c(0, width), ylim =
      c(limits[1]-limit_pad, limits[2] + limit_pad), axes = FALSE, xlab
    = "", ylab = "", xaxs = "i", yaxs = "i", ...)
  rect(0, limits[1], width, limits[2], col = bg, border = NA)
  axis(4, col = border.col, col.axis = "grey40", at = at, labels = at.labels, cex.axis = 1.1)

  # the colours:
  for(i in 1:(N-1)) {
    if(col.regions[i+1] > limits[1] & col.regions[i] < limits[2]) {
      rect(0, col.regions[i], width, col.regions[i+1], col = col.pal[i],
        border = NA)
    }
  }

  # border on the whole box:
  rect(0, limits[1], width, limits[2], border = border.col, lwd = 0.9)

  # add locator box?
  if(add_locator) {
    rect(0-0.01, loc_limits[1], width+0.01,loc_limits[2], border =
        loc_col, lwd = loc_width)
    par(xpd = TRUE)
  }

}
