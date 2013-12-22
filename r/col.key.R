col.key <- function# Add a continuous colour key to a plot.
### Add a continuous colour key to a plot. Emphasis on not looking terrible.
(
cols,
### A character vector of the colours to make up the continuous colour
### scheme.
key.loc = c(0, 0.02, 0.3, 0.7),
### The location of the plot as a numeric vector following the format:
### \code{c(x.left, x.right, y.lower, y.upper)}. This corresponds to \code{fig} in
### \code{par}.
at,
### The locations to label the colour ramp at. Corresponds to the
### \code{at} parameter in the call to \code{axis()}.
labels,
### What to label the labels on the colour ramp. Corresponds to the
### \code{labels} parameter in the call to \code{axis()}.
tck = -0.2,
### The axis tick length.
orientation = c("vertical", "horizontal"),
### Whether you want the colour key to be horizontal or vertical in
### orientation.
box.col = "darkgrey"
### The colour of the box around the colour key.
) {
  par(new = TRUE)
  par(fig = key.loc, mar = c(0,0,0,0), oma = c(0,0,0,0))
  type <- match.arg(orientation)
  n <- length(cols)
  if(type == "horizontal") {
    plot(1, 1, ylim = c(0, 1), xlim = c(0, n), xaxs = "i", yaxs = "i", type = "n", xlab = "", ylab = "", axes = F)
    for(i in 1:(n - 1)){rect((0:n)[i], 0, (1:n)[i+1], 1, border = NA, col = cols[i])}
    box(col = box.col)
    axis(1, at = at, labels = labels, tck = tck, col = box.col)
  } else {
    plot(1, 1, xlim = c(0, 1), ylim = c(0, n), xaxs = "i", yaxs = "i", type = "n", xlab = "", ylab = "", axes = F)
    for(i in 1:(n - 1)){rect(0, (0:n)[i], 1, (1:n)[i+1], border = NA, col = cols[i])}
    box(col = box.col)
    axis(4, at = at, labels = labels, tck = tck, col = box.col)
  }
  par(new = FALSE)
}

