map2 <- function#Draw (prettier) geographical maps
### Draw lines and polygons as specified by a map database. This
### version has been modified from the map() function in the maps
### package to be less ugly. The only difference is that it won't plot
### country borders if fill = TRUE.
(database = "world", regions = ".", exact = FALSE, boundary = TRUE, 
    interior = TRUE, projection = "", parameters = NULL, orientation = NULL, 
    fill = FALSE, col = 1, plot = TRUE, add = FALSE, namesonly = FALSE, 
    xlim = NULL, ylim = NULL, wrap = FALSE, resolution = if (plot) 1 else 0, 
    type = "l", bg = par("bg"), mar = c(4.1, 4.1, par("mar")[3], 
        0.1), border = 0.01, ...) 
{
    if (resolution > 0 && !plot) 
        stop("must have plot=TRUE if resolution is given")
    if (!fill && !boundary && !interior) 
        stop("one of boundary and interior must be TRUE")
    doproj <- !missing(projection) || !missing(parameters) || 
        !missing(orientation)
    #coordtype <- maptype(database)
    coordtype <- "IGNORE"
    if (coordtype == "unknown") 
        stop("missing database or unknown coordinate type")
    if (doproj && coordtype != "spherical") 
        stop(paste(database, "database is not spherical; projections not allowed"))
    if (is.character(database)) 
        as.polygon = fill
    else as.polygon = TRUE
    coord <- map.poly(database, regions, exact, xlim, ylim, boundary, 
        interior, fill, as.polygon)
    if (is.na(coord$x[1])) 
        stop("first coordinate is NA.  bad map data?")
    if (plot) {
        assign(".map.range", coord$range, envir = globalenv())
    }
    if (doproj) {
        nam <- coord$names
        require(mapproj)
        coord <- mapproject(coord, projection = projection, parameters = parameters, 
            orientation = orientation)
        coord$projection = projection
        coord$parameters = parameters
        coord$orientation = orientation
        if (plot && coord$error) 
            if (all(is.na(coord$x))) 
                stop("projection failed for all data")
            else warning("projection failed for some data")
        coord$names <- nam
    }
    else mapproject <- function() {
    }
    if (plot) {
        if (!add) {
            opar = par(bg = bg)
            if (!par("new")) 
                plot.new()
            if (is.null(xlim) || doproj) 
                xrange <- range(coord$x, na.rm = TRUE)
            else xrange <- xlim
            if (is.null(ylim) || doproj) 
                yrange <- range(coord$y, na.rm = TRUE)
            else yrange <- ylim
            if (coordtype != "spherical" || doproj) {
                aspect <- c(1, 1)
            }
            else aspect <- c(cos((mean(yrange) * pi)/180), 1)
            d <- c(diff(xrange), diff(yrange)) * (1 + 2 * border) * 
                aspect
            if (coordtype != "spherical" || doproj) {
                plot.window(xrange, yrange, asp = 1/aspect[1])
            }
            else {
                par(mar = mar)
                p <- par("fin") - as.vector(matrix(c(0, 1, 1, 
                  0, 0, 1, 1, 0), nrow = 2) %*% par("mai"))
                par(pin = p)
                p <- par("pin")
                p <- d * min(p/d)
                par(pin = p)
                d <- d * border + ((p/min(p/d) - d)/2)/aspect
                usr <- c(xrange, yrange) + rep(c(-1, 1), 2) * 
                  rep(d, c(2, 2))
                par(usr = usr)
            }
            on.exit(par(opar))
        }
        if (is.character(database) && resolution != 0 && type != 
            "n") {
            pin <- par("pin")
            usr <- par("usr")
            resolution <- resolution * min(diff(usr)[-2]/pin/100)
            coord[c("x", "y")] <- mapthin(coord, resolution)
        }
        if (type != "n") {
            if (wrap) 
                coord = map.wrap(coord)
            if (fill) 
                polygon(coord, col = col, border = col, lty = 1, ...)
            else lines(coord, col = col, type = type, ...)
        }
    }
    class(coord) = "map"
    value <- if (namesonly) 
        coord$names
    else coord
    if (plot) 
        invisible(value)
    else value
} 

