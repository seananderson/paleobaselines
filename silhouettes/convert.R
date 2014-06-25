library(grImport)
PostScriptTrace("Gastropoda.eps")
PostScriptTrace("Bivalvia.eps")
PostScriptTrace("Mammalia.eps")
PostScriptTrace("Sharks.eps")
PostScriptTrace("Echinoidea.eps")
PostScriptTrace("Scleractinia.eps")

# test
# files <- list.files(pattern = "*\\.eps\\.xml")
#
# par(mfrow = c(2, 3), cex = 0.8, mar = c(0,0,0,0))
# for(i in 1:length(files)) {
#   p <- readPicture(files[i])
#   plot(1:10, type = "n", asp = 1)
#   picture(p, 2, 2, 8, 8)
# }
