files <- list.files(path = "../figs", pattern = "\\.pdf")
files_png <- sub("\\.pdf", ".png", files)

files <- paste0("../figs/", files)
files_png <- paste0("../figs/", files_png)

for(i in 1:length(files)) {
  message(files_png[i])
  system(paste("convert -density 180 -quality 100 -trim",
      files[i], files_png[i]))
}

files <- list.files(path = "../static-figs", pattern = "\\.pdf")
files_png <- sub("\\.pdf", ".png", files)

files <- paste0("../static-figs/", files)
files_png <- paste0("../static-figs/", files_png)

for(i in 1:length(files)) {
  message(files_png[i])
  system(paste("convert -density 180 -quality 100 -trim",
      files[i], files_png[i]))
}
