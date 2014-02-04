#' Calibrate the fossil data
#'
#' This function attempts to calibrate the fossil data to account for
#' undersamping compared to the contemporary period by using the data from the
#' Plio-Pleistocene as a model. For each class-range parameter combination it
#' takes the genera found in both the modern and Plio-Pleistocene datasets and
#' regresses the former on the latter. It then uses this regression to calibrate
#' the data.
#'
#' @param dat The input data. Should be output from
#' \code{\link{standardize_data}} over multiple stages.
#' @export

calibrate_fossil <- function(dat) {

    ## choose modern and Plio-Pleistocene data for fossil range calibration based on fossil data
    Plio_Pleistocene <- dat[dat$stage == "Plio-Pleistocene", ]
    Modern <- dat[dat$stage == "Modern", ]

    ## reshape data
    drops <- c("stage","stage_top","mean.lat.zone","tropical_only","Ex")
    Plio_Pleistocene <- Plio_Pleistocene[,!(names(Plio_Pleistocene) %in% drops)]
    Modern <- Modern[,!(names(Modern) %in% drops)]

    Plio_Pleistocene <- reshape2::melt(Plio_Pleistocene,
      id = c("class","group","genus"),
      measured = c("richness","occupancy","occurrences", "min.lat",
        "max.lat","lat.range", "mean.lat", "great.circle"))

    Modern <- reshape2::melt(Modern,
      id=c("class","group","genus"),
      measured=c("richness","occupancy","occurrences", "min.lat",
        "max.lat","lat.range", "mean.lat", "great.circle"))

    comp.data <- merge(Plio_Pleistocene,Modern,by=c("class","group","genus","variable"))
    colnames(comp.data) <- c("class","group","genus","variable","Plio_Pleistocene","Modern")

    #require(ggplot2)
    #p <- ggplot(comp.data,aes(Plio_Pleistocene,Modern))
    #p + geom_point() + facet_wrap(~variable,ncol=3,scales="free") + geom_smooth(method="lm")

    ### make table of regressions for each class-variable combination

    coefs <- function(df) {
      model <- lm(df$Modern ~ df$Plio_Pleistocene)
      slope <- coefficients(model)[2]
      intercept <- coefficients(model)[1]
      n.gen <- length(df$Modern)
      outs <- data.frame(slope,intercept,n.gen)
    }
    regressions <- plyr::ddply(comp.data,.(class,variable),each(coefs))

    #names(regressions) <- c(".class", ".variable", "slope", "intercept", "n.gen")
    #regressions2 <- reshape2::melt(regressions,id=c(".class", ".variable"),
      #measured=c("slope","intercept","n.gen"))
    #regressions3 <- reshape2::dcast(regressions2, .class + .variable  ~ variable)
    #regressions2 <- regressions3
    #regressions2 <- plyr::rename(regressions2, c(".variable" = "variable", ".class" = "class"))

    ### merge regression table to original data tables, extract new range parameters

    ## set aside modern, remove from the dataset

    Modern <- dat[dat$stage == "Modern", ]
    dat <- dat[dat$stage != "Modern", ]

    data.out2 <- list()

    for(i in 1:length(unique(dat$stage))){
      data <- dat[dat$stage == unique(dat$stage)[i], ]
      data <- reshape2::melt(data, id=c("stage","stage_top","class","group","genus"),
        measured=c("richness","occupancy","occurrences", "min.lat",
          "max.lat","lat.range", "mean.lat",
          "great.circle","mean.lat.zone","tropical_only","Ex"))
      data <- merge(data,regressions,by=c("class","variable"),all=TRUE)
      adjusted.value <- (data$value*data$slope) + data$intercept
      adjusted.value[is.na(adjusted.value)] <- -1
      data$value <- ifelse(adjusted.value <0,data$value,adjusted.value)
      drops <- c("slope","intercept","n.gen")
      data <- data[,!(names(data) %in% drops)]
      data$stage_top <- as.factor(data$stage_top)
      data <- reshape2::melt(data,measured="value", variable.name = ".variable",
        id = c("class", "variable", "stage", "stage_top", "group", "genus"))
      data <- reshape2::dcast(data, stage + stage_top + class + group + genus ~ variable)
      data.out2[[i]] <- data
      }
      out <- do.call("rbind", data.out2)
      return(out)
  }
