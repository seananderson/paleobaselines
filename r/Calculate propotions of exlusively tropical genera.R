source(Spalding.Map.FINAL.R)
head(d.eco.filled.zone)
trop_score <- ifelse(d.eco.filled.zone$Zone=="tropical",1,0)
extratrop_score <- ifelse(trop_score==1,0,1)
zone.scored <- data.frame(d.eco.filled.zone,trop_score,extratrop_score)

trop.only <- function(df) {
  tsums <- sum(df$extratrop_score)
  trop <- ifelse(tsums>0,0,1)
  trop
}
extratrop.only <- function(df) {
  esums <- sum(df$trop_score)
  extratrop <- ifelse(esums>0,0,1)
  extratrop
}


gen.scores <- ddply(zone.scored,.(class,genus),each(trop.only,extratrop.only))

prop.trop <- function(df) sum(df$trop.only)/length(df$trop.only)
prop.extratrop <- function(df) sum(df$extratrop.only)/length(df$extratrop.only)

props <- ddply(gen.scores,.(class),each(prop.trop,prop.extratrop))
prop.cosmo <- 1-(props$prop.trop + props$prop.extratrop)
props <- data.frame(props,prop.cosmo)
