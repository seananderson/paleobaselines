#### combine files
Cen <- read.csv("~/Dropbox/nescent_extinction_map/data/PaleoDB downloads/6.11.2013/PaleoDB_6.11.2013.csv",header = TRUE,stringsAsFactors = FALSE)
    
time <- read.table("~/Dropbox/Paleobiology Database Downloads/timescale.csv", header=TRUE, sep=",")
# 
#     # Merged file containing bin assignements excluding non-assigned bins # changed
comb<-merge(Cen,time, by.x="X10_my_bin", by.y="Bin.name", all=FALSE, sort=FALSE) # 430854
 
# # Stages
 stg <- read.table(file="~/Dropbox/Paleobiology Database Downloads/PBDB_stages.csv", header=TRUE, sep=",")
 # Extract a field for stages
   slc <- rep(NA, nrow(comb))
  for (i in 1:nrow(stg)) {
   st.n <- stg$SLICE[i]
    st.b <- stg$BIN[i]
   b <- stg$LOW[i]
   t <- stg$HIGH[i]
    if (stg$use.bin[i]=="y") slc <- replace(slc, which(comb$Bin==st.b), st.n)
    if (stg$use.bin[i]=="n") slc <- replace(slc, which(comb$ma_max<=b & comb$ma_min>=t), st.n)
   }

  comb <- cbind(comb, slc) # merge
  
Cenozoic.csv <- write.table(comb,"~/Dropbox/nescent_extinction_map/data/PaleoDB downloads/6.11.2013/PaleoDB_6.11.2013_slc.csv",sep=",")
   