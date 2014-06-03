library(plyr)

load('~/Dropbox/nescent_extinction_map/data/PaleoDB downloads/4.6.2014/4.6.2014.occs.rda')

### redefine Plio-Pleistocene FR_2 Bin
pbdb$FR2_bin <- ifelse(pbdb$interval_base <= 5.333 & pbdb$interval_top >= .0117,"Plio-Pleistocene",as.character(pbdb$FR2_bin))

#define the stages we want to use, for reference
FR2_bin <- c("Pleistocene","Pliocene","Plio-Pleistocene","Upper Miocene","Middle Miocene","Lower Miocene")
stage.name <- c("Plio-Pleistocene","Plio-Pleistocene","Plio-Pleistocene","Upper Miocene","Middle Miocene","Lower Miocene")
top <- c(.0117,.0117,.0117,5.333,11.608,15.97)
bottom <- c(5.333,5.333,5.333,11.608,15.97,23.03)
stage.dates <- data.frame(FR2_bin,stage.name,bottom,top)

pbdb <- merge(pbdb,stage.dates,by="FR2_bin")


### drop any name without a taxon #
pbdb <- pbdb[!(is.na(pbdb$taxon_no) | pbdb$taxon_no==0), ] # not removing anything


### make column of class-genus to avoid mixing up different groups
pbdb$clgen <- paste(pbdb$class_name,pbdb$occurrence.genus_name)

### extract apparent extinction dates of genera
gen.LA <- function(df) min(df$top)
gen.LAs <- ddply(pbdb,.(clgen),each(gen.LA),.progress="text")

### merge back in and add row to indicate extinction in LA bin if the genus is not listed as extant
pbdb <- merge(pbdb,gen.LAs,by="clgen",all.x=TRUE)

pbdb$Extinct.in.bin <- ifelse(pbdb$top == pbdb$gen.LA & pbdb$genus_extant != "yes",1,0)

# Okay, this next step is a bit of a kludge.  Automating all of the
# taxonomic processing that went into the first file is going to take some time.
# So instead what I'll do here is take the older version, match those taxonomic
# groups to these based on shared genus names, and then apply these groups to
# any genera with matching higher taxonomy. So a genus that wasn't in the
# original download will be dropped UNLESS it belongs to a family or order than
# had already been placed within in a group.  For the final version, this step
# should be eliminated.

pbdb.gen.cats <- read.csv("~/Dropbox/nescent_extinction_map/data/PBDB.genera.Groups.csv", header=TRUE)

genera.to.drop <- unique(pbdb$occurrence.genus_name)[!unique(pbdb$occurrence.genus_name) %in% unique(pbdb.gen.cats$Genus)]
pbdb.dropped <- pbdb[pbdb$occurrence.genus_name %in% genera.to.drop, ]

pbdb.gen <- data.frame(pbdb$class_name,pbdb$order_name,
                       pbdb$family_name, pbdb$occurrence.genus_name)
colnames(pbdb.gen) <- c("class","order","family","Genus")

pbdb.cats <- merge(pbdb.gen,pbdb.gen.cats,by="Genus",all.x=TRUE)

### make table show family-level and higher matches to our categories
pbdb.fams <- na.omit(unique(data.frame(pbdb.cats$family,pbdb.cats$Group)))
colnames(pbdb.fams) <- c("family","family.Group")
pbdb.fams <- pbdb.fams[!(is.na(pbdb.fams$family) | pbdb.fams$family==""), ]

pbdb.gen <- data.frame(pbdb.gen.cats$Group,pbdb.gen.cats$Genus)
colnames(pbdb.gen) <- c("genus.Group","occurrence.genus_name")


pbdb$order.Group <- ifelse(pbdb$class_name == "Chondrichthyes",
                           "Elasmobranchii_Elasmobranchii",
                           ifelse(pbdb$order_name == "Decapoda","Decapoda_Decapoda",
                                  ifelse(pbdb$order_name == "Testudines","Testudines_Testudines",
                                         "none")))

pbdb <- merge(pbdb,pbdb.gen,by="occurrence.genus_name",all.x=TRUE)
pbdb <- merge(pbdb,pbdb.fams,by.x="family_name",by.y="family",all.x=TRUE)

levels(pbdb$order.Group) <- c(levels(pbdb$order.Group), "none")
levels(pbdb$family.Group) <- c(levels(pbdb$family.Group), "none")
levels(pbdb$genus.Group) <- c(levels(pbdb$genus.Group), "none")

pbdb$order.Group[is.na(pbdb$order.Group)] <- "none"
pbdb$family.Group[is.na(pbdb$family.Group)] <- "none"
pbdb$genus.Group[is.na(pbdb$genus.Group)] <- "none"
pbdb$order.Group <- as.character(pbdb$order.Group)
pbdb$family.Group <- as.character(pbdb$family.Group)
pbdb$genus.Group <- as.character(pbdb$genus.Group)

pbdb$Group <- ifelse(pbdb$genus.Group != "none",
                     pbdb$genus.Group,ifelse(pbdb$family.Group != "none",
                                             pbdb$family.Group, pbdb$order.Group))

pbdb <- pbdb[!(pbdb$Group =="none"),]

taxa <- strsplit(pbdb$Group, "_")
taxa2 <- ldply(taxa)
colnames(taxa2) <- c("class", "order")

pbdb <- data.frame(pbdb,taxa2)

### to be safe, remove occurrences that fall off of the equal-area grid
pbdb <- pbdb[(pbdb$paleolatdec  < 82) & (pbdb$paleolatdec  > -75) ,]
pbdb <- 

###okay, now make table
Genera <- function(df) length(unique(df$occurrence.genus_name))
Occurrences <- function(df) length(df$occurrence.genus_name)


Table.S1 <- ddply(pbdb,.(FR2_bin,class),each(Genera ,Occurrences))




