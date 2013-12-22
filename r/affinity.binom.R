# Carl Simpson April 13 2012


affin.binom <- function(vec) {
    # Calculates habitat affinities using a binomial affinity approach. Based on
    #   Foote 2006 and Kiessling & Aberhan 2007.
    # Input is a vector containing numbers of occurrences
    # hab1 = the number of occurences in habitat affinity is tested
    # hab2 = the number of occurrences in alternate habitate
    # hab1.all = the global number of occurrences of habitat 1
    # hab2.all = the global number of occurrences of habitat 2
    
    hab1 <- vec[1]
    hab2 <- vec[2]
    hab1.all <- vec[3]
    hab2.all <- vec[4]
    pall <- hab1.all/(hab1.all + hab2.all)
    Pr <- binom.test(hab1, hab1 + hab2, p = pall, alternative = "g")
    
    out <- c(prob = as.numeric(Pr$estimate), p.value = Pr$p.value)
    return(out)
    # Returns the probability of having an affinity for habitat 1 given the
    #   proportion of global occurrences
    
} 
