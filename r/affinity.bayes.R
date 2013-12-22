# Carl Simpson April 13 2012

affin.bayes <- function(vec) {
    # Calculates habitat affinities using a bayesian affinity approach. Based on
    #   Simpson & Harnik 2009.
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
    
    top <- dbinom(hab1, hab1 + hab2, pall) * 0.5
    bot <- dbinom(hab1, hab1 + hab2, pall) * 0.5 + dbinom(hab2, hab1 + hab2, pall) * 
        0.5
    Pr.Bayes <- top/bot
    
    out <- c(prob = Pr.Bayes)
    return(out)
    # Returns the probability of having an affinity for habitat 1 given the
    #   proportion of global occurrences
    
} 
