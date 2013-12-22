Quartiles <- function(x){
	Q1 <- data.frame(cut2(x, g=10, levels.mean = FALSE))
    Q1 <- data.frame(seq(1,length(Q1[,1]),1),Q1)
    colnames(Q1) <- c("num","Q")
    Qlev <- levels(as.factor(Q1$Q))
    Qord <- seq(1,length(Qlev),by = 1)
    Qlookup <- data.frame(Qlev,Qord)
    colnames(Qlookup) <- c("Q","quant")
    Qquant <- merge(Qlookup,Q1,by = "Q",sort = F)
    Qquant <- Qquant[order(Qquant$num),]
    return(Qquant$quant)
    }