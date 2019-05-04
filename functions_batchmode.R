#########################################
#               step1                   #
# criate data for counts_readcounts.pdf #
#########################################
plot_counts_readcounts<- 
function(amplicon, variable, value){
  
  data <- as.data.frame(amplicon)
  data$variable <- variable
  data$value    <- value
  
  ampliconlen <- length(amplicon)
  if(ampliconlen> 10 & ampliconlen < 100){ #10~100
    lensize <- 10/ampliconlen * 25
    ledgendstheme <- theme(legend.title = element_text(size=15),legend.text = element_text(size=lensize))
  }else{
    if(ampliconlen >= 100){
      ledgendstheme <- theme(legend.position = 'none')
    }else{
      ledgendstheme <- theme(legend.title = element_text(size=15),legend.text = element_text(size=15))
    }
  }
  
  g2 <- ggplot(data, aes (x = variable, y = value, fill = amplicon))
  g2 <- g2 + labs(fill = "Sample name") + labs(x="", y="read counts")
  g2 <- g2 + geom_bar(stat = "identity", col= "grey85") 
  g2 <- g2 + guides(fill = guide_legend(ncol  = 1)) + ledgendstheme
  g2 <- g2 + theme(axis.text.x = element_text(size=15), 
                   axis.text.y = element_text(size=15),
                   axis.title.y = element_text(size=15, margin = margin(0, 20, 0, 0))
  )
  
  return(g2)
}

#########################################
#               step2                   #
# criate data for gs_resultfixedInOut   #
#########################################
plot_gs_resultfixedInOut <- 
  function(resultCIGs){
  Indel_mat   <- melt(as.matrix(resultCIGs[[4]]))
  q  <- ggplot(Indel_mat, aes(x=Var2, y=value, fill=Var1)) 
  q <- q + geom_bar(stat="identity") 
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + ylab("% of total reads") + xlab("Sample name") +
    theme(legend.title = element_blank(),legend.text = element_text(size=15),
          axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
          axis.title.x = element_text(size=15), axis.title.y = element_text(size=15))
  
  return(q)
}

#########################################
#               step2                   #
# criate data for gs_resultDSBhistogram #
#########################################
plot_gs_resultDSBhistogram <- 
  function(resultCIGs){
  psa2 <- resultCIGs[[2]]
  DSB  <- resultCIGs[[3]]
  
  plist <- vector("list", length = length(psa2))
  for(i in 1:length(psa2)){
    In    <- insertion(psa2[[i]])
    Del   <- deletion(psa2[[i]])
    cove  <- coverage(append(unlist(In), unlist(Del))) 
    y <- rep(cove@values,cove@lengths)
    df <- data.frame(x = 1:length(y), y = y)
    
    plist[[i]] <- 
      ggplot(df, aes(x,y))  + theme_bw() +  ggtitle(names(DSB[i]))  +
      xlab("Indel position (bp)") + ylab("sequences") +
      geom_vline(aes(xintercept = DSB[i], colour ="DSB"),colour= 'red', show.legend = FALSE) +
      geom_hline(aes(yintercept = 0, linetype = "DSB"), colour= 'red') +
      geom_hline(aes(yintercept = 0, linetype = "DSB"), colour= 'gray75', show.legend = FALSE) +
      geom_line(aes(colour = "Indel"), col = "black") + 
      theme(axis.title.y = element_text(size = 12, vjust = 5), axis.title.x = element_text(size = 12, vjust = -5)) +
      theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) +
      theme(legend.title = element_text(size=12),legend.text = element_text(size=13))+
      theme(legend.position = c(0.8, 0.8), legend.title = element_blank())+
      theme(plot.margin= unit(c(3, 2, 2, 2), "lines")) 
  }
  
  return(plist)
}

