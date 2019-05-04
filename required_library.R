install.packages("shiny")       #ok
install.packages("shinythemes") #ok
install.packages("ggplot2")     #ok
install.packages("gridExtra")   #ok
install.packages("grid")        #Do I use it?
install.packages("openxlsx")    #ok
install.packages("shinyjs")

source("https://bioconductor.org/biocLite.R")
biocLite("ShortRead")           #ok
biocLite("Biostrings")          #ok

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("IRanges", version = "3.8")
