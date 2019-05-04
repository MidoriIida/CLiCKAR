library(ShortRead)
library(Biostrings)

#v4.1.0 (header Name were changed)
demultiplex <- function(barcodetable, read){
  
  Barcode_Table <- read.csv(file=barcodetable, stringsAsFactors = FALSE)
  reads         <- readFastq(read)
  
  #trim_length   <- as.numeric(as.character(trim_length))
  Barcode_Table$FWD_primer <- as.character(Barcode_Table$FWD_primer) #changed
  Barcode_Table$REV_primer <- as.character(Barcode_Table$REV_primer) #changed
  
  Clones <- list()
  
  for (i in 1:length(Barcode_Table$Sample_name)) #changed
  {
    Barcode1 <- toupper(Barcode_Table$FWD_primer[i])
    Barcode2 <- toupper(Barcode_Table$REV_primer[i])
    name     <- Barcode_Table$Name[i]
    
    # create filters
    RowFilter <- srFilter(function(x){
      substr(sread(x),1,nchar(Barcode1))==Barcode1
    },name="Row Filter")
    
    ColumnFilter <- srFilter(function(x){
      substr(sread(x),1,nchar(Barcode2))==Barcode2
    },name="Column Filter")
    
    # subset row and column reads
    RowReads    <- reads[substr(sread(reads),1,nchar(Barcode1))==Barcode1] #reads[RowFilter(reads)]
    ColumnReads <- reads[substr(sread(reads),1,nchar(Barcode2))==Barcode2] #reads[ColumnFilter(reads)]
    
    # create reverse complement objects of row and column reads
    RowReads_Reverse <- ShortReadQ(reverseComplement(sread(RowReads)), FastqQuality(reverse(quality(quality(RowReads)))), id(RowReads))
    ColumnReads_Reverse <- ShortReadQ(reverseComplement(sread(ColumnReads)), FastqQuality(reverse(quality(quality(ColumnReads)))), id(ColumnReads))
    
    # Search Row Reads Rev for Column Reads and vice versa
    Clone_1 <- RowReads_Reverse[substr(sread(RowReads_Reverse),1,nchar(Barcode2))==Barcode2] #RowReads_Reverse[ColumnFilter(RowReads_Reverse)]
    Clone_2 <- ColumnReads_Reverse[substr(sread(ColumnReads_Reverse),1,nchar(Barcode1))==Barcode1] #ColumnReads_Reverse[RowFilter(ColumnReads_Reverse)]
    
    # inversion clone_2
    Clone_1 <- ShortReadQ(reverseComplement(sread(Clone_1)), FastqQuality(reverse(quality(quality(Clone_1)))), id(Clone_1))
    
    # Append two directions
    Clone <- append(Clone_1,Clone_2)
    
    # trim barcodes; change the number to the length of the longest barcode
    Clone <- ShortReadQ(reverseComplement(sread(Clone)), FastqQuality(reverse(quality(quality(Clone)))), id(Clone))
    Clone <- ShortReadQ(reverseComplement(sread(Clone)), FastqQuality(reverse(quality(quality(Clone)))), id(Clone))
    
    # write fastq
    Clones[[i]] <- Clone
  }
  
  return(Clones)
}