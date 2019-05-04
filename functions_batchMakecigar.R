library(reshape2)
library(ggplot2)
#v6.0.0 
batchAddCIGAR <-
  function(data, Range = 10, filter_ratio=0.1){
    Query_Table         <- data$query_table
    targets             <- unique(Query_Table$Gene)
    list_results        <- vector(mode = "list", length = length(targets))
    
    for(k in 1:length(targets)){
      target <- targets[k]
      Trget_Query_Table   <- Query_Table[Query_Table$Gene == target,]
      sgRNA               <- toupper(Trget_Query_Table$Protospacer_sequence_plus_PAM)
      Range               <- as.numeric(Range)
      filter_ratio        <- as.numeric(filter_ratio)
      
      ### 1. register ref ##############################################################
      ref  <- DNAStringSet(Trget_Query_Table$Ref[1])
      
      ### 2. register data #############################################################
      tmp <- sapply(data$demultiplexed_fastq[Query_Table$Gene %in% target], sread)
      
      ### 3. Alignment #######################################################
      mat <- nucleotideSubstitutionMatrix(match = 3, mismatch = -2, baseOnly = TRUE)
      psa <- function(x){pairwiseAlignment(pattern = x, subject = ref,  type = "local-global",
                                           substitutionMatrix = mat,
                                           gapOpening = 5, gapExtension = 0)}
      
      psa2 <- sapply(tmp, psa) #output1
      
      ### 4. register DSB site #######################################################
      #detect direction of guideRNA (FWD:GGNN/CCNN/, REV:NNCC/NNGG, UN:CCGG/GGCC)
      tags <- paste(substr(sgRNA, 1, 2), substr(sgRNA, nchar(sgRNA) - 1, nchar(sgRNA)), sep="") #extract start and end
      
      ntags <- gsub("A", "N", gsub("T", "N", tags)) # translate A or T to N
      
      gtags <- ntags
      gtags[substr(ntags, 1, 2) == "CC" | substr(ntags, 1, 2) == "GG"] <- "FWD"
      gtags[substr(ntags, nchar(ntags) - 1, nchar(ntags)) == "CC" | substr(ntags, nchar(ntags) - 1, nchar(ntags)) == "GG"] <- "REV"
      gtags[!(1:length(ntags) %in% grep("N", ntags))] <- "UN"
      
      #detect position of guideRNA
      DSB <- rep(0, length(sgRNA))
      DSB[gtags == "FWD"] <- sapply(sgRNA, function(x){regexpr(x, ref) + 5}) #in case of "FWD”, DSB site is left side from PAM CCTGG[G]/T
      DSB[gtags == "REV"] <- sapply(sgRNA, function(x){s <- regexpr(x, ref)  #in case of "REV", DSB site is right side from PAM [G]/TTACGG
      e <- s + attr(s, "match.length") -1
      dsb <- e - 6
      })        
      dsp <- lapply(1:length(psa2), function(i){apply(as.matrix(psa2[[i]]), 2, function(x){sum(x=="-")})})
      DSB[gtags == "UN"] <- sapply(1:length(sgRNA), function(i){             #in case of "UN"
        fw <- regexpr(sgRNA[i], ref) + 5 
        s <- regexpr(sgRNA[i], ref) 
        e <- s + attr(s, "match.length") -1
        rev <- e - 6 
        c(fw, rev)[which.min(c(abs(which.max(dsp[[i]]) - fw), abs(which.max(dsp[[i]]) - rev) ))]
      })
      names(DSB) <- Trget_Query_Table$Sample_name #output2
      
      Target_ref_seq <- substr(Trget_Query_Table$Ref, DSB - Range + 1, DSB + Range) 
      
      ### 5. make result file ########################################################
      CIGER_count_filtereds <- vector(mode = "list", length = length(psa2))
      for(f in 1:length(psa2)){
        
        ### 5. make range ########################################################
        In <-  insertion(psa2[[f]])
        Del <- deletion(psa2[[f]])
        #When there are "compare range" before inserstion, move comparange behind by width of insertion
        tmp <- which(sapply(end(In), length) > 0)
        
        if(length(tmp) > 0){
          s <- restrict(In[tmp],  end = DSB[f] - Range + 1) 
          widths <- as.matrix(width(s)) 
          widths[is.na(widths)] <- 0
          widthsSum <- rowSums(widths)
        }else{
          widthsSum <- 0
        }
        
        if(sum(widthsSum) > 0){
          comprange <- lapply(1:length(psa2[[f]]), function(x){IRanges(start = DSB[f] - Range + 1, end=DSB[f] + Range)})
          
          for( i in 1:length(tmp)){
            comprange[[tmp[i]]] <- shift(comprange[[tmp[i]]], shift = widths[i])
          }
        }else{
          comprange <- lapply(1:length(psa2[[f]]), function(x){IRanges(start = DSB[f] - Range + 1, end=DSB[f] + Range)})
        }
        
        comprange2 <- comprange
        for(i in 1:length(psa2[[f]])){
          tmp <- findOverlaps(comprange[[i]], In[[i]]) #search position where overrapping new compare range and Insertion
          
          if(length(subjectHits(tmp)) > 0){
            tmp2 <- In[[i]][subjectHits(tmp)]                    #extract range have insretion
            tmp3 <- sum(width(tmp2))                             #count how long the Insertion is
            end(comprange2[[i]]) <- end(comprange[[i]]) + tmp3   #when Insertion is in [compare region], move end behind
          }
          
          tmp4 <- IRanges(start = start(comprange2[[i]]), end = start(comprange2[[i]])) #startだけ取ってきた
          tmp5 <- overlapsAny(tmp4, Del[[i]])  #If the deletion overlapped at start
          
          if(tmp5){
            comprange2[[i]]<- union(Del[[i]][subjectHits(findOverlaps(tmp4, Del[[i]]))], comprange2[[i]])
          }
          
          tmp6 <- IRanges(start = end(comprange2[[i]]), end = end(comprange2[[i]])) #endだけ取ってきた
          tmp7 <- overlapsAny(tmp6, Del[[i]])  #If the deletion overlapped at end
          
          if(tmp7){
            comprange2[[i]]<- union(Del[[i]][subjectHits(findOverlaps(tmp6, Del[[i]]))], comprange2[[i]])
          }
        }
        
        psa3 <- comprange2
        
        ### 6. get seqence #######################################################
        #In this step, "GGACCTG----AGTT?AGCTT" and "GGACCTG----AGTTCA?CTT" are defferent.
        compSeq <- compareStrings(psa2[[f]])
        
        psa4 <- vector(mode = "character", length = length(compSeq))
        for(i in 1:length(compSeq)){
          psa4[i] <- substr(compSeq[i],start(psa3[[i]]), end(psa3[[i]]))
        }
        
        ### 7. count seqences #############################################################
        #In this step, "GGACCTG----AGTT?AGCTT" and "GGACCTG----AGTTCA?CTT" are defferent.
        psa4_table <- sort(table(psa4), decreasing = TRUE)
        
        ### 8. make_CIGAR #################################################################
        list_CIGAR <- sapply(names(psa4_table), make_CIGAR)
        
        ### 9. molding ########################################
        psa5 <- as.data.frame(psa4_table) 
        psa5$CIGAR <- list_CIGAR          
        
        ### 10. make data #################################################################
        psa6_num <- as.data.frame(tapply(psa5$Freq, psa5$CIGAR, sum) )                                        #the number of each sequence based on the CIGAR
        psa6_subseq    <- as.data.frame(tapply(psa5$psa4, psa5$CIGAR, function(x){paste(x, collapse = ",")})) #the subtype of seqence
        psa6_maxsubseq <- tapply(as.character(psa5$psa4), psa5$CIGAR, function(x){x[1]})                      #the most high freqency subseq
        # psa6_twseq <- NULL
        # for(i in 1:length(row.names(psa6_subseq))){
        #   tmp <- make_seq(row.names(psa6_subseq)[i], Target_ref_seq[f])
        #   psa6_twseq <- c(psa6_twseq, tmp)
        # }
        psa6_par <- psa6_num/colSums(psa6_num)*100
        psa6_CIGAR <- row.names(psa6_par)
        
        ### 11. Indel count ###################################################################
        b <- lapply(strsplit(psa6_CIGAR, "[M, D, I]"),as.numeric)
        c <- lapply(strsplit(psa6_CIGAR, "[0-9]+"), function(x){x[-1]})
        psa6_deletions <- mapply(function(x, y){sum(x[grep("D", y)])}, x=b, y=c) #count Deletion
        psa6_insertions<- mapply(function(x, y){sum(x[grep("I", y)])}, x=b, y=c) #count Insertion
        
        ### 12. Frame or Inframe #############################################################
        psa6_shifts<- unlist(sapply(sapply(psa6_CIGAR, function(x){apply(as.matrix(x), 1, CIG_singl)}), Frame))
        fil1 <- suppressWarnings(!is.na(sapply(strsplit(psa6_CIGAR, "M"), as.numeric)))
        fil2 <- sapply(strsplit(psa6_CIGAR, "M"), length) == 1
        psa6_shifts[which(fil1 & fil2)] <- "WT"
        
        ### 13. get full seqence ###############################################################
        psa4_all <- unlist(psa4) #for i
        psa2_all <- compareStrings(psa2[[f]]) #for i
        
        names(psa2_all) <- psa4_all
        psa6_full_seqence <- psa2_all[psa6_maxsubseq]    #get full seqence from alignmented Strings
        
        ### 14. sumarizing data ################################################################
        CIGER_count_full <- cbind(psa6_maxsubseq, psa6_num, psa6_par, psa6_CIGAR, psa6_deletions, psa6_insertions, psa6_shifts, psa6_full_seqence)
        colnames(CIGER_count_full) <- c("Target window seqence", "# of reads", "% of reads", "CIGAR", "Deletions","Insertions", "In-/Out- of frame", "Full amplicon seqence")
        CIGER_count_full$`Full amplicon seqence` <- gsub("\\?", "N", CIGER_count_full$`Full amplicon seqence`)
        
        ### 15. filtering data ################################################################
        CIGER_count_filter <- CIGER_count_full[CIGER_count_full$`% of reads` > filter_ratio,]
        CIGER_count_filter$`% of reads` <- CIGER_count_filter$`# of reads`/sum(CIGER_count_filter$`# of reads`)*100
        CIGER_count_filtered <- CIGER_count_filter[order(CIGER_count_filter$`% of reads`, decreasing = T),]
        
        CIGER_count_filtereds[[f]] <- CIGER_count_filtered
        
      }
      names(CIGER_count_filtereds) <- Trget_Query_Table$Sample_name 
      
      ### 16. inout table #################################################################################################
      indel_mat <- data.frame(matrix(data =0, nrow=3, ncol=length(Trget_Query_Table$Sample_name)))
      colnames(indel_mat) <- Trget_Query_Table$Sample_name
      row.names(indel_mat) <- c("WT", "In", "Out")
      
      indel_mat[1,] <- sapply(CIGER_count_filtereds, function(x){sum(x[x[,7] == "WT", 3])})
      indel_mat[2,] <- sapply(CIGER_count_filtereds, function(x){sum(x[x[,7] == "In", 3])})
      indel_mat[3,] <- sapply(CIGER_count_filtereds, function(x){sum(x[x[,7] == "Out", 3])})
      
      list_results[[k]] <- list(CIGER_count_filtered=CIGER_count_filtereds, 
                                psa2=psa2, DSB=DSB, indel_mat = indel_mat)
    }
    
    return(list_results)
  }