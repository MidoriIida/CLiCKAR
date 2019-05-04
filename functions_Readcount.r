# 2-2 Read functions for count 

#make_CIGAR
#This function will make CIGAR from charactor vector.
#input:character vector. ex:"CCTCC+++++++A"
#output:CIGAR.           ex:"5M7I1M"

make_CIGAR <- function(x){
  
  y <- gsub("A\\+", "A%\\+", x) 
  y <- gsub("C\\+", "C%\\+", y)
  y <- gsub("T\\+", "T%\\+", y) 
  y <- gsub("G\\+", "G%\\+", y)
  y <- gsub("-\\+", "-%\\+", y)
  y <- gsub("\\?\\+", "\\?%\\+", y)
  
  z <- gsub("\\+A", "\\+%A", y) 
  z <- gsub("\\+C", "\\+%C", z)
  z <- gsub("\\+T", "\\+%T", z) 
  z <- gsub("\\+G", "\\+%G", z)
  z <- gsub("\\+-", "\\+%-", z)
  z <- gsub("\\+\\?", "\\+%\\?", z)
  z <- gsub("\\-\\?", "\\-%\\?", z)
  
  v <- gsub("A-", "A%-", z) 
  v <- gsub("C-", "C%-", v)
  v <- gsub("T-", "T%-", v) 
  v <- gsub("G-", "G%-", v)
  v <- gsub("\\?\\-", "\\?%\\-", v)
  v <- gsub("\\?\\+", "\\?%\\+", v)
  
  w <- gsub("-A", "-%A", v) 
  w <- gsub("-C", "-%C", w)
  w <- gsub("-T", "-%T", w) 
  w <- gsub("-G", "-%G", w) 
  
  adi      <- strsplit(w, "%")
  adi_char <- sapply(adi, nchar)
  ac  <- as.character(adi_char)
  ADI <- rep("M", length(ac))
  A <- grep("^[[:alnum:]]+$", adi[[1]]) #Alphabet
  D <- grep("^-+$", adi[[1]])   #Delition
  I <- grep("^\\++$", adi[[1]]) #Insertion
  ADI[D] <- "D"
  ADI[I] <- "I"
  
  ADI2 <- paste0(ADI, collapse = "")  #combine
  ADI3 <- regexpr("DI", ADI2)         #search DI
  ADI4 <- regexpr("ID", ADI2)         #search DI
  
  ADI5 <- gsub("DI", "DMI", ADI2)     #searh DI & Insert M
  ADI6 <- gsub("ID", "IMD", ADI5)     #searh ID & Insert M
  ADI6 <- strsplit(ADI6, "")[[1]]
  
  if(ADI3[1]>0 & ADI4[1]>0){
    ac2 <-append(ac, 0, after=ADI3)  #searh DI & Insert 0　    
    ac2 <-append(ac2, 0, after=ADI4) #searh ID & Insert 0　    
  }else{
    if(ADI4[1]>0){
      ac2 <-append(ac, 0, after=ADI4) #searh ID & Insert 0　    
    }else{
      if(ADI3[1]>0){
        ac2 <-append(ac, 0, after=ADI3) #searh DI & Insert 0　    
      }else{
        ADI6<- ADI
        ac2 <- ac
      }
    }
  }
  
  CIGAR  <- paste0(paste(ac2, ADI6, sep=""), collapse = "")
  return(CIGAR)
}

#make_CIGAR
#This function will make CIGAR which recognize a substitution from a charactor vector.
#input:character vector. ex:"CCTCC+++++++?A"
#output:CIGAR.           ex:"5M7I1S1M"
make_CIGAR2 <- function(x){
  
  y <- gsub("A\\+", "A%\\+", x) 
  y <- gsub("C\\+", "C%\\+", y)
  y <- gsub("T\\+", "T%\\+", y) 
  y <- gsub("G\\+", "G%\\+", y)
  y <- gsub("-\\+", "-%\\+", y)
  y <- gsub("\\?", "\\%?%", y)
  
  z <- gsub("\\+A", "\\+%A", y) 
  z <- gsub("\\+C", "\\+%C", z)
  z <- gsub("\\+T", "\\+%T", z) 
  z <- gsub("\\+G", "\\+%G", z)
  z <- gsub("\\+-", "\\+%-", z)
  z <- gsub("\\+\\?", "\\+%\\?", z)
  z <- gsub("\\-\\?", "\\-%\\?", z)
  
  v <- gsub("A-", "A%-", z) 
  v <- gsub("C-", "C%-", v)
  v <- gsub("T-", "T%-", v) 
  v <- gsub("G-", "G%-", v)
  v <- gsub("\\?\\-", "\\?%\\-", v)
  v <- gsub("\\?\\+", "\\?%\\+", v)
  
  w <- gsub("-A", "-%A", v) 
  w <- gsub("-C", "-%C", w)
  w <- gsub("-T", "-%T", w) 
  w <- gsub("-G", "-%G", w) 
  
  adi      <- strsplit(w, "%")
  adi_char <- sapply(adi, nchar)
  ac  <- as.character(adi_char)
  ADI <- rep("M", length(ac))
  A <- grep("^[[:alnum:]]+$", adi[[1]]) #Alphabet
  D <- grep("^-+$", adi[[1]])   #Delition
  I <- grep("^\\++$", adi[[1]]) #Insertion
  S <- grep("\\?", adi[[1]])
  ADI[D] <- "D"
  ADI[I] <- "I"
  ADI[S] <- "S"
  
  ADI2 <- paste0(ADI, collapse = "")  #combine
  ADI3 <- regexpr("DI", ADI2)         #search DI
  ADI4 <- regexpr("ID", ADI2)         #search DI
  
  ADI5 <- gsub("DI", "DMI", ADI2)     #searh DI & Insert M
  ADI6 <- gsub("ID", "IMD", ADI5)     #searh ID & Insert M
  ADI6 <- strsplit(ADI6, "")[[1]]
  
  if(ADI3[1]>0 & ADI4[1]>0){
    ac2 <-append(ac, 0, after=ADI3)  #searh DI & Insert 0　    
    ac2 <-append(ac2, 0, after=ADI4) #searh ID & Insert 0　    
  }else{
    if(ADI4[1]>0){
      ac2 <-append(ac, 0, after=ADI4) #searh ID & Insert 0　    
    }else{
      if(ADI3[1]>0){
        ac2 <-append(ac, 0, after=ADI3) #searh DI & Insert 0　    
      }else{
        ADI6<- ADI
        ac2 <- ac
      }
    }
  }
  
  CIGAR  <- paste0(paste(ac2, ADI6, sep=""), collapse = "")
  return(CIGAR)
}

#make_seq
#This function will calculate occupancy from numeric vector
#input: CIGAR and refrence seqence (Target window)
#output:seqence of the target window based on CIGAR
make_seq <- 
  function(CIGAR, Target_ref_seq){
    num <- as.numeric(strsplit(CIGAR, "[A-Z]")[[1]])
    al  <- strsplit(CIGAR, "[0-9]")[[1]]
    al <- al[al != ""]
    
    s <- 1
    e <- 0
    seqs <- ""
    if(al[1] == "D"){#Dから始まる時はどうするか
      a <- paste(rep("-", num[1]), collapse = "")
      b <- substr(Target_ref_seq,  nchar(Target_ref_seq) - num[2] + 1,nchar(Target_ref_seq))
      seqs <- paste(a,b, sep="")
    }else{
      for(i in 1:length(al)){
        if(al[i] == "M"){
          e <- e + num[i]
          a <- substr(Target_ref_seq, s, e) 
          s <- s + num[i]
        }else{
          if(al[i] == "I"){
            a <- paste(rep("+", num[i]), collapse = "")
          }else{
            e <- e + num[i]
            a <- paste(rep("-", num[i]), collapse = "")
            s <- s + num[i]
          }
        }
        #print(c(a, s, e))
        seqs <- paste(seqs, a, sep = "")
      }
    }
    return(seqs)
  }

#culc_occupancy
#This function will calculate occupancy from numeric vector
#input:  read count . ex:100, 30, 20, 0
#Output: occupancy  . ex:0.66, 0.20, 0.13, 0
culc_occupancy <- 
  function(x){
    occupancy_all <- NULL
    C    <- 0
    for(i in 1:length(x)){
      C <- C+x[i]
      occupancy_all <- c(occupancy_all, C/sum(x))
    }
    return(occupancy_all)
  }


#CIG_singl
#This function will take CIGAR and output number
#input: CIGAR
#output: total number of delition and inserstion
CIG_singl <- function (CIGAR) {
  B <- lapply(strsplit(CIGAR, "[M, D, I]"),as.numeric)        
  C <- lapply(strsplit(CIGAR, "[0-9]+"), function(x){x[-1]})  
  
  s <- 0
  
  for(i in 1:length(B[[1]])){
    if(C[[1]][i] == "M"){
      s <- s
    }else{
      if(C[[1]][i] == "I"){
        s <- s + B[[1]][i]
      }else{
        s <- s - B[[1]][i]
      }
    }
  }
  return(s)
}

#Frame
#This function will take number and output frameshift or not
#input: total number of delition and inserstion
#output: frameshift or Inframeshift

Frame <- function (A) {
  B <- rep(0, length(A))
  B[A == 0] <- "In"
  B[A %% 3 == 0 & B != "-"] <- "In"
  B[A %% 3 != 0] <- "Out"
  return(B)
}


#CIGfix
#This function will fix CIGER in out of DSB range (only from front side)
#input: strsplited CIG num, strsplited CIG seq and a numeric vector
#output: fixed CIG num, fixed CIG seq 
CIGfix <- 
  function(CIG_num, CIG_sep, point){
    while(TRUE){
      fst_front_B  <- sapply(CIG_num, function(x){x[1]})    #number of first nucleotide when splited by M, D, or I
      sec_front_B  <- sapply(CIG_num, function(x){x[2]})    #number of second nucleotide when splited by M, D, or I
      
      ext_front_B  <- fst_front_B <= point                   #Indel or insertion located in a place shorter than the set position
      lon_front_B  <- (fst_front_B + sec_front_B) <= point   #Indel or insertion dosen't located in a place over than the set position
      lon_front_B[is.na(lon_front_B)] <- FALSE
      
      if(sum(lon_front_B) == 0) break
      
      sec <- sapply(CIG_sep[(ext_front_B & lon_front_B)], function(x){paste(x[1:3], sep="", collapse = "")})
      
      kae               <- CIG_num[(ext_front_B & lon_front_B)]
      kae[sec == "MDM"] <- sapply(CIG_num[(ext_front_B & lon_front_B)][sec == "MDM"], function(x){sum(x[1:3])})
      kae[sec == "MIM"] <- sapply(CIG_num[(ext_front_B & lon_front_B)][sec == "MIM"], function(x){sum(x[c(1,3)])})
      
      rest  <- sapply(CIG_num[(ext_front_B & lon_front_B)],function(x){if(length(x)>3){x[4:length(x)]}else{x <- NULL}}) #extract latter half
      
      if(is.list(kae) & is.list(rest)){
        tmp1  <- mapply(FUN = function(x,y){append(x, y)},x=kae, y=rest)
        tmp2  <- sapply(CIG_sep[(ext_front_B & lon_front_B)], function(x){if(length(x)>=3){x[3:length(x)]}else{x}}) #thied or later CIGAR
        CIG_num[(ext_front_B & lon_front_B)] <- tmp1
        CIG_sep[(ext_front_B & lon_front_B)] <- tmp2
        
      }else{
        if(is.list(kae) & is.matrix(rest)){
          tmp1 <- lapply(apply(rbind(kae, rest), 2, function(x){list(as.numeric(x))}) , function(x){unlist(x)})
          tmp2 <- sapply(CIG_sep[(ext_front_B & lon_front_B)], function(x){if(length(x)>=3){x[3:length(x)]}else{x}})
          CIG_num[(ext_front_B & lon_front_B)] <- tmp1
          CIG_sep[(ext_front_B & lon_front_B)] <- lapply(apply(tmp2,2,list),unlist)
          
        }else{
          tmp1 <- append(as.vector(unlist(kae)), as.vector(unlist(rest)))
          tmp2 <- as.vector(sapply(CIG_sep[(ext_front_B & lon_front_B)], function(x){if(length(x)>=3){x[3:length(x)]}else{x}})) 
          CIG_num[(ext_front_B & lon_front_B)][[1]] <- tmp1
          CIG_sep[(ext_front_B & lon_front_B)][[1]] <- tmp2
        }
      }
    }
    return(list(CIG_num, CIG_sep))
  }

#CIGfix
#This function will fix CIGER in out of DSB range (only from back side)
#input: strsplited CIG num, strsplited CIG seq and a numeric vector
#output: fixed CIG num, fixed CIG seq 

CIGfix_rev <- 
  function(CIG_num, CIG_sep, point){

    long <- which(sapply(CIG_num, length)>3)                        #Retrieve CIGAR consisting of 4 or more characters
    if(length(long) == 0 ){
      cat("No reads for fix")
      return(list(CIG_num, CIG_sep))
    }else{
      Tec <- lapply(CIG_num[long], function(x){cumsum(x) <= point}) #Retrieve the number behind point (back)
      if(sum(unlist(Tec) == FALSE) == 0){
        cat("No reads for fix")
        return(list(CIG_num, CIG_sep))
        
      }else{
        s1 <- lapply(Tec, function(x){which(x == FALSE)[1]})
        
        for(i in 1:length(s1)){
          fst_back_B <- sapply(CIG_num[long[[i]]], function(x){x[s1[[i]][1]:length(x)]}) #Specify the number you want to process
          fst_back_C <- sapply(CIG_sep[long[[i]]], function(x){x[s1[[i]][1]:length(x)]}) #Specify the character to be processed
          
          if(length(fst_back_B) > 1 & fst_back_C[1]=="M"){
            fst_front_B <- sapply(CIG_num[long[[i]]], function(x){x[1:(s1[[i]][1]-1)]}) #Specify the number you want to leave
            fst_front_C <- sapply(CIG_sep[long[[i]]], function(x){x[1:(s1[[i]][1]-1)]}) #Specify the letter you want to learn
            
            num_sum <- tapply(fst_back_B, fst_back_C, sum)# Ignore i, add one when D or M
            
            if(is.na(num_sum["D"])){
              kae_B <- num_sum["M"]
            }else{
              kae_B <- num_sum["M"] + num_sum["D"]
            }
            kae_C <- "M"
            
            kae_B <- apply(rbind(fst_front_B, kae_B), 1, as.numeric)
            names(kae_B) <- NULL
            
            kae_C <- apply(rbind(fst_front_C, kae_C), 1, as.character)
            names(kae_C) <- NULL
            
            CIG_num[long[[i]]][[1]] <-  kae_B
            CIG_sep[long[[i]]][[1]] <- kae_C
          }
        }
        
        return(list(CIG_num, CIG_sep))
        
      }
      }
      
  }


#checkpeimerlens
#This function will fix read_data when the primer length is diffferent with others
#input: list of DNAStringSets and a numeric vector
#output: list of DNAStringSets  

checkpeimerlens <- 
  function(tmp, primer_length, direction="FWD"){
    
    if(direction=="FWD"){

      gr_ma <- as.numeric(names(which.max(table(primer_length))))                        #Number of barcode & primer in the most commn group 
      gr_mi <- as.numeric(names(table(primer_length)[-which.max(table(primer_length))])) #Number of barcode & primer in the other groups
      
      for(i in 1:length(gr_mi)){
        primer_m <- which(primer_length == gr_mi[i])
        
        for(j in 1:length(primer_m)){
          if(which.max(c(gr_ma, gr_mi[i])) == 1){ #when gr_mx grather than gr_mi
            letnum <- gr_ma - gr_mi[i]            #Number of letters which is lacking in the dnatringset
            letbew <- substr(as.character(tmp[[which(primer_length == gr_ma)[1]]][1]), 1, letnum) #Letters which is lacking in the dnastringset
            tmp[[primer_m[j]]] <- DNAStringSet(paste(letbew, tmp[[primer_m[j]]], sep="")) #Add the letthers for the dnastringset
          }else{ #when gr_mx less than gr_mi
            letnum <- gr_mi[i] - gr_ma
            letbew <- nchar(substr(as.character(tmp[[which(primer_length == gr_ma)[1]]][1]), 1, letnum)) + 1
            tmp[[primer_m[j]]] <- DNAStringSet(substr(as.character(tmp[[primer_m[j]]]), letbew, nchar(as.character(tmp[[primer_m[j]]])))) #shave the surplus letthers 
            
          }
        }
        
      }
      return(tmp)
    }else{

      gr_ma <- as.numeric(names(which.max(table(primer_length))))                        #Number of barcode & primer in the most commn group
      gr_mi <- as.numeric(names(table(primer_length)[-which.max(table(primer_length))])) #Number of barcode & primer in the other groups
      
      for(i in 1:length(gr_mi)){
        primer_m <- which(primer_length == gr_mi[i])
        
        for(j in 1:length(primer_m)){
          if(which.max(c(gr_ma, gr_mi[i])) == 1){ #when gr_mx grather than gr_mi
            letnum <- gr_ma - gr_mi[i]            #Number of letters which is lacking in the dnatringset
            rev_tmp <- as.character(tmp[[which(primer_length == gr_ma)[1]]][1]) #reverse the dnstrings
            letbew  <- substr(rev_tmp, 1, letnum) #Letters which is lacking in the dnastringset
            tmp[[primer_m[j]]] <- DNAStringSet(paste(as.character(tmp[[primer_m[j]]]), letbew, sep="")) #Add the letthers for the dnastringset
          }else{ #When gr_mx is less
            letnum <- gr_mi[i] - gr_ma
            letbew <- nchar(substr(as.character(tmp[[which(primer_length == gr_ma)[1]]][1]), 1, letnum))  #surplus letthers
            tmp[[primer_m[j]]] <- DNAStringSet(substr(as.character(tmp[[primer_m[j]]]), 1, nchar(as.character(tmp[[primer_m[j]]])) - letbew)) #shave the surplus letthers 
            
          }
        }
        
      }
      return(tmp)
    }
    
    
  }
