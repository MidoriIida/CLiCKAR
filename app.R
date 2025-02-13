library(shiny)
library(shinythemes)
library(ShortRead)
library(Biostrings)
library(ggplot2)
library(gridExtra)
library(grid)
library(openxlsx)
# library(shinyjs) 
options(shiny.maxRequestSize = 400*1024^2)
# options(repos = BiocInstaller::biocinstallRepos())
# getOption("repos")

source("functions_demultiplex.r")
source("functions_makecigar.r")
source("functions_Readcount.r")
source("functions_batchmode.r")
sample_query_table_for_download   <- read.csv("sample_query_table_for_Download.csv")
sample_for_download               <- readFastq("sample.fastq.gz")

shinyApp(
  ui = tagList(
    #shinythemes::themeSelector("cerulean"),
    navbarPage(
      theme = shinytheme("cerulean"),  # <--- To use a theme, uncomment this
      "CLiCKAR v2.0.0",                # development version v6.0.0
      
      tabPanel("Step 1",
               sidebarPanel(
                 #useShinyjs(), #new!
                 p("Step 1 procedure", style = "font-family: 'Helvetica'; font-size: 16pt"),
                 
                 p("0) If necessary, download sample data files from 'get sample!' button below."),
                 downloadButton("CLiCKAR_sample.zip", "get sample!"),
                 br(), br(), br(),
                 
                 p("1) Browse your files from your computer through the left panel."),
                 
                 #h4("[Required parameters]"),
                 fileInput('query_table', 'Select query table (.csv)', #1.Select query table (.csv)
                           accept=c('csv', 
                                    'text/comma-separated-values,text/plain',
                                    '.csv')),
                 
                 fileInput('marged_fastq', 'Select fastq file (.fastq or .fastq.gz)', #2.Select fastq file (.fastq or .fastq.gz)
                           accept=c('fastq',
                                    'fastq.gz'
                           )),
                 
                 
                 p("2) Click 'CLiCKAR submit!' button below."),
                 actionButton("go", div(img(src="CLiCKAR.png", height = 40), "submit!")),
                 
                 br(), br(), br(),
                 p("3) Click 'Download' button and get data files for Step 2."),
                 # p("The downloded file contains four files as described below.", style = "font-family: 'times'; font-size: 10pt"),
                 # p("- demultiplexed read files (.fastq.gz).", style = "font-family: 'times'; font-size: 10pt"),
                 # p("- read counts of each sample (count_result.csv).", style = "font-family: 'times'; font-size: 10pt"),
                 # p("- a bar plot of the read counts (counts_readcounts.pdf)", style = "font-family: 'times'; font-size: 10pt"),
                 # p("- a demultiplexed FASTQ data file (demultiplexed_fastq.dat): as input in Step 2.", style = "font-family: 'times'; font-size: 10pt"),
                 downloadButton("CLiCKAR_demultiplexed.zip", "Download")
               ),
               mainPanel(
                 plotOutput("gmplot", width = 800, height = 800)
               )
      ),
      
      tabPanel("Step 2", 
               
               sidebarPanel(
                 # useShinyjs(), #new!
                 p("Step 2 procedure", style = "font-family: 'Helvetica'; font-size: 16pt"),
                 
                 h4("[Required parameters]"),
                 p("1) Browse and upload your 'demultiplexed_fastq.dat' file downloded at Step 1 from your computer through the panel below."),
                 fileInput('datafile', 'Select demultiplexed_fastq.dat (.dat)',accept='.dat'),
                 
                 p("2) Select your target gene from the drop-down menu."),
                 htmlOutput("selectUI"),
                 
                 br(),  br(), #br(), #br(),
                 h4("[Optional parameters]"),
                 p("3) Set minimum frequency (%) to omit sequence errors and rare reads."),
                 
                 numericInput('filter_ratio', 
                              label = "Set minimum frequency (%)",
                              value = 0.1, min = 0, max = 100,  step = 0.01),
                 
                 p("4) Set target window (bp) to identified mutations around the DSB site."),
                 numericInput('Range', 
                              label = "Set target window (bp)",
                              value = 10, min = 0, step = 1),
                 
                 br(),  br(), #br(),
                 p("5) Click 'CLiCKAR submit!' button."),
                 
                 actionButton("go2", div(img(src="CLiCKAR.png", height = 40), "submit!")),
                 
                 p("6) Click 'Download' button and get compressed data file containing four files."),
                 # p("- 'count.xlsx' file: the detail of mutation profiles in each sample.", style = "font-family: 'times'; font-size: 10pt"),
                 # p("- 'indel_position.pdf'  file: the putative position of the DSB.", style = "font-family: 'times'; font-size: 10pt"),
                 # p("- 'mutation_rate.csv'  file: the percentages of wild-, in-, or out-of-frame.", style = "font-family: 'times'; font-size: 10pt"),
                 # p("- 'mutation_rate.pdf' file: graphical report of 'muration_rate.csv'", style = "font-family: 'times'; font-size: 10pt"),
                 downloadButton("CLiCKAR_calculated.zip", "Download")
                 
                 
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Table 1", 
                            textOutput("tablesubtilte2"),
                            dataTableOutput('fixCIGARcount')
                   ),
                   
                   tabPanel("Figure 1", 
                            plotOutput("DSBhist", width = 600)
                            
                   ),
                   tabPanel("Figure 2",
                            plotOutput("INoutframe", width = 600, height = 450)
                   )
                 )
               )
      ),
      
      tabPanel("Batch mode",
               
               sidebarPanel(
                 #useShinyjs(),
                 
                 p("Batch mode procedure", style = "font-family: 'Helvetica'; font-size: 16pt"),
                 
                 p("0) If necessary, download sample data files from 'get sample!' button below."),
                 downloadButton("CLiCKAR_sample.zip", "get sample!"),
                 br(), br(), br(),
                 
                 p("1) Browse your files from your computer through the left panel."),
                 
                 fileInput('batch_query_table', 'Select query table (.csv)', #1.Select query table (.csv)
                           accept=c('csv', 
                                    'text/comma-separated-values,text/plain',
                                    '.csv')),
                 
                 fileInput('batch_marged_fastq', 'Select fastq file (.fastq or .fastq.gz)', #2.Select fastq file (.fastq or .fastq.gz)
                           accept=c('fastq',
                                    'fastq.gz'
                           )),
                 
                 h4("[Optional parameters]"),
                 p("2) Set minimum frequency (%) to omit sequence errors and rare reads."),
                 
                 numericInput('batch_filter_ratio', 
                              label = "Set minimum frequency (%)",
                              value = 0.1, min = 0, max = 100,  step = 0.01),
                 
                 p("3) Set target window (bp) to identified mutations around the DSB site."),
                 numericInput('batch_Range', 
                              label = "Set target window (bp)",
                              value = 10, min = 0, step = 1),
                 
                 br(),  br(), #br(),
                 p("4) Click 'CLiCKAR submit!' button."),
                 
                 actionButton("go3", div(img(src="CLiCKAR.png", height = 40), "submit!")),
                 
                 p("5) Click 'Download' button and get compressed data file containing four files."),
                 downloadButton("batch_CLiCKAR_calculated.zip", "Download")

               ),
               
               mainPanel(
                 plotOutput("batch_gmplot", width = 800, height = 800)
               )
      ),
      
      tabPanel("Usage tutorial",
               fluidPage(titlePanel("Hello CLiCKAR!"),
                         
                         h4("1) Prepare your 'query table (.csv)'."),
                         img(src = "sample_qery_table.png", height = 144, width = 850),
                         #
                         br(), br(),
                         p("A comma-separated values (csv) data sheet ('query_table.csv'; details in Table 2) 
                           is needed to run CLiCKAR. Query table requires gene name (Gene), 
                           sample name for individual crispant (Sample_name), forward (FWD_primer) 
                           and reverse (REV_primer) primer sequences with custom barcoded sequence (underlined) 
                           without Illumina overhang adaptor sequence, full reference sequence of target amplicon 
                           (Reference_sequence), and protospacer sequence with protospacer adjacent motif (PAM) 
                           (Protospacer_sequence_plus_PAM) in each amplicon region of on-target sites.",
                           style = "font-family: 'times'; font-size: 12pt"),
                         
                         br(),br(),
                         h4("2) Prepare your 'fastq file (.fastq or .fastq.gz)'."),
                         
                         withTags({
                           div(class="body", checked=NA,
                               p("CLiCKAR accepts only single-end sequencing data. 
                                 When paired-end sequencing data are used, both stranded reads must be joined 
                                 by fastq-join or Pear in Galaxy (",
                                 a(href="https://usegalaxy.org", "https://usegalaxy.org"),
                                 "); alternatively, 
                                 one of the paired-end reads must be used in isolation. In the latter case, 
                                 amplicons must be sequenced from end to end, because CLiCKAR must recognize 
                                 both locus-specific primer sequences at the both ends during demultiplexing process.",
                                 style = "font-family: 'times'; font-size: 12pt")
                               )
                         }),
                         
                         br(),br(),
                         h4("3) Run CLiCKAR in each step accordance with the instructions."),
                         
                         tags$b("Step 1 procedure", style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         p("0) If necessary, download sample data files from 'get sample!' button below.",     style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         div(downloadButton("CLiCKAR_sample2.zip", "get sample!"),                             style = "padding-left: 15em"),
                         
                         br(),
                         p("1) Browse your files from your computer through 'Select query table' panel.",      style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         p("1-1) Select your query table (.csv).",                                             style = "font-family: 'times'; font-size: 12pt; padding-left: 4em"),
                         p("1-2) Select your fastq file (.fastq or .fastq.gz).",                               style = "font-family: 'times'; font-size: 12pt; padding-left: 4em"),                         
                         
                         br(),
                         p("2) Click 'CLiCKAR submit!' button.",                                               style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         
                         br(),
                         p("3) Click 'Download' button and get data files for Step 2.",                        style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         p("The downloded file contains four files as described below.",                       style = "font-family: 'times'; font-size: 12pt; padding-left: 3em"),
                         p("- '.fastq.gz' file: demultiplexed read files.",                                          style = "font-family: 'times'; font-size: 10pt; padding-left: 4em"),
                         p("- 'count_result.csv' file: read counts of each sample.",                                 style = "font-family: 'times'; font-size: 10pt; padding-left: 4em"),
                         p("- 'counts_readcounts.pdf' file: a bar plot of the read counts",                          style = "font-family: 'times'; font-size: 10pt; padding-left: 4em"),
                         p("- 'demultiplexed_fastq.dat' file: a demultiplexed FASTQ data file as input data in Step 2.", style = "font-family: 'times'; font-size: 10pt; padding-left: 4em"),
                         
                         br(),
                         tags$b("Step 2 procedure", style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         br(),
                         p("1) Browse your 'demultiplexed_fastq.dat' file at Step 1 from your computer through the data upload panel.",      style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         
                         br(),
                         p("2) Select your target gene from the drop-down menu.",                               style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         
                         br(),
                         p("3) Set minimum frequency (%) to omit sequence errors and rare reads.",              style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         
                         br(),
                         p("4) Set target window (bp) to identified mutations around the ddouble strand break (DSB) site.", style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         
                         br(),
                         p("5) Click 'CLiCKAR submit!' button.",                                                 style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         
                         br(),
                         p("6) Click 'Download' button and get a data file containing four files below.",       style = "font-family: 'times'; font-size: 12pt; padding-left: 2em"),
                         p("- 'count.xlsx'        file: the detail of mutation profiles in each sample.",       style = "font-family: 'times'; font-size: 10pt; padding-left: 4em"),
                         p("- 'indel_position.pdf'file: the putative position of the DSB.",                     style = "font-family: 'times'; font-size: 10pt; padding-left: 4em"),
                         p("- 'mutation_rate.csv' file: the percentages of wild-, in-, or out-of-frame.",       style = "font-family: 'times'; font-size: 10pt; padding-left: 4em"),
                         p("- 'mutation_rate.pdf' file: graphical report of 'muration_rate.csv'",               style = "font-family: 'times'; font-size: 10pt; padding-left: 4em")
                         
                         )
               
                         ),         
      tabPanel("Information",
               h5("How to cite:"),
               p("Please refer to our manuscript 'CLiCKAR: a web tool for practical genotyping and evaluation of 
                 CRISPR-Cas9-based knock-out phenotypes using multiplexed amplicon sequencing. Iida et al., in submission."),
               br(),
               h5("Version:"),
               p("v1.0.0 (March, 2019): released"),
               
               br(),
               h5("Contact:"),
               p("iida.midori517[at]mail.kyutech.jp")
               )
               )
      ),
  
  server = 
    function(input, output) {
      #step1
      
      output$CLiCKAR_sample.zip <- downloadHandler(
        filename = function() {
          paste('CLiCKAR-sample-', Sys.Date(), '.zip', sep='')
        },
        content = function(con3) {
          tmpdir <- tempdir()
          file.remove(dir(tmpdir, full.names=TRUE))
          setwd(tmpdir)
          
          fs <- c("sample_query_table.csv", "sample.fastq.gz")
          
          write.csv(sample_query_table_for_download, file=fs[1], row.names = F)
          writeFastq(sample_for_download, file=fs[2], compress = TRUE, mode="w")
          
          zip(zipfile=con3, files=fs)
        }
      )
      
      fastq <- eventReactive(input$go,{
        if (is.null(input$query_table)|is.null(input$marged_fastq)) 
          return(NULL)
        
        withProgress(message = 'caluclation in progress 2/2',
                     detail = 'This may take a while...', value = 0, {
                       
                       inFile  <- input$query_table
                       inFile2 <- input$marged_fastq
                       
                       for (i in 1:25) {
                         incProgress(1/50)
                         Sys.sleep(0.5)
                       }
                       
                       files <- demultiplex(inFile$datapath, read=inFile2$datapath) 
                       
                       for (i in 26:50) {
                         incProgress(1/50)
                         Sys.sleep(0.5)
                       }
                       
                       return(files)
                       
                     })
      })
      
      results <- eventReactive(input$go,{
        if (is.null(input$query_table)|is.null(input$marged_fastq))
          return(NULL)
        
        withProgress(message = 'caluclation in progress 1/2',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:50) {
                         incProgress(1/50)
                         Sys.sleep(0.25)
                       }
                     })
        
        
        inFile  <- input$query_table
        Barcode_Ref_Table <- read.csv(file=inFile$datapath, stringsAsFactors = FALSE)
        
        Read_counts <- sapply(fastq(), length)
        readcounts <- cbind(Barcode_Ref_Table, Read_counts)
        
        return(readcounts)
      })
      
      gs2 <- eventReactive(input$go,{
        
        if (is.null(input$query_table)|is.null(input$marged_fastq)) #|is.null(input$trim_length)
          return(NULL)
        
        withProgress(message = 'Making plots 1/2',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.25)
                       }
                     })
        
        
        amplicon <- results()$Sample_name
        variable <- results()$Gene
        value    <- results()$Read_counts
        
        #plot each read count
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
        
      })
      
      output$tablesubtilte <- renderText(if(is.null(input$query_table)|is.null(input$marged_fastq)){
        "Step 1: Download sample data from 'get sample!' button below and submit the sample files through left panel."
        #"This is test"
      })
      
      output$gmplot <- renderPlot({
        
        if (is.null(input$query_table)|is.null(input$marged_fastq))
          return(NULL)
        
        grid.arrange(gs2(), nrow=1)
        
      })
      
      output$CLiCKAR_demultiplexed.zip <- downloadHandler(
        filename = function() {
          paste('CLiCKAR-demultiplexed-', Sys.Date(), '.zip', sep='')
        },
        content = function(con) {
          tmpdir <- tempdir()
          file.remove(dir(tmpdir, full.names=TRUE))
          setwd(tmpdir)
          
          inFile  <- input$query_table
          Query_Table <- read.csv(file=inFile$datapath, stringsAsFactors = FALSE)
          
          fs <- c("count_result.csv", "counts_readcounts_total_ratio.pdf" ,
                  "counts_readcounts.pdf",  "demultiplexed_fastq.dat")
          
          for(i in 1:nrow(Query_Table)){
            trgene   <- Query_Table$Gene[i] #new! v2.0.0
            name     <- Query_Table$Sample_name[i]
            file_name <- file_name <- paste(trgene, sprintf("%s.fastq.gz", name), sep="-") #new! v2.0.0
            writeFastq(fastq()[[i]], file=file_name, compress = TRUE, mode="w")
            fs <- c(fs, file_name)
          }
          
          data <- list(query_table=Query_Table, demultiplexed_fastq=fastq())
          
          write.csv(results(), file=fs[1])
          ggsave(file = fs[3], plot = gs2(), width=8, height=8)
          save(data, file = fs[4]) 
          zip(zipfile=con, files=fs)
          
        }
      )
      
      observe({ #new!
        shinyjs::toggleState("go", !is.null(input$marged_fastq) && !is.null(input$query_table)) #output$CLiCKAR_demultiplexed.zip
      })
      
      observe({ #new!
        shinyjs::toggleState("CLiCKAR_demultiplexed.zip", input$go) #output$CLiCKAR_demultiplexed.zip #input$go
      })
      
      #step2
      filedata <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
          # User has not uploaded a file yet
          return(NULL)
        }
        get(load(infile$datapath))
      })
      
      resultCIGs <- eventReactive(input$go2,{
        if (is.null(input$datafile)|is.null(input$targetName))
          return(NULL)
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:90) {
                         setProgress(i/100)
                         Sys.sleep(0.5)
                       }
                       data <- filedata()
                       result <- 
                         AddCIGAR(data, 
                                  target=input$targetName, 
                                  filter_ratio=as.numeric(input$filter_ratio),
                                  Range = as.numeric(input$Range))
                     })
        
        return(result)
      })
      
      gs_resultfixedInOut <- eventReactive(input$go2,{
        if (is.null(input$datafile)|is.null(input$targetName))
          return(NULL)
        
        Indel_mat   <- melt(as.matrix(resultCIGs()[[4]]))
        q  <- ggplot(Indel_mat, aes(x=Var2, y=value, fill=Var1)) 
        q <- q + geom_bar(stat="identity") 
        q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + ylab("% of total reads") + xlab("Sample name") +
          theme(legend.title = element_blank(),legend.text = element_text(size=15),
                axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
                axis.title.x = element_text(size=15), axis.title.y = element_text(size=15))
      })
      
      gs_resultDSBhistogram <- eventReactive(input$go2,{
        if (is.null(input$datafile)|is.null(input$targetName))
          return(NULL)
        
        psa2 <- resultCIGs()[[2]]
        DSB  <- resultCIGs()[[3]]
        
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
      })
      
      output$tablesubtilte2 <- renderText(if(is.null(input$datafile)|is.null(input$targetName)){"Result will be shown here."})
      
      output$selectUI <- renderUI({ 
        data <- filedata()
        if (is.null(data)) 
          return(selectInput("targetName", "Select your target gene", choices = "NULL") ) #
        
        items=unique(data$query_table$Gene)
        selectInput("targetName", "Select your target gene", choices = items) 
      })
      
      output$fixCIGARcount<- renderDataTable({ 
        data <- resultCIGs()[[1]]
        return(data[[1]])
      })
      
      output$DSBhist      <- renderPlot({
        plist <- gs_resultDSBhistogram()
        grid.arrange(grobs = plist[1], ncol=1)
      })
      
      output$INoutframe   <- renderPlot({
        grid.arrange( gs_resultfixedInOut(),nrow=1)
      })
      
      output$CLiCKAR_calculated.zip <- downloadHandler(
        filename = function() {
          paste(input$targetName,'_CLiCKAR-calculated-', Sys.Date(), '.zip', sep='')
        },
        content = function(con2) {
          tmpdir <- tempdir()
          file.remove(dir(tmpdir, full.names=TRUE))
          setwd(tmpdir)
          
          target <- input$targetName
          fn <- c("_count.xlsx",        
                  "_mutation_rate.pdf", 
                  "_Indel_position.pdf",
                  "_mutation_rate.xlsx" 
          )
          fs2 <- paste(target, fn, sep="")
          
          plist <- gs_resultDSBhistogram()
          
          write.xlsx(resultCIGs()[[1]], file=fs2[1], sheetName=names(resultCIGs()[[1]]))
          write.xlsx(resultCIGs()[[4]], file=fs2[4], row.names=TRUE)
          
          ggsave(file=  fs2[2], plot = gs_resultfixedInOut(), width=8.50, height=8.50)
          pdf(file=  fs2[3], height=8.50, width=8.50)
          print(plist)
          dev.off()
          
          zip(zipfile=con2, files=fs2)
          
        }
      )
      
      observe({ #new!
        shinyjs::toggleState("go2", !is.null(input$datafile)) #output$CLiCKAR_demultiplexed.zip
      })
      
      observe({ #new! #
        shinyjs::toggleState("CLiCKAR_demultiplexed.zip", input$go2) #output$CLiCKAR_demultiplexed.zip
      })
      
      #batch mode
      batch_results <- eventReactive(input$go3,{
        if (is.null(input$batch_query_table)|is.null(input$batch_marged_fastq)) 
          return(NULL)
        
        withProgress(message = 'caluclation in progress 2/2',
                     detail = 'This may take a while...', value = 0, {
                       
                       inFile  <- input$batch_query_table
                       inFile2 <- input$batch_marged_fastq
                       
                       #step1 input data
                       Query_Table <- read.csv(file=inFile$datapath, stringsAsFactors = FALSE)
                       variable    <- Query_Table$Gene
                       
                       for (i in 1:25) {
                         incProgress(1/50)
                         Sys.sleep(0.5)
                       }
                       
                       #step1 calculation
                       files       <- demultiplex(inFile$datapath, read=inFile2$datapath)
                       
                       #step1 data output
                       data        <- list(query_table=Query_Table, demultiplexed_fastq=files)
                       
                       
                       #step2 input the data
                       targetNames         <- unique(variable)
                       list_results        <- vector(mode = "list", length = length(targetNames))
                       names(list_results) <- targetNames
                       
                       #step2 calculation
                       for(i in 1:length(targetNames)){
                         targetName = targetNames[i]
                         list_results[[i]] <- 
                           AddCIGAR(data, 
                                    target = targetName, 
                                    filter_ratio = as.numeric(filter_ratio),
                                    Range = as.numeric(Range))
                       }
                       
                       for (i in 26:50) {
                         incProgress(1/50)
                         Sys.sleep(0.5)
                       }
                       
                       return(list(files = files, list_results = list_results))
                       
                     })
      })
      
      output$batch_CLiCKAR_calculated.zip <- downloadHandler(
        filename = function() {
          paste('CLiCKAR-batch_calculated-', Sys.Date(), '.zip', sep='')
        },
        
        content = function(con3) {
          tmpdir <- tempdir()
          file.remove(dir(tmpdir, full.names=TRUE))
          setwd(tmpdir)
          
          resultCIGs  <- batch_results()[[2]]
          Query_Table <- read.csv(file=inFile$datapath, stringsAsFactors = FALSE)
          variable    <- Query_Table$Gene
          targetNames <- unique(variable)
          fn <- c("_count.xlsx",        
                  "_mutation_rate.pdf", 
                  "_Indel_position.pdf",
                  "_mutation_rate.xlsx" 
          )
          fs3 <- lapply(targetNames, function(x){paste(x, fn, sep="")})
          
          #output file 
          Read_counts <- sapply(batch_results()[[1]], length)
          readcounts  <- cbind(Query_Table, Read_counts)
          
          #output plot 
          amplicon    <- readcounts$Sample_name
          value       <- readcounts$Read_counts
          
          plot_countread <- plot_counts_readcounts(amplicon = amplicon, variable = variable, value = value)
          
          list_plot_mutation_rate  <- lapply(resultCIGs, plot_gs_resultfixedInOut)   #fs2[2]
          list_plot_Indel_position <- lapply(resultCIGs, plot_gs_resultDSBhistogram) #fs2[3]
          
          
          for(i in 1:length(targetNames)){
            write.xlsx(resultCIGs[[i]][[1]], file=fs3[[i]][1], sheetName=names(resultCIGs[[i]][[1]]))
            write.xlsx(resultCIGs[[i]][[4]], file=fs3[[i]][4], row.names=TRUE)
            
            ggsave(file = fs3[[i]][2], plot = list_plot_mutation_rate[[i]], width=8.50, height=8.50)
            
            pdf(file = fs3[[i]][3], height=8.50, width=8.50)
            print(list_plot_Indel_position[[i]])
            dev.off()
          }
          
          zip(zipfile=con3, files=unlist(fs3))
          
        }
      )
      
      observe({ #new!
        shinyjs::toggleState("go3", !is.null(input$batch_marged_fastq) && !is.null(input$batch_query_table)) #output$CLiCKAR_demultiplexed.zip
      })
      
      
      observe({ #new! #
        shinyjs::toggleState("batch_CLiCKAR_calculated.zip", input$go3) #output$CLiCKAR_demultiplexed.zip
      })
      
      
      #Usage
      output$CLiCKAR_sample2.zip <- downloadHandler(
        filename = function() {
          paste('CLiCKAR-sample-', Sys.Date(), '.zip', sep='')
        },
        content = function(con3) {
          tmpdir <- tempdir()
          file.remove(dir(tmpdir, full.names=TRUE))
          setwd(tmpdir)
          
          fs <- c("sample_query_table.csv", "sample.fastq.gz")
          
          write.csv(sample_query_table_for_download, file=fs[1], row.names = F)
          writeFastq(sample_for_download, file=fs[2], compress = TRUE, mode="w")
          
          zip(zipfile=con3, files=fs)
        }
      )
    }
  )
