##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

tab_about <- tabItem(
  tabName = "about",
  tagList(
    h1(HTML("<font color=\"#225F8B\">The Nephgen </font>
            <font color=\"#971037\">sc</font><font color=\"#225F8B\">Explorer</font> "), 
       style = "margin-top: 0px; margin-bottom: 20px;"),
    
    
    h4("The single-cell explorer \"scEplorer\" is a searchable database to visualize 
      gene expression from RNA-sequencing."),
    h4(HTML("Its intended use is to make datasets generated 
      within the NephGen Initiative (CRC 1453) accessible and searchable,
      for all researchers within the initiative and to thereby promote scientific 
      collaboration and progress. It allows for downloading of plots, as well as lists of marker genes. <br>
      Upon publication the datasets will also be publicly accessible via the scExplorer.")),
    
    h4("For demonstration, it currently contains data from two published single-cell 
      RNA-seq experiments of human kidney:"),
    tags$ul(
      tags$li(h4(em("Wu et al:"), style="margin-bottom: 0px;"),
              h4(HTML("Data from 4525 nuclei. Kidney tissue from a single human individual."), 
                 style =" margin-top: 0px; margin-bottom: 0px;"),
                      
              p(HTML("Wu H, Uchimura K, Donnelly EL, Kirita Y, Morris SA, Humphreys BD. 
                      Comparative Analysis and Refinement of Human PSC-Derived Kidney Organoid Differentiation with Single-Cell 
                      Transcriptomics. <br> 
                      Cell Stem Cell. 2018;23(6):869-881.e8. doi:10.1016/j.stem.2018.10.010"))
 
              ),
      tags$li(h4(em("Lake et al:"), style="margin-bottom: 0px;"),
              h4(HTML("Data from 17659 nuclei. Kidney tissue from 15 different human individuals."), 
                  style="margin-top: 0px; margin-bottom: 0px;")),
              p(HTML("Lake BB, Chen S, Hoshi M, et al. 
                      A single-nucleus RNA-sequencing pipeline to decipher 
                      the molecular anatomy and pathophysiology of human kidneys. <br> 
                      Nature Communications. 2019 Jun;10(1):2832. DOI: 10.1038/s41467-019-10861-2. "))
      
    ),
    
   
    h4(strong(HTML("<font color=\"#225F8B\">
                    Please give it a try by loading a dataset and searching for your genes of interest!
                     </font>"))),
    br(),
    h3(HTML("<font color=\"#225F8B\">Further References</font>")),
    tags$ul(
      tags$li(h4(em("Hillje et al:"), style="margin-bottom: 0px;"),
              h4(HTML("The scExplorer application is based upon the \"Cerebro\" single-cell analysis tool by Roman Hillje."),
                 style =" margin-top: 0px; margin-bottom: 0px;"),
              
              p(HTML("Hillje R, Pelicci PG, Luzi L. 
                      Cerebro: interactive visualization of scRNA-seq data. <br> 
                      Bioinformatics. 2020;36(7):2311-2313. doi:10.1093/bioinformatics/btz877"))
              
              
      )
    )
    

  )
)

