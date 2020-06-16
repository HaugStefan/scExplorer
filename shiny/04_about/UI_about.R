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
      within the NephGen Initiative (CRC 1453) findable, accessible and searchable,
      for all reasearchers within the initiative and to thereby promote scientific 
      collaboration and progress. It allows for downloading of plots, as well as lists of marker genes. <br>
      Upon publication the datasets will also be publicly accessible via the scExplorer.")),
    
    h4("For demonstration, it currently contains data from two published single-cell 
      RNA-seq experiments of human kidney:"),
    tags$ul(
      tags$li(h4(em("Wu et al:"), style="margin-bottom: 0px;"),
              h4(HTML("Data from 4525 nuclei. Kidney tissue from a single human individual. <br>
                    Cell Stem Cell 2018, PMID: 30449713"),
                 style =" margin-top: 0px;"),
              ),
      tags$li(h4(em("Lake et al:"), style="margin-bottom: 0px;"),
              h4(HTML("Data from 17659 nuclei. Kidney tissue from 15 different human individuals. <br>
                    Nature Communications 2019 , PMID: 31249312"), 
              style="margin-top: 0px;")),
    ),
    
   
    h4(strong(HTML("<font color=\"#225F8B\">
                    Please give it a try by loading a dataset and searching for your genes of interest!
                     </font>"))),
    br(),
    h3(HTML("<font color=\"#225F8B\">Further References</font>")),
    tags$ul(
      tags$li(h4(em("Hillje et al:"), style="margin-bottom: 0px;"),
              h4(HTML("This application is based upon the \"Cerebro\" single-cell analysis tool by Roman Hillje. <br>
                      Bioinformatics 2020, PMID: 31764967"),
                 style =" margin-top: 0px;"),
      )
    )
    

  )
)

