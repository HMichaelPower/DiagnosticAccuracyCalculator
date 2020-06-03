#################### ui for ShinyApp to calculate diagnostic accuracy measures ###############################

ui <- function(request) {
  navbarPage(title = "",
             tabPanel("About", includeHTML("www/About.html")),
             hr(),
             tabPanel("Diagnostic accuracy calculator",
                        includeHTML("www/Calculator.html"),
                        sidebarPanel(
                            fluidRow(
                              textInput("customTableTitle", "Optional custom title for table", value = ""),
                              textInput("customPlotTitle", "Optional custom title for plot", value = ""),
                              
                                radioButtons("studyDesign", "Design of study providing data", choices = c("Case-control/don't know", "cohort"),
                                             selected = NA),
                                radioButtons("dataType", "Type of test accuracy data", choices = c("sensitivity & specificity", "TP, FP, FN, TN"),
                                             selected = NA),
                                hr(),
                                uiOutput("dataEntryTitle"),
                                uiOutput("dataEntry2x2"), # ? data for 2x2 table
                                uiOutput("dataEntrySS"), # ? sensitivity, specificity data
                                uiOutput("dataEntryPrevN"), # ? prevalence and study size if needed
                                checkboxInput("sorted", label = "Sort population by condition and test result",
                                              value = FALSE),
                                checkboxInput("ciFlag", label = "Show 95% confidence intervals", value = FALSE),
                                actionButton("GoButton", "Recalculate (will take a while)", class = 'middleAlign'),
                                p(""),
                                bookmarkButton("Get a link with current inputs")
                            )),
                        mainPanel(
                          navbarPage(title = "",
                                     tabPanel("Results",
                            span(textOutput("validtext"), style="color:red; font-style:italic"),
                            span(textOutput("table1title"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                            textOutput("customTableTitle"),
                            tags$head(tags$style("#customTableTitle{color: red;
                                 font-size: 18px;
                                 font-style: italic;
                                 }")),
                            textOutput("table1subtitle"),
                            tags$head(tags$style("#table1subtitle{color: red;
                                 font-size: 14px;
                                 font-style: italic;
                                 }")),
                            tableOutput("dx2x2Table"),
                            tableOutput("dxStatsTable"),
                            hr(),
                            span(textOutput("predValuesPlotTitle"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                            textOutput("customPlotTitle"),
                            tags$head(tags$style("#customPlotTitle{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
                            withSpinner(plotlyOutput("predValuesPlot")),
                            hr(),
                            span(textOutput("plot1title"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                            withSpinner(plotOutput("populationPlot")),
                            hr(),
                            span(textOutput("plot2title"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                            withSpinner(plotOutput("testedPlots"))
                            # withSpinner(plotOutput("distributionplots"))
                            
                            # # would prefer for this to be in tabs.
                            # "test",
                            # tabPanel("Population", plotOutput("populationPlot"), plotOutput("testedPlots")),
                            # tabPanel("Distributions", plotOutput("distributionplots"))
                            
                        ),
                        # tabPanel("Tables", includeHTML("www/Tables.html")),
                        tabPanel("Guide to using the calculator", includeHTML("www/Guide.html")),
                        tabPanel("Worked example", includeHTML("www/WorkedExample.html")),
                        selected = "Results")
               )),
               # 
               # 
               # tabPanel("Data and statistics",
               #          column(12, align="center",
               #                 # span(textOutput("table1title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 20px"),
               #                 # tableOutput("dx2x2Table"),
               #                 span(textOutput("table2title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 20px"),
               #                 tableOutput("pvdf")
               #          )
               # ),
               # 
               # tabPanel("Distributions",
               #          span(textOutput("plot3title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 18px"),
               #          withSpinner(plotOutput("distributionplots")),
               #          textOutput("distributiontext")
               # ),
               # 
               # tabPanel("Download summary report",
               #          p("This document contains all the tables and figures generated from your input data."),
               #          radioButtons('format', 'Please select the document format you require',
               #                       c('PDF', 'HTML', 'Word'),
               #                       inline = TRUE),
               #          downloadButton('downloadReport', 'Download summary report'),
               #          br(), br(),
               #          p("NB generating the document can take some time.")
               # ),
               # 
               
               # 
               # ###################################
               # #
               # #     credits as a running footer
               # #
               
               tabPanel("Acknowledgements", includeHTML("www/Acknowledgements.html")),
               tabPanel("Contact", includeHTML("www/Contact.html")),
               
               tags$br(),
               tags$b("Cite as:"),
               tags$p("Joy Allen, Michael Power, and ..."),
               tags$em("A calculator for calculating and visualizing diagnostic accuracy measures"),
               tags$p("NIHR Diagnostic Evidence Co-operative Newcastle. July 2017"),
               tags$br(),
               tags$img(src = "NIHRNewcastleMICLogo.jpg", width = "80px", height = "28px", align = "right"), # add the NIHR logo
            selected = "Diagnostic accuracy calculator")
}
