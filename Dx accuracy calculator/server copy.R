source("global.R")


shinyServer (
    function(input, output, session) {
        
        session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
        
        
        # validity check add in
        # check validity
        # NOT USED
        # inValidData <- eventReactive(input$GoButton,{ 
        #   return(  
        #     is.null(input$studyDesign) |
        #     is.null(input$dataType) |
        #     is.null(input$TP) |
        #     is.null(input$FP) |
        #     is.null(input$FN) |
        #     is.null(input$TN) |
        #     is.null(input$sensitivity) |
        #     is.null(input$specificity) |
        #     is.null(input$prevalence) |
        #     is.null(input$n)
        #   )
        # },ignoreNULL = FALSE)
        
        isValid_num <- eventReactive(input$GoButton,{
            
            if (
                  is.null(input$studyDesign) 
                | is.null(input$dataType)
                ) 
                return(FALSE)
            
            if (input$dataType == "TP, FP, FN, TN")
                return(!
                           is.null(input$TP) |
                           is.null(input$FP) |
                           is.null(input$FN) |
                           is.null(input$TN) )
            else return(!
                            is.null(input$sensitivity) |
                            is.null(input$specificity) |
                            is.null(input$prevalence) |
                            is.null(input$n) |
                            input$n < 1
            )
        },ignoreNULL = FALSE)
        
        # starting data taken from this paper with John Ioannidis as co-author
        # https://www.medrxiv.org/content/10.1101/2020.04.14.20062463v1.full.pdf
        #
        # see critique on their confidence intervals etc by Andrew Gelman
        # https://statmodeling.stat.columbia.edu/2020/04/19/fatal-flaws-in-stanford-study-of-coronavirus-prevalence/
        
        output$dataEntryTitle <- 
            renderUI({
                req(input$studyDesign, input$dataType)
                if (input$studyDesign == "cohort") # ****Note: ifelse() does not work****
                    h4("Enter results from cohort diagnostic accuracy study:")         
                else h4("Enter results from case-control diagnostic accuracy study:")
            })
        
        output$dataEntry2x2 <- # test accuracy data
            renderUI({ # data input for TP, FP, FN, TN
                req(input$studyDesign, input$dataType)
                if (
                    input$dataType == "TP, FP, FN, TN") 
                {
                    tagList(
                        numericInput("TP", "True positives", min = 0, value = 0),
                        numericInput("FP", "False positives", min = 0, value = 0),
                        numericInput("FN", "False negatives", min = 0, value = 0),
                        numericInput("TN", "True negatives", min = 0, value = 0)
                    )}
            })
        
        output$dataEntrySS <- # test accuracy data
            renderUI({ # data input for sensitivity, specificity data
                req(input$studyDesign, input$dataType)
                if (
                    input$dataType == "sensitivity & specificity") {
                    tagList(
                        numericInput("sensitivity", "sensitivity of index test (as %)", min=0, max=100, value= 100),
                        numericInput("specificity", "specificity of index test (as %)", min=0, max=100, value= 100) 
                    )}
            })
        
        
        output$dataEntryPrevN <-  # prevalence and study size for case-control studies
            renderUI({
                req(input$studyDesign, input$dataType)
                if (
                    input$dataType == "sensitivity & specificity"  | (input$studyDesign == "Case-control/don't know"))
                {
                    tagList(
                        hr(),
                        h5("Enter parameters to enable calculation of predictive values"),
                        numericInput("n", "number in study providing data", min=0, value = 1),
                        numericInput("prevalence", "prevalence of disease/condition (as %)", min = 0, max = 100, value = 0)
                    )}
            })
        
        
        
        observeEvent(input$GoButton, {
            output$validtext  <- renderText({
                if(!isValid_num()){
                    print("Inputs not valid, please check that the values for
             prevalence, sensitivity and specificity specified, lie between 0 and 1. ")
                    
                    message(input$studyDesign) 
                    message(input$dataType) 
                    message(input$TP) 
                    message(input$FP) 
                    message(input$FN) 
                    message(input$TN) 
                    message(input$sensitivity) 
                    message(input$specificity) 
                    message(input$prevalence) 
                    message(input$n)
                    message(input$n == 0)
                    
                    
                } else {
                    return(NULL)
                }
            })
        })
        
        
        sensitivity <- reactive(
            if (input$dataType == "sensitivity & specificity") {
                validate(
                    need(input$sensitivity > 0 & input$sensitivity < 100, "sensitivity must be >0 & < 100")
                )
            input$sensitivity/100
            }
            else
                input$TP / (input$TP + input$FN))

        specificity <- reactive(
            if (input$dataType == "sensitivity & specificity") 
                if (input$dataType == "sensitivity & specificity") {
                    validate(
                        need(input$specificity > 0 & input$specificity < 100, "specificity must be >0 & < 100")
                    )
                    input$specificity/100
                }
            else 
                input$TN / (input$TN + input$FP))
        
        n <- reactive(
            if (input$dataType == "sensitivity & specificity") {
                validate(
                    need(input$n > 0, "Study number must be greater thgn zero")
                )    
            
                input$n 
    }
            else 
                input$TP + input$FP + input$FN + input$TN)
        
        prevalence <- reactive(
            if (input$dataType == "sensitivity & specificity") {
                validate(
                    need(input$prevalence > 0, "prevalence must be greater thgn zero")
                )    
                input$prevalence/100
            }
            else 
                (input$TP + input$FP) / (input$TP + input$FP + input$FN + input$TN))
        
        observeEvent(input$GoButton, {
            
            output$table1title  <- renderText({
                "Summary of data and statistical measures"
            })
            
            output$tableStats  <- renderText({
                "Diagnostic accuracy statistics"
            })
            
            observeEvent(input$GoButton, {
                if(isValid_num()){
                    output$dx2x2Table <- renderTable({
                        isolate(dx2x2Table(n(), prevalence(), sensitivity(), specificity()))
                    }, digits = 0)
                }
                
                output$dxStatsTable <- renderTable({
                    isolate(dxStatsTable(n(), prevalence(), sensitivity(), specificity()))
                }, digits = 2, align = "rlrlrl", na = "")
                
            })
            
            output$customPlotTitle <- renderText(input$customPlotTitle)
            
            output$predValuesPlot<-renderPlotly({
                if(isValid_num()){
                    isolate(predValuesPlot(n(), prevalence(), sensitivity(), specificity(), input$ciFlag))
                }
            })
            
            
            output$populationPlot<-renderPlot({
                if(isValid_num()){
                    isolate(popplot(n(), prevalence(), sensitivity(), specificity(),
                                    input$sorted, input$ciFlag))
                }
            })
            output$testedPlots<-renderPlot({
                if(isValid_num()){
                    isolate(popplot2(n(), prevalence(), sensitivity(), specificity(),
                                     input$sorted, input$ciFlag))
                }
            })
            
            # output$distributionplots <- renderPlot({
            #   isolate(distributionplots(n(), prevalence(), sensitivity(), specificity(),
            #                    input$sorted, input$ciFlag))
            # })
            
        }, ignoreNULL = FALSE)
        
        
        output$predValuesPlotTitle  <- renderText({
            "Positive and negative predictive values versus prevalence"
        })
        
        output$plot1title  <- renderText({
            "Population before testing: people with and without the condition"
        })
        
        output$plot2title  <- renderText({
            "Population after testing: true and false positives; false and true negatives"
        })
        
        output$plot3title  <- renderText({
            "Distribution of index test results: true and false positives; false and true negatives.s"
        })
        
        output$distributiontext <- renderText({
            "     For any diagnostic test, there is often a trade-off
              between sensitivity and specificity. This is a natural consequence
              of the continuous nature of the outcome of the test result
              (e.g. the biomarker level present in the blood)
              and the dichotomous nature of the interpretation of the
              test result (i.e. positive or negative). The trade-off
              between maximising sensitivity and maximising specificity
              (or, equivalently, minimising FN or minimise FP)
              is what decides the threshold biomarker level during test development. \n
              \n

              A sensitivity and specificity value can be assigned to any
              given threshold. These values are then plotted graphically
              to produce a Receiver Operator Characteristic (ROC).
              In a ROC curve the sensitivity (true positive rate) is
              plotted against the false positive rate (1- specificity).
              "
        })
        
        
        # observeEvent(input$GoButton, {
        # output$distributionplots <- renderPlot({
        #    isolate(distributionplots(n(), prevalence(), sensitivity(), specificity()))
        # })
        # }, ignoreNULL = FALSE)
        #
        
        
        
        output$table2title  <- renderText({
            ("Test accuracy statistics")
        })
        
        output$customTableTitle <- renderText(input$customTableTitle)
        
        
        observeEvent(input$GoButton, {
            output$pvdf <- renderTable({
                isolate(pvdf(n(), prevalence(), sensitivity(), specificity()))
            }, digits = 0, rownames = TRUE)
            
        }, ignoreNULL = FALSE)
        
        
        
        
        ## Thanks to Mark Strong for this code
        # https://github.com/Sheffield-Accelerated-VoI/SAVI/blob/master/server.R
        
        output$downloadReport <- downloadHandler(
            filename = function() {  #"my-report.pdf"
                paste('report', sep = '.', switch(
                    input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                ))
            },
            
            content = function(file) {
                src <- normalizePath('report.Rmd')
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                file.copy(src, 'report.Rmd', overwrite=TRUE)
                
                library(rmarkdown)
                out <- render(input = 'report.Rmd', #pdf_document()
                              output_format = switch(
                                  input$format,
                                  PDF = pdf_document(), HTML = html_document(),
                                  Word = word_document())
                )
                file.copy(out, file)
            },
            contentType = "text/plain"
        )
        
        
        
        
    })
