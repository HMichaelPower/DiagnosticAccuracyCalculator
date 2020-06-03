source("global.R")


shinyServer (
    function(input, output, session) {
        
        session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it

##### dynamic UI-s
        
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
        
##### reactive expressions
        
        sensitivity <- reactive({
            req(input$dataType)
            if (input$dataType == "sensitivity & specificity") {
                req(input$sensitivity)
                validate(
                    need(input$sensitivity > 0 & input$sensitivity < 100, "sensitivity must be >0 & < 100")
                )
            input$sensitivity/100
            }
            else {
                req(input$TP, input$FN)
                input$TP / (input$TP + input$FN)
            }
                                         })

        specificity <- reactive({
            req(input$dataType)
            if (input$dataType == "sensitivity & specificity") {
                req(input$specificity)
                    validate(
                        need(input$specificity > 0 & input$specificity < 100, "specificity must be >0 & < 100")
                    )
                    input$specificity/100
                }
            else {
                req(input$TN, input$FP)
                input$TN / (input$TN + input$FP)
            }
        })
        
        n <- reactive({
            req(input$dataType, input$studyDesign)
            if (input$dataType == "sensitivity & specificity" | (input$studyDesign == "Case-control/don't know")) {
                req(input$n)
                validate(
                    need(input$n > 0, "Study number must be greater thgn zero")
                )    
            
                input$n 
    }
            else {
                req(input$TP, input$FP, input$FN, input$TN)
                input$TP + input$FP + input$FN + input$TN
            }
        })
        
        prevalence <- reactive({
            req(input$dataType, input$studyDesign)
            if (input$dataType == "sensitivity & specificity" | (input$studyDesign == "Case-control/don't know")) {
                req(input$prevalence)
                validate(
                    need(input$prevalence > 0, "prevalence must be greater thgn zero")
                )    
                input$prevalence/100
            }
            else {
                req(input$TP, input$FP, input$FN, input$TN)
                (input$TP + input$FP) / (input$TP + input$FP + input$FN + input$TN)
            }
        })
 
        
##### outputs

        observeEvent(input$GoButton, {
            req(input$dataType, input$studyDesign)
            if (input$studyDesign == "Case-control/don't know" & input$dataType == "TP, FP, FN, TN") {
                output$table1subtitle  <- renderText({
                 "If the 2x2 table differs from entered data, read the Guide to see why this intended"
                })
            }
            else
                output$table1subtitle  <- NULL
        })
                
        observeEvent(input$GoButton, {
            output$table1title  <- renderText({
                "Summary of data and statistical measures"
            })
 
            output$tableStats  <- renderText({
                "Diagnostic accuracy statistics"
            })
            
            output$dx2x2Table <- renderTable({
                 isolate(dx2x2Table(n(), prevalence(), sensitivity(), specificity()))
                  }, digits = 0)
                
                output$dxStatsTable <- renderTable({
                    isolate(dxStatsTable(n(), prevalence(), sensitivity(), specificity()))
                }, digits = 2, align = "rllrlrl", na = "")
            
            output$customPlotTitle <- renderText({
                input$customPlotTitle
            })
        
            output$predValuesPlot<-renderPlotly({
                    isolate(predValuesPlot(n(), prevalence(), sensitivity(), specificity(), input$ciFlag))
            })
            
            
            output$populationPlot<-renderPlot({
                    isolate(popplot(n(), prevalence(), sensitivity(), specificity(),
                                    input$sorted, input$ciFlag))

            })
            output$testedPlots<-renderPlot({
                    isolate(popplot2(n(), prevalence(), sensitivity(), specificity(),
                                     input$sorted, input$ciFlag))

            })
            
            # output$distributionplots <- renderPlot({
            #   isolate(distributionplots(n(), prevalence(), sensitivity(), specificity(),
            #                    input$sorted, input$ciFlag))
            # })
        
        
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
