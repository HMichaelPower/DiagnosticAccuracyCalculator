##########################################################
# FunctionsUsedByTestAccuracyApp.R()
#
# non-reactive functions used by the Shiny App to explore clinical accuracy measurements
#
# installation of colour picker from htTP://deanattali.com/blog/plot-colour-helper/
#
#install.packages("devtools")
#devtools::install_github("daattali/colourpicker")
#
##########################################################
#
# load packages used by the App 
LoadPackages <- function() {
  library(shiny)
  library(tidyverse) # Imports: broom, DBI, dplyr, forcats, ggplot2, haven, httr, hms, jsonlite, lubridate, magrittr, modelr, purrr, readr, readxl, stringr, tibble, rvest, tidyr, xml2
  library(rsconnect)   # needed to upload to Shinyio
  library(readr)       # needed for GET()
  library(vcd)         # mosaic() plot htTP://www.statmethods.net/advgraphs/mosaic.html
  library(colourpicker) # htTP://deanattali.com/blog/plot-colour-helper/ 
  library(shinythemes)
  library(DT)
  library(knitr)
  library(rmarkdown)
  library(shinycssloaders)
  #      library(proportion)  package no longer being maintained :-(
  library(PropCIs)
  library(ggrepel)
  library(glue)
  library(plotly)
  # ...
}

marginInsidePlot = 0.01
#################### confidence interval on a proportion ###############################
#
### this is a wrapper to allow the choice of CI method to be easily changed
#
# use PropCIs::scoreci() to calculate Wilson's confidence interval for a single proportion. 
#             Score CI based on inverting the asymptotic normal test using the null standard error
# Arguments: 
# x	           Number of successes
# n            Total sample size
# conf.level   Confidence level

ciprop <- function(x, n, alpha = 0.05) {

  # validaity checks do not work!!!!!!!
  # if (x/n >1)
  #   # scorci gives plausible-looking results for non-integers
  #   # so they are allowed through
  #   stop(paste(
  #      "invalid call to scorci(): x must not be < 0 or > n. ", 
  #      "\n   x = ", x, 
  #      "\n   n = ", n))

  # the CI calculator works well with non-integers, which is convenient
  #
  conf.level <- 1 - alpha
  
    return({
    data.frame(
      ciL = PropCIs::scoreci(x, n, conf.level)$conf.int[1],
      ciU = PropCIs::scoreci(x, n, conf.level)$conf.int[2]
    )
  })
}


# for testing
# sensitivity <- 0.758
# specificity <- 0.853
# n <- 719
# prevalence <- 0.414
# 
# TP <- TPFN(sensitivity, specificity, n, prevalence)
# FP <- FPFN(sensitivity, specificity, n, prevalence)
# FN <- FNFN(sensitivity, specificity, n, prevalence)
# TN <- TNFN(sensitivity, specificity, n, prevalence)
# 
# Dpos <- DposFN(sensitivity, specificity, n, prevalence)
# Dneg <- DnegFN(sensitivity, specificity, n, prevalence)
# 
# tesTPos <- tesTPosFN(sensitivity, specificity, n, prevalence)
# tesTNeg <- tesTNegFN(sensitivity, specificity, n, prevalence)

TPFN <- function(sensitivity, specificity, n, prevalence) {
  n*prevalence*sensitivity
}

FPFN <- function(sensitivity, specificity, n, prevalence) {
  FP <- n*(1 - prevalence)*(1 - specificity)
}

FNFN <- function(sensitivity, specificity, n, prevalence) {
  FN = n*prevalence*(1 - sensitivity)
}

TNFN <- function(sensitivity, specificity, n, prevalence) {
  TN = n*(1 - prevalence)*specificity
}

DposFN <- function(sensitivity, specificity, n, prevalence) {
  Dpos = n*prevalence
}

dNegFN <- function(sensitivity, specificity, n, prevalence) {
  Dneg = n*(1 - prevalence)
}

tesTPosFN <- function(sensitivity, specificity, n, prevalence) {
  tesTPos = n*prevalence*sensitivity + n - n*specificity - n*prevalence + n*prevalence*specificity
}

tesTNegFN <- function(sensitivity, specificity, n, prevalence) {
  tesTNeg <- n*(specificity + prevalence*(1 - sensitivity - specificity))
}





DxStats <- function(n, prevalence, sensitivity, specificity) {
  #prevalence <- min(prevalence,0.9999)
  #prevalence <- max(prevalence,0.0001)
  
  Dpos <- n * prevalence
  Dneg <- n - Dpos
  
  TP <- round(sensitivity * round(Dpos))
  TN <- round(specificity * round(Dneg))
  
  FN <- round(Dpos) - round(round(Dpos)*sensitivity)
  FP <- round(Dneg) - round(round(Dneg)*specificity)
  
  sensitivity = sensitivity
  specificity = specificity
  
  PPV <- TP/(TP + FP)
  NPV <- TN/(TN + FN) 
  
  LRp <- sensitivity/(1 - specificity)
  LRn <- (1 -sensitivity)/(specificity)
  
  PreTestOddsP <- prevalence/(1 - prevalence)
  PreTestOddsN <- (prevalence)/(1 -prevalence)
  
  PostTestOddsP <- PreTestOddsP*LRp
  PostTestOddsN <- PreTestOddsN*LRn
  
  PostTesTProbP <- PostTestOddsP/(PostTestOddsP + 1) # = PPV
  PostTesTProbN <- PostTestOddsN/(PostTestOddsN + 1) # = (1 - NPV)
  
  cidf <- ciprop(TP, TP + TP) # CI for post-positive test probability
  PPV_ciL <- cidf$ciL
  PPV_ciU <- cidf$ciU
  
  cidf <- ciprop(TN, TN + FN) # CI for post-negative test probability
  NPV_ciL <- cidf$ciL
  NPV_ciU <- cidf$ciU
  
  data.frame(
    Dpos = Dpos,
    Dneg = Dneg,
    
    TP = TP,
    TN = TN,
    
    FN = FN,
    FP = FP,
    
    PPV = PPV,
    NPV = NPV,
    
    LRp = LRp,
    LRn = LRn,
    
    PreTestOddsP = PreTestOddsP,
    PreTestOddsN = PreTestOddsN,
    
    PostTestOddsP = PostTestOddsP,
    PostTestOddsN = PostTestOddsP,
    
    PostTesTProbP = PostTesTProbP,
    PostTesTProbN = PostTesTProbN,
    
    PPV_ciL = PPV_ciL,
    PPV_ciU = PPV_ciU,
    
    NPV_ciL = NPV_ciL,
    NPV_ciU = NPV_ciU
  )
}

DxStatsExact <- function(n, prevalence, sensitivity, specificity) {
  
  # # for debugging
  # n <- 719
  # prevalence <- 0.414
  # sensitivity <- 0.758
  # specificity <- 0.853
  
  #prevalence <- min(prevalence,0.9999)
  #prevalence <- max(prevalence,0.0001)
  
  Dpos <- n * prevalence
  Dneg <- n - Dpos
  
  TP <- sensitivity * Dpos
  TN <- specificity * Dneg
  
  FN <- Dpos - Dpos*sensitivity
  FP <- Dneg - Dneg*specificity
  
  PPV <- TP/(TP + FP)
  NPV <- TN/(TN + FN) 
  
  LRp <- sensitivity/(1 - specificity)
  LRn <- (1 -sensitivity)/(specificity)
  
  PreTestOddsP <- prevalence/(1 - prevalence)
  PreTestOddsN <- (prevalence)/(1 -prevalence)
  
  PostTestOddsP <- PreTestOddsP*LRp
  PostTestOddsN <- PreTestOddsN*LRn
  
  PostTesTProbP <- PostTestOddsP/(PostTestOddsP + 1) # = PPV
  PostTesTProbN <- PostTestOddsN/(PostTestOddsN + 1) # = (1 - NPV)
  
# CI for PPV
  PPV_ciL <- ciprop(TP, (TP + FP))$ciL
  PPV_ciU <- ciprop(TP, (TP + FP))$ciU
  
 # CI for NPV
  NPV_ciL <- ciprop(TN, (TN + FN))$ciL
  NPV_ciU <- ciprop(TN, (TN + FN))$ciU

  data.frame(
    Dpos = Dpos,
    Dneg = Dneg,
    
    TP = TP,
    TN = TN,
    
    FN = FN,
    FP = FP,
    
    sensitivity = sensitivity,
    sens_ciL = ciprop(TP, (TP + FN))$ciL,
    sens_ciU = ciprop(TP, (TP + FN))$ciU,

    specificity = specificity,
    spec_ciL = ciprop(TN, (TN + FP))$ciL,
    spec_ciU = ciprop(TN, (TN + FN))$ciU,
    
    prevalence = prevalence,
    prev_ciL = ciprop(TP, n)$ciL,
    prev_ciU = ciprop(TP, n)$ciU,

    PPV = PPV,
    PPV_ciL = PPV_ciL,
    PPV_ciU = PPV_ciU,
 
    NPV = NPV,   
    NPV_ciL = NPV_ciL,
    NPV_ciU = NPV_ciU,  
    
    LRp = LRp,
    LRn = LRn,
    
    PreTestOddsP = PreTestOddsP,
    PreTestOddsN = PreTestOddsN,
    
    PostTestOddsP = PostTestOddsP,
    PostTestOddsN = PostTestOddsP,
    
    PostTesTProbP = PostTesTProbP,
    PostTesTProbN = PostTesTProbN

  )
}



DxCounts <- function(StudyDesign, TP, FP, FN, TN) {
  Dpos <- TP + FN
  Dneg <- TN + FP
  
  sensitivity <- TP/Dpos
  specificity <- TN/Dneg
  
  population <- NA
  prevalence <- NA
  
  if (StudyDesign == "cohort") {
    Population <- Dpos + Dneg
    Prevalence <- DposPopulation
  }
  
  DxStatsExact(population, prevalence, Sens, Spec)
} 


linesDf <- function(n, prevalence, sensitivity, specificity){
  
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  # Dpos <- round((n * input$prevalence))
  #  Dneg <- round(input$n - Dpos)
  # FN <- round((1 - input$sensitivity) * Dpos)
  # TN <- round(input$specificity * Dneg)
  
  return({data.frame(
    ### define computed line segments for vertical line separating Dpos from Dneg    
    vx = Dx$Dpos /n,
    vxlci = ciprop(Dx$Dpos, n)$ciL,
    vxuci = ciprop(Dx$Dpos, n)$ciU,
    
    ### define computed line segments for horizontal lines separating TesTPos from TesTNeg    
    hy1 = Dx$FN/Dx$Dpos,  
    hy1lci = ciprop(Dx$FN, Dx$Dpos)$ciL, 
    hy1uci = ciprop(Dx$FN, Dx$Dpos)$ciU,
    
    hy2 = Dx$TN/Dx$Dneg,
    hy2lci = ciprop(Dx$TN, Dx$Dneg)$ciL,
    hy2uci = ciprop(Dx$TN, Dx$Dneg)$ciU)
  })
}


dx2x2Table <- function(n, prevalence, sensitivity, specificity){
  Dx <- DxStatsExact(n, prevalence, sensitivity, specificity) 
  table <-  data.frame(
    . = c("Test positive", "Test negative", "Totals"),
    ConditionPresent = c(Dx$TP, Dx$FN, Dx$Dpos),
    ConditionAbsent = c(Dx$FP, Dx$TN, Dx$Dneg),
    Totals = c(Dx$TP + Dx$FP, Dx$FN + Dx$TN, n)
  )
  colnames(table) <- c("", "Condition present", "Condition absent", "Total")
  return(table)
}

dxStatsTable <- function(n, prevalence, sensitivity, specificity){
  Dx <- DxStatsExact(n, prevalence, sensitivity, specificity)
  table <-  data.frame(
    labels1 = c("sensitivity =", "specificity =", "prevalence ="),
    values1 = c(sensitivity, specificity, prevalence),
    ConfInts = c(
      paste0("(95% CI ", as.character(round(Dx$sens_ciL, digits = 2)), 
             " - ", as.character(round(Dx$sens_ciU, digits = 2)), ")"),
      paste0("(95% CI ", as.character(round(Dx$spec_ciL, digits = 2)), 
             " - ", as.character(round(Dx$spec_ciU, digits = 2)), ")"),
      paste0("(95% CI ", as.character(round(Dx$prev_ciL, digits = 2)), 
             " - ", as.character(round(Dx$prev_ciU, digits = 2)), ")")
    ),
    labels2 = c("+ve PV =", "-ve PV =", "study size ="),
    values2 = c(
      as.character(round(Dx$PPV, digits = 2)), 
      as.character(round(Dx$NPV, digits = 2)), 
      as.character(round(n))),
                
    labels3 = c("LR+ =", "LR- =", ""),
    values3 = c(Dx$LRp, Dx$LRn, NA)
  )
  colnames(table) <- rep(c(""), 7)
  
  return(table)
}

### coordinates and labels for contingency matrix graphic
contingencyM <- function(n, prevalence, sensitivity, specificity){
  
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  
  return({data.frame(
    cmX = c(
      0.5 * Dx$Dpos/n,          # TP 
      (Dx$Dpos + 0.5*Dx$Dneg)/n,   # FP
      0.5 * Dx$Dpos/n,          # FN
      (Dx$Dpos + 0.5*Dx$Dneg)/n,   # TN
      0.5 * Dx$Dpos/n,          # ppv
      (Dx$Dpos + 0.5*Dx$Dneg)/n    # npv
    ),
    cmY = c(
      (Dx$FN + 0.5*Dx$TP)/(Dx$FN + Dx$TP),        #TP
      (Dx$TN + 0.5*Dx$FP)/(Dx$TN + Dx$FP),        #FP
      0.5 * Dx$FN/(Dx$FN + Dx$TP),             #FN
      0.5 * Dx$TN/(Dx$TN + Dx$FP),             #TN
      (Dx$FN + 0.5*Dx$TP)/(Dx$FN + Dx$TP) - 0.04, #ppv
      0.5 * Dx$TN/(Dx$TN + Dx$FP) - 0.04       #npv
      
    ), 
    labs = c(
      paste("TP = ", round(Dx$TP)),
      paste("FP = ", round(Dx$FP)),
      paste("FN = ", round(Dx$FN)),
      paste("TN = ", round(Dx$TN)),
      paste("ppv = ", paste(format(100*Dx$TP / (Dx$TP + Dx$FP), digits = 2),"%", sep = "")),
      paste("npv = ", paste(format(100*Dx$TN / (Dx$TN + Dx$FN), digits = 2),"%", sep = ""))
    )
  )
  })
}


pvdf <- function(n,prevalence, sensitivity, specificity){
  
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  table2 <- data.frame(
    PredictiveValues = c(
      paste(format(100*Dx$TP / (Dx$TP + Dx$FP), digits = 3), "%", sep = ""),
      paste(format(100*Dx$TN / (Dx$TN + Dx$FN), digits = 3), "%", sep = "")
    ),
    ATPrevalence = c(paste(format(100*prevalence, digits = 2), "%", sep = "")),
    Measure = c("Sensitivity", "Specificity"),
    LL95CI = c(
      paste(trimws(format((sensitivity*100 - ciprop(sensitivity*100, n)$ciL), digits = 2)), "%", sep = ""),
      paste(trimws(format((specificity*100 - ciprop(specificity*100, n)$ciL), digits = 2)), "%", sep = "")),
    Mid = c(
      paste(trimws(format(100*sensitivity, digits = 2)), "%", sep = ""),
      paste(trimws(format(100*specificity, digits = 3)), "%", sep = "")
    ),
    UL95CI = c(
      paste(trimws(format((100*sensitivity + ciprop(100*sensitivity, n)$ciU), digits = 2)), "%", sep = ""),
      paste(trimws(format((100*specificity + ciprop(100*specificity, n)$ciU), digits = 2)), "%", sep = "")),
    row.names = c("PPV", "NPV")
  )
  
  colnames(table2) <- c("Predictive values", "Prevalence", "Accuracy measure", "Lower 95% CI", "Mid point", "Upper 95% CI")
  return(table2)
}


populationdf <- function(n, prevalence, sensitivity, specificity, sorted){
  
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  if (sorted){
    x = c(
      runif(round(round(Dx$Dpos)*sensitivity), min = marginInsidePlot, max = round(Dx$Dpos)/n - marginInsidePlot),
      runif(round(Dx$Dpos) - round(round(Dx$Dpos)*sensitivity), min = marginInsidePlot, max = round(Dx$Dpos)/n - marginInsidePlot),
      runif(round(Dx$Dneg) - round(round(Dx$Dneg)*specificity), min = round(Dx$Dpos)/n + marginInsidePlot, max = 1 - marginInsidePlot),
      runif(round(round(Dx$Dneg)*specificity), min = round(Dx$Dpos)/n + marginInsidePlot, max = 1 - marginInsidePlot)
    )
    y = c(
      runif(round(round(Dx$Dpos)*sensitivity), min = round(Dx$FN,0)/(round(Dx$TP,0) + round(Dx$FN,0)) + marginInsidePlot, max = 1 - marginInsidePlot),
      runif(round(Dx$Dpos) - round(round(Dx$Dpos)*sensitivity), min = marginInsidePlot, max =  Dx$FN/(Dx$TP + Dx$FN) - marginInsidePlot),
      runif(round(Dx$Dneg) - round(round(Dx$Dneg)*specificity), min = round(Dx$TN,0)/(round(Dx$FP,0) + round(Dx$TN,0)) + marginInsidePlot, max = 1 - marginInsidePlot),  # need to fix this if FPs less than 1!!
      runif(round(round(Dx$Dneg)*specificity), min = marginInsidePlot, max = Dx$TN/(Dx$FP + Dx$TN) - marginInsidePlot)
    )
    
  } else {
    x = c(runif(n, min = marginInsidePlot, max = 1 - marginInsidePlot))
    y = c(runif(n, min = marginInsidePlot, max = 1 - marginInsidePlot))
  }
  
  
  return( {
    data.frame(
      ID = 1:n,
      condition = c(
        rep(paste("Present  = ", Dx$Dpos), times = round(Dx$Dpos)),
        rep(paste("Absent = ", n - Dx$Dpos), times = n - round(Dx$Dpos))
      ),
      conditionShape = c(
        rep(21, times = round(Dx$Dpos)),
        rep(22, times = n - round(Dx$Dpos))
      ),
      
      testResult = c(
        rep(paste("TesTPos = ", Dx$FP + Dx$TP), 
            times = round(round(Dx$Dpos)*sensitivity) + round(Dx$Dneg) - round(round(Dx$Dneg)*specificity)),
        rep(paste("TesTNeg = ", round(round(Dx$Dneg)*specificity) + round(Dx$Dpos) - round(round(Dx$Dpos)*sensitivity)),
            times = (round(round(Dx$Dneg)*specificity) + round(Dx$Dpos) - round(round(Dx$Dpos)*sensitivity))) 
      ),
      
      result = c(
        rep(paste("TruePos = ", round(round(Dx$Dpos)*sensitivity)), times = round(round(Dx$Dpos)*sensitivity)),
        rep(paste("FalseNeg = ", round(Dx$Dpos) - round(round(Dx$Dpos)*sensitivity)), times = round(Dx$Dpos) - round(round(Dx$Dpos)*sensitivity)), 
        rep(paste("FalsePos = ",  round(Dx$Dneg) - round(round(Dx$Dneg)*specificity)), times = round(Dx$Dneg) - round(round(Dx$Dneg)*specificity)), 
        rep(paste("TrueNeg = ", round(round(Dx$Dneg)*specificity)), times = round(round(Dx$Dneg)*specificity))
      ),
      resultShape = c(
        rep(21, times = round(round(Dx$Dpos)*sensitivity)), ## need to sort out these!
        rep(22, times = round(Dx$Dpos) - round(round(Dx$Dpos)*sensitivity)), 
        rep(23, times = round(Dx$Dneg) - round(round(Dx$Dneg)*specificity)), 
        rep(24, times = round(round(Dx$Dneg)*specificity))
      ),
      x, 
      y
    )
  })
}

predValuesPlot <- function(n, prevalence, sensitivity, specificity, ciFlag){
  
  # # # for testing function in console before trying in shiny
  # n <- 719
  # prevalence <- 0.4144645341
  # sensitivity <- 0.7583892617
  # specificity <- 0.8527315914
  # #
  # # #
  
  prevalenceLine <- tibble(
    x = c(prevalence, prevalence, prevalence, prevalence),
    y = c(0, 
          DxStatsExact(n, prevalence, sensitivity, specificity)$NPV, 
          DxStatsExact(n, prevalence, sensitivity, specificity)$PPV, 
          1),
    labels = c("", 
               paste0("NPV = ", round( 100*DxStatsExact(n, prevalence, sensitivity, specificity)$NPV), "%\n for prev = ", round(prevalence*100, digits = 1), "%" ),
               paste0("PPV = ", round( 100*DxStatsExact(n, prevalence, sensitivity, specificity)$PPV), "%\n for prev = ", round(prevalence*100, digits = 1), "%" ),
               "")
  )
  
  pvPoints <- tibble(
    x = c(prevalence, prevalence),
    y = c(
      DxStatsExact(n, prevalence, sensitivity, specificity)$NPV, 
      DxStatsExact(n, prevalence, sensitivity, specificity)$PPV),
    grp = c("PPV", "NPV")  )
  
  predValues <- tibble(prevalence = seq(0, 1, by = 0.01)) %>% 
    mutate(
      PPV = DxStatsExact(!!n, prevalence, !!sensitivity, !!specificity)$PPV,
      NPV = DxStatsExact(!!n, prevalence, !!sensitivity, !!specificity)$NPV
    ) %>% 
    pivot_longer(-prevalence, names_to = "Pred", values_to = "Predictive_Value") %>% 
    group_by(Pred)
  
  Ribbons <- tibble(prevalence = seq(0, 1, by = 0.01)) %>% 
            mutate(
              PPV_ciL =  DxStatsExact(!!n, prevalence, !!sensitivity, !!specificity)$PPV_ciL,
              PPV     =  DxStatsExact(!!n, prevalence, !!sensitivity, !!specificity)$PPV,
              PPV_ciU =  DxStatsExact(!!n, prevalence, !!sensitivity, !!specificity)$PPV_ciU,

              NPV_ciL =  DxStatsExact(!!n, prevalence, !!sensitivity, !!specificity)$NPV_ciL,
              NPV     =  DxStatsExact(!!n, prevalence, !!sensitivity, !!specificity)$NPV,
              NPV_ciU =  DxStatsExact(!!n, prevalence, !!sensitivity, !!specificity)$NPV_ciU,
            )
if (ciFlag) {
  for (i in seq(from = 0, to = 1, by = 0.01)){
  Ribbons$PPV_ciL[i * 100] =  DxStatsExact(n, i, sensitivity, specificity)$PPV_ciL
  Ribbons$PPV[i * 100]     =  DxStatsExact(n, i, sensitivity, specificity)$PPV
  Ribbons$PPV_ciU[i * 100] =  DxStatsExact(n, i, sensitivity, specificity)$PPV_ciU
  
  Ribbons$NPV_ciL[i * 100] =  DxStatsExact(n, i, sensitivity, specificity)$NPV_ciL
  Ribbons$NPV[i * 100]     =  DxStatsExact(n, i, sensitivity, specificity)$NPV
  Ribbons$NPV_ciU[i * 100] =  DxStatsExact(n, i, sensitivity, specificity)$NPV_ciU
}}
  # pseudoCI <- 0.1
  # for (prevalence in seq(from = 0, to = 1, by = 0.01)){
  #   i <- prevalence*100
  #   Ribbons$PPV[i] =  DxStatsExact(n, prevalence, sensitivity, specificity)$PPV
  #   Ribbons$PPV_ciL[i] =  max(Ribbons$NPV[i] - pseudoCI, 0)
  #   Ribbons$PPV_ciU[i] =  min(Ribbons$PPV[i] + pseudoCI, 1)
  #   
  #   Ribbons$NPV[i] =  DxStatsExact(n, prevalence, sensitivity, specificity)$NPV
  #   Ribbons$NPV_ciL[i] =  max(Ribbons$NPV[i] - pseudoCI, 0)
  #   Ribbons$NPV_ciU[i] =  min(Ribbons$NPV[i] + pseudoCI, 1)
  # }
  

  p0 <- ggplot()
  
  if (ciFlag) {
    # the ribbons are not very informative for PPV near 0 and NPV near 1 
    # which is a pity, because that is where they would be most useful :=(
    p0 <- p0 + 
      geom_ribbon(aes( x = prevalence, ymin = PPV_ciL, ymax = PPV_ciU), data = Ribbons, fill = "seashell", alpha = 0.9) +
      geom_ribbon(aes( x = prevalence, ymin = NPV_ciL, ymax = NPV_ciU), data = Ribbons, fill = "mintcream", alpha = 0.9) 
  }
  
  p0 <- p0 + 
    geom_point(data = predValues, aes(prevalence, Predictive_Value, colour = Pred), size = 1, stroke = 0) +
    scale_color_manual(values=c("cyan3", "firebrick")) +
    geom_line(data = prevalenceLine, aes(x, y)) +
    geom_text(data = prevalenceLine, aes(x, y, label = labels)) +
    geom_point(data = pvPoints, aes(x, y, colour = grp, fill = grp),  size = 2, shape = 23, stroke = 1) + 
    scale_x_continuous(limits = c(-0.025, 1.025), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_y_continuous(limits = c(-0.025, 1.025), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    # theme(legend.title=element_blank()) +
    theme(legend.position = "none") +
    labs(y= "Predictive value", x = "Prevalence") +
    coord_fixed()
  
  p0 <- ggplotly(p0, tooltip = c("prevalence", "Predictive_Value"))

  }


popplot <- function(n, prevalence, sensitivity, specificity, sorted, ciFlag){
  
  populationdf <- populationdf(n, prevalence, sensitivity, specificity, sorted)
  linesDf <- linesDf(n, prevalence, sensitivity, specificity)
  
  p1 <- ggplot(populationdf, aes(x=x, y=y, color=condition, shape = condition)) + geom_point(size = 4) +
    # scale_color_manual(values=c("#E69F00", "#999999")) + coord_fixed() + 
    scale_color_manual(values=c("cyan3", "firebrick")) + coord_fixed() + 
    labs("Condition") #, colour="Result")
  if (sorted) {
    p1 <- ggplot(populationdf, aes(x=x, y=y, color=condition, shape = condition)) + 
      geom_point(size = 4) + #scale_color_manual(values=c("#999999", "#E69F00"))  +
      
      ### add line segments (with 95% CI)  to separate Condition present from condition absent
      geom_segment(aes(x = vx, y = 0, xend = vx, yend = 1, colour = NULL, shape = NULL), 
                   data = linesDf) + 
      
      
      ### add in scales for x and y axis 
      scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                         labels = c("0","25%","50%","75%","100%")) + theme(axis.text.x = element_text(size = 15,colour = "azure4")) + 
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      # scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
      #                    labels = c("0","25%","50%","75%","100%")) + theme(axis.text.y = element_text(size = 15,colour = "azure4")) + 
      coord_fixed()
    
    if (ciFlag) {
      p1 <- p1 + annotate("rect", xmin = linesDf$vxlci, xmax = linesDf$vxuci, ymin = 0, ymax = 1,
                          colour = "deepskyblue", alpha = 0.2)
    }
  }
  if (!sorted) {
    p1 <- p1 +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      ) 
  }
  p1 <- p1 +
    labs(x = "", y = "") #, title="Population: people with and without the condition") + 
  #  theme(plot.title = element_text(size = rel(1.5), colour = "dodgerblue3"))
  p1
  
}


popplot2 <- function(n, prevalence, sensitivity, specificity, sorted, ciFlag){
  
  populationdf <- populationdf(n, prevalence, sensitivity, specificity, sorted)
  linesDf <- linesDf(n, prevalence, sensitivity, specificity)
  contingencyM <- contingencyM(n, prevalence, sensitivity, specificity)
  
  p2 <- ggplot(populationdf, aes(x=x, y=y, color=condition, shape = result)) + geom_point(size = 4) +
    coord_fixed() + labs(color="Condition", shape="Result")
  
  if (sorted) {
    p2 <- p2 + 
      ### line to separate Condition present/absent
      geom_segment(aes(x = vx, y = 0, xend = vx, yend = 1, colour = NULL, shape = NULL), 
                   data = linesDf) +
      
      ### line to separate TP from FN          
      geom_segment(aes(x = 0, y = hy1, xend = vx, yend = hy1, colour = NULL, shape = NULL), 
                   data = linesDf) + 
      
      ### line to separate FP from TN          
      geom_segment(aes(x = vx, y = hy2, xend = 1, yend = hy2, colour = NULL, shape = NULL), 
                   data = linesDf) + 
      
      ### label the cells of the contingency matrix 
      geom_label_repel(data = contingencyM, size = 5, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL), 
                       fontface = 2, colour = "gray41", label.padding = unit(0.15, "lines"), label.r = unit(0.2, "lines")) + 
      
      ### add in scales for x and y axis
      scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                         labels = c("0","25%","50%","75%","100%")) + theme(axis.text.x = element_text(size = 15,colour = "azure4")) + 
      scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                         labels = c("0","25%","50%","75%","100%")) + theme(axis.text.y = element_text(size = 15,colour = "azure4")) + 
      coord_fixed()
    
    
    if (ciFlag) {
      p2 <- p2 +
        ### rectangles to show 95% CIs
        annotate("rect", xmin = linesDf$vxlci, xmax = linesDf$vxuci, ymin = 0, ymax = 1,
                 colour = "deepskyblue", alpha = 0.2) +
        annotate("rect", xmin = linesDf$vxlci, xmax = linesDf$vxuci, ymin = 0, ymax = 1,
                 colour = "deepskyblue", alpha = 0.2) +
        annotate("rect", xmin = 0, xmax = linesDf$vx, ymin = linesDf$hy1lci, ymax = linesDf$hy1uci,
                 colour = "darksalmon", alpha = 0.2) +
        annotate("rect", xmin = linesDf$vx, xmax = 1, ymin = linesDf$hy2lci, ymax = linesDf$hy2uci,
                 colour = "darksalmon", alpha = 0.2)      #+
      #  geom_text_repel(data = contingencyM, size = 7, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL), 
      #            fontface = 2, colour = "gray41") 
    }
    
    
  }
  if (!sorted) {
    p2 <- p2 +  theme(
      axis.text.x = element_blank(),#element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) 
  }
  p2 <- p2 +
    labs(x = "", y = "")#, 
  #    title ="Test accuracy: true and false positives; false \n and true negatives.") +
  #   theme(plot.title = element_text(size = rel(1.5), colour = "dodgerblue3"))
  #ggtitle("Test accuracy: true and false positives; \n false and true negatives, sensitivity, specificity, ...")
  p2
  
}

distributiondf <- function(prevalence, sensitivity, specificity){
  xdist <- seq(0,1,length = 1000)
  mean_pos <- 0.5 - sensitivity*0.25
  ydist <- prevalence*dnorm(xdist,mean = mean_pos,sd = 0.1)
  xdist2 <- seq(0,1,length = 1000)
  mean_neg <- 0.5 + specificity*0.25
  ydist2 <- (1-prevalence)*dnorm(xdist2,mean = mean_neg,sd = 0.1)
  
  ydist[1] <- 0
  ydist2[1] <- 0
  ydist[1000] <- 0
  ydist2[1000] <- 0
  
  return({data.frame(
    mean_pos = mean_pos, 
    mean_neg = mean_neg,
    xdist = xdist, 
    ydist = ydist,
    xdist2 = xdist2,
    ydist2 = ydist2)
  })
  
}

### coordinates and labels for distributions graphic
distritext <- function(n, prevalence, sensitivity, specificity){
  distdf <- distributiondf(prevalence, sensitivity, specificity )
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  
  max1 <- 0.25*max(distdf$ydist, na.rm = TRUE)
  max2 <- 0.25*max(distdf$ydist2, na.rm = TRUE)
  
  data.frame(
    cmX = c(
      distdf$mean_pos[1],
      distdf$mean_neg[1] - 0.25,
      distdf$mean_pos[1] + 0.25,
      distdf$mean_neg[1]
    ),
    cmY = c(
      max1,
      max2*0.25,
      max1*0.25,
      max2
    ), 
    labels = c(
      strwrap(paste0("TP = ", Dx$TP)),
      strwrap(paste0("FP = ", Dx$FP)),
      strwrap(paste0("FN = ", Dx$FN)),
      strwrap(paste0("TN = ", Dx$TN))
    )
  )
}


distributionplots <- function(n, prevalence, sensitivity, specificity){
  distridf <- distributiondf(prevalence, sensitivity, specificity )
  
  distritext <- distritext(n, prevalence, sensitivity, specificity )
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  shade <- rbind(c(0.5,0), subset(distridf, xdist < 0.5), c(distridf[nrow(distridf), "X"], 0))
  shade3 <- rbind(c(0.5,0), subset(distridf, xdist < 0.5), c(distridf[nrow(distridf), "X"], 0.5))
  shade2 <- rbind(c(0.5,0), subset(distridf, xdist2 > 0.5), c(distridf[nrow(distridf), "X2"], 0))
  distri <- ggplot(distridf, aes(x = xdist2, y = ydist2)) +
    geom_polygon(data = shade2, aes(xdist2, ydist2), fill = "#999999")
  distri <- distri + geom_line(colour = "#999999")
  distri <- distri + geom_line(aes(x = xdist, y = ydist), colour =  "#E69F00") + geom_vline(xintercept = 0.5) +
    geom_polygon(data = shade3, aes(xdist, ydist), fill = "#E69F00") + 
    geom_line(data = distridf, aes(x = xdist2, y = ydist2), colour = "#999999" ) + 
    theme(axis.title.x=element_text(size = 8),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + 
    scale_x_continuous("Index test result") +
    annotate("text",  x = 0.05, y = 3.1, label = "With condition", size = 6, colour = "royalblue3" ) + 
    annotate("text",  x = 0.95, y = 3.1, label = "Without condition", size = 6, colour = "royalblue3" ) + 
    geom_text(data = distritext, size = 6, aes(x = cmX, y = cmY, label = labels))
  distri
  
}




