
### Load libraries
#library(flexdashboard)
require(data.table)
require(ggplot2)
library(stringr)
require(shiny)
require(shinydashboard)
require(shinythemes)
#require(leaflet)
#require(htmltools)
#require(rgdal)
require(heatmaply)

### Read data
p <- fread("ShinyProgrammingExercise_2020-04/Random_PatientLevelInfo_2020.tsv")
l <- fread("ShinyProgrammingExercise_2020-04/Random_LabValuesInfo_2020.tsv")
### Small initial checks:
any(duplicated(p$USUBJID))      ### All Patient IDs are unique
length(unique(l$STUDYID)) == 1  ### We have a single STUDYID
table(l$LBTESTCD, l$LBTEST)     ### We have 3 types of tests LBTESTCD is a shorn name for LBTEST
table(l$LBTESTCD, l$LBCAT)      ### We have 2 types of test categories (CHEMISTRY and IMMUNOLOGY)

### Samples seem to be coming from multiple locations
### Get country info
l <- l[, COUNTRY := str_split_fixed(USUBJID, "-", 3)[,2]]
lcorr <- l[,.(AVALnorm= (AVAL/(AVAL[AVISIT %in% "BASELINE"])) * 100 ) , by=.(USUBJID, LBTESTCD)]
l <- cbind(l, lcorr[,"AVALnorm"])
l <- l[p[, -1], on="USUBJID"]
map <- seq_along(levels(factor(l$AVISIT)))
names(map) <- levels(factor(l$AVISIT))[c(2,1,3:7)]
l <- l[, AVISITnum := map[AVISIT]]
l <- l[, Biomarker1_median_split := ifelse(BMRKR2>median(BMRKR1), yes=c("above median", "below median"))]
p <- p[, COUNTRY := str_split_fixed(USUBJID, "-", 3)[,2]]
nms <- unique(l$AVISIT)
names(nms) <- nms


ui <- dashboardPage(
  skin="black",
  dashboardHeader(
    title="Roche Task - Random Patient and Lab Info",
    titleWidth="38%"),
  dashboardSidebar(
    ### TIME SERIES INPUTS
    radioButtons(inputId = "test_type",
                 label = "TIME SERIES - Test type:",
                 choices = c("ALT" = "ALT",
                             "CRP" = "CRP",
                             "IGA" = "IGA")
    ),
    selectInput(inputId = "group_type",
                label="TIME SERIES - Group by:",
                choices = c("Biomarker 1 median split" = "Biomarker1_median_split",
                            "Biomarker 2" = "BMRKR2",
                            "Country" = "COUNTRY",
                            "Sex" = "SEX",
                            "Race" = "RACE",
                            "Arm" = "ACTARM")
    ),
    radioButtons(inputId = "norm_type",
                 label = "TIME SERIES - Test values:",
                 choices = c("Normalized" = "AVALnorm",
                             "Raw" = "AVAL")
    ),
    sliderInput("age_range", "TIME SERIES - Age:",
                min = min(l$AGE), max = max(l$AGE),
                value = c(min(l$AGE), max(l$AGE))
    ),
    ### BOXPLOT INPUTS
    radioButtons(inputId = "d_boxplot_test_type",
                 label = "BOXPLOT - Test type:",
                 choices = c("ALT" = "ALT",
                             "CRP" = "CRP",
                             "IGA" = "IGA")
    ),
    selectInput(inputId = "avisit_type",
                label="BOXPLOT - Select timepoint:",
                choices = nms
    ),
    radioButtons(inputId = "d_boxplot_fill",
                 label = "BOXPLOT - break data by:",
                 choices = c("Biomarker 1 median split" = "Biomarker1_median_split",
                             "Biomarker 2" = "BMRKR2",
                             "Country" = "COUNTRY",
                             "Sex" = "SEX",
                             "Race" = "RACE",
                             "Arm" = "ACTARM")
    ),
    ### BARPLOT INPUTS
    radioButtons(inputId = "d_barplot_test_type",
                 label = "BARPLOT - Test type:",
                 choices = c("ALT" = "ALT",
                             "CRP" = "CRP",
                             "IGA" = "IGA")
    ),
    radioButtons(inputId = "d_barplot_fill",
                 label = "BARPLOT - break data by:",
                 choices = c("Biomarker 1 median split" = "Biomarker1_median_split",
                             "Biomarker 2" = "BMRKR2",
                             "Country" = "COUNTRY",
                             "Sex" = "SEX",
                             "Race" = "RACE",
                             "Arm" = "ACTARM")
    )
  ),
  dashboardBody(
    br(),
    br(),
    p("Time series AVAL (normalized or raw) for each patient - color fill by various metadata, biomarkers, age range to be selected"),
    plotOutput("time_series"),
    br(),
    br(),
    p("Boxplot - normalized AVAL plotted by country and user-selected visit timepoint, color fill by various metadata"),
    plotOutput("d_boxplot"),
    br(),
    br(),
    p("Barplot - various data types plotted by country, plotly-iteractive on hover"),
    plotlyOutput("d_barplot")
  )
)

server <- function(input, output) {
  output$time_series <- renderPlot({
    ### Plot time series
    ### Data filtering - radioButton & sliderInput
    d <- l[l[["LBTESTCD"]] %in% input$test_type &
             l[["AGE"]] >= input$age_range[1] &
             l[["AGE"]] <= input$age_range[2], ]
    ### Plot ggplot with time series:
    ggplot() +
      geom_path(data=d, aes(x=AVISITnum,
                            y=d[[input$norm_type]],
                            group=USUBJID,
                            color=factor(d[[input$group_type]]))) +
      ylab(ifelse(input$norm_type=="AVALnorm",
                  yes="AVALnorm [(U/L) / (U/L)]",
                  no=paste0("AVAL"," [", unique(d$AVALU), " ]"))) +
      scale_x_discrete(labels=names(map),
                       limits=map,
                       name="AVISIT") +
      labs(color = input$group_type)
  })
  ## Plot boxplot AVALnorm by timepoint and fill with country, race and other information to choose from
  output$d_boxplot <- renderPlot({
    b <- l[l[["AVISIT"]] %in% input$avisit_type & l[["LBTESTCD"]] %in% input$d_boxplot_test_type ]
    ggplot() + geom_boxplot(data = b, aes(x=COUNTRY, y=AVALnorm, fill = b[[input$d_boxplot_fill]])) + labs(fill = input$d_boxplot_fill)
  })
  ### Plot barplot AVALnorm by timepoint and fill with country, race and other information to choose from
  output$d_barplot <- renderPlotly({
    b <- l[l[["AVISIT"]] %in% "BASELINE" & l[["LBTESTCD"]] %in% input$d_barplot_test_type ]
    print(
      ggplotly(
        ggplot() + geom_bar(data = b, aes(COUNTRY, fill=b[[input$d_barplot_fill]])) + labs(fill = input$d_barplot_fill)
      )
    )
  })
}

shinyApp(ui = ui, server = server)

