# ui.R
shinyUI(dashboardPage(
  dashboardHeader(title = "CLD3 Dashboard"),
  dashboardSidebar(
    includeCSS("styles.css"),
    uiOutput("tit0"),
    div(class = "mapLevelDiv",
        uiOutput("map_level")),
    uiOutput("countySelect"),
    uiOutput("tit1"),
    uiOutput("areaDataMenu"),
    uiOutput("locationDataMenu")
  ),
  dashboardBody(fluidRow(
    tabBox(
      title = textOutput("mapTitle"),
      id = "tabset1",
      width = NULL,
      tabPanel("Map",
               box(leafletOutput("mymap"),
                   width = NULL,
                   title = uiOutput('map_title'),
                   solidHeader = TRUE),
               box(plotlyOutput("map_hist_plotly"),
                   width = NULL,
                   title = uiOutput('map_bar_title'))
      ),
      tabPanel("Analysis",
               box(
                 uiOutput("cor1"),
                 textOutput("analysis_corr_coef"),
                 plotlyOutput("corr1_plot"),
                 title = "Correlation",
                 style = "min-height:400px",
                 width = NULL
               ),
               box(
                 uiOutput("ts1"),
                 plotlyOutput("ts1_plotly"),
                 title = "Time Series",
                 width = NULL
               )
      ),
      tabPanel("Data",
               box(
                 DT::dataTableOutput("map_data_dt"),
                 title = "Data1",
                 width = NULL
               )
      )
    )
  ))
))
