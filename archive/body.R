body <- dashboardBody(
  fluidRow(
    tabBox(
      title = "mymap",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", width = NULL,
      tabPanel("Map", leafletOutput("mymap")),
      tabPanel("Data", "Tab content 2")
    )
    # box(
    #   title = "mymap", width = NULL, status = "primary", solidHeader = TRUE,
    #   leafletOutput("mymap"))
  )
  
  # tabItems(
  #   # First tab content
  #   tabItem(tabName = "dashboard",
  #           fluidRow(
  #             box(
  #               title = "mymap", width = NULL, status = "primary", solidHeader = TRUE,
  #               leafletOutput("mymap"))
  #           ),
  #           fluidRow(
  #               box(
  #                 title = "plot1", width = NULL, status = "primary", solidHeader = TRUE,
  #                 ggvisOutput("plot1"))
  #           )
  #   ),
  #   
  #   # Second tab content
  #   tabItem(tabName = "widgets",
  #           h2("Widgets tab content")
  #   )
  # ),
  # fluidRow(
  #   column(width = 4,
  #          box(
  #            title = "Box title", width = NULL, status = "primary",
  #            "Box content"
  #          ),
  #          box(
  #            title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
  #            "Box content"
  #          ),
  #          box(
  #            width = NULL, background = "black",
  #            "A box with a solid black background"
  #          )
  #   ),
  #   
  #   column(width = 4,
  #          box(
  #            status = "warning", width = NULL,
  #            "Box content"
  #          ),
  #          box(
  #            title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
  #            "Box content"
  #          ),
  #          box(
  #            title = "Title 5", width = NULL, background = "light-blue",
  #            "A box with a solid light-blue background"
  #          )
  #   ),
  #   
  #   column(width = 4,
  #          box(
  #            title = "Title 2", width = NULL, solidHeader = TRUE,
  #            "Box content"
  #          ),
  #          box(
  #            title = "Title 6", width = NULL, background = "maroon",
  #            "A box with a solid maroon background"
  #          )
  #   )
  # )
)