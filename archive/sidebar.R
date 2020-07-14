sidebar <- dashboardSidebar(
  includeCSS("styles.css"),
  selectizeInput("select", label = h4("Select Map Type"),
                 choices = list("Access and Proximity to Grocery Store" = 1, "Food Assistance" = 2, "Store Availability" = 3, "Sexual Activity Rates" = 4),
                 selected = 1),
  checkboxInput("showSnap", "Show/Hide Snap Retailers"),
  htmlOutput("mymap_shape_click"),
  radioButtons(inputId="mapLevel", label="Map Level",
               choices=c("County","Block Group", "School District")),
  tags$hr(style = "border-top: 2px solid #8c8b8b;"),
  div(style = "max-height:250px; overflow-y: auto;",
      conditionalPanel(
        condition = "input.mapLevel == 'County'"
        
        
      ))
  )