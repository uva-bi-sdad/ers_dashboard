# # plots.R
#
# makeHistogram <- function(yvar_name, plot_column) {
#   print(paste(yvar_name, plot_column))
#     plot_data <- as.data.frame(va_counties_spdf[c("COUNTY_PAD", plot_column)])
#     # Lables for axes
#     xvar_name <- "County FIPS Code"
#     yvar_name <- yvar_name
#     yvar <- prop("y", as.symbol(plot_column))
#
#     plot_data %>%
#       ggvis(~COUNTY_PAD, yvar, stroke.hover := 200) %>%
#       layer_bars(fill.hover := "red") %>%
#       set_options(width = "auto") %>%
#       add_axis("x", title = "County FIPS Code", tick_padding = 15, properties = axis_props(
#         #axis = list(stroke = "red", strokeWidth = 5),
#         grid = list(stroke = "blue"),
#         ticks = list(stroke = "blue", strokeWidth = 2),
#         labels = list(angle = 270, align = "left", fontSize = 6))) %>%
#       add_axis("y", title = yvar_name) %>%
#       add_tooltip(county_names, "hover") %>%
#       #handle_hover(hoverFunc) %>%
#       bind_shiny("plot1")
# }
#
# makeCorrelation <- function(Var1, Var2) {
#   corrdata <- as.data.frame(va_counties_spdf[, c("COUNTY_PAD", Var1, Var2)])
#   x <- corrdata[,2]
#   y <- corrdata[,3]
#   values$corrCoefficient <- cor(x, y, use = "na.or.complete")
#   corrdata %>%
#     ggvis(~corrdata[,2], ~corrdata[,3]) %>%
#     set_options(width = "auto") %>%
#     layer_points() %>%
#     #layer_model_predictions(model = "lm", se = TRUE) %>%
#     add_axis("x", title = "x", tick_padding = 15, properties = axis_props(
#       #axis = list(stroke = "red", strokeWidth = 5),
#       grid = list(stroke = "blue"),
#       ticks = list(stroke = "blue", strokeWidth = 2),
#       labels = list(angle = 270, align = "left", fontSize = 10))) %>%
#     add_axis("y", title = "y") %>%
#     bind_shiny("corr1")
# }
#
# # change things on map_shape_click
# hoverFunc <- function(data, location, session) {
#   dt <- as.data.table(va_counties_spdf@data)
#   colnames(dt) <- make.unique(tolower(colnames(dt)))
#   q <- sqldf(paste0("SELECT NAME FROM dt WHERE COUNTY_PAD = '", data$x_, "'"))
#   msg <- paste0(as.character(q[1,1]), ": ", round(data$stack_upr_, 2))
#   output$mymap_shape_click <- renderText(msg)
# }
#
# # get county names
# county_names <- function(x) {
#   #print(plotCol())
#   if(is.null(x)) return(NULL)
#   county_fips <- paste0(format(x[1]))
#   county_data <- ""
#   if (county_fips != "") {
#     county_data <- va_counties_spdf[va_counties_spdf$COUNTY_PAD==substr(county_fips,1,3),]
#     county_data <- as.data.frame(county_data[1,])
#   }
#   else {
#     county_data <- va_counties_spdf[va_counties_spdf$COUNTY_PAD=="001",]
#     county_data <- as.data.frame(county_data[1,])
#   }
#   countyInfo <- renderText({ print(paste("<b>County:</b> ",
#                                          county_data[,c("NAME")],
#                                          "<br /><b>Value:</b>",
#                                          if (values$percentage)
#                                            percent(county_data[,c(values$plotColumn)]/100)
#                                          else
#                                            round(county_data[,c(values$plotColumn)], 2)
#                                          )
#                                    )
#     })
#   return(countyInfo())
# }
