library(readr)
library(data.table)
library(sdalr)
library(DBI)
library(R.oo)

setwd("/home/aschroed/git/dashboard/")

# build selection_menu_data.html
createSelectionMenuData <- function(menu_data = menu_data_set, containing_geo_id = "51013", geo_data_level = "Census Block Group") {

  if (!file.exists(sprintf("menus/selection_menu_data_%s_%s.html", geo_data_level, containing_geo_id))) {

    options <- menu_data[GeoDataLevel==geo_data_level & ContainingGeoID==containing_geo_id & DataColumnName %like% ".*E$" ]
    options <- setorder(options, DataSource, DataTable)
    selection_menu_data.html <- tempfile()

    # open top div
    readr::write_lines("<div class='attr-col shiny-input-radiogroup' id='map_poly_data_choice' style='margin-top:10px; margin-bottom:10px;'>",
                       selection_menu_data.html)

    ds <- ""
    for (o in 1:nrow(options)) {

      if (ds != options[o,.(DataSource)][[1]]) {

        if (ds != "") {
          # close list
          readr::write_lines("</ul>",
                             selection_menu_data.html,
                             append = TRUE)

          # close collapsible div
          readr::write_lines("</div>",
                             selection_menu_data.html,
                             append = TRUE)
        }


        ds <- options[o,.(DataSource)][[1]]

        # create collapsible div name
        collapsible_div_name <- gsub(" ", "_", trim(tolower(sprintf("%s_collapsible", ds))))

        # create data source button
        readr::write_lines(sprintf("<button type='button' class='mapLevelButton' data-toggle='collapse' data-target='#%s'>%s</button>", collapsible_div_name, ds),
                           selection_menu_data.html,
                           append = TRUE)

        # open data source collapsible div
        readr::write_lines(sprintf("<div class='collapse' id='%s'>", collapsible_div_name),
                           selection_menu_data.html,
                           append = TRUE)

        # open list
        readr::write_lines("<ul>",
                           selection_menu_data.html,
                           append = TRUE)
      }

      readr::write_lines(paste0("<li><input type='radio' name='map_poly_data_choice' value='",
                                paste(options[o,.(DataSource)], options[o,.(DataColumnName)]),
                                ";",
                                paste(options[o,.(DataSource)], options[o,.(DataColumnName)]),
                                ";",
                                options[o,.(DataColumnName)],
                                ";",
                                options[o,.(ContainingGeoID)],
                                ";",
                                options[o,.(ContainingGeoLat)],
                                ";",
                                options[o,.(ContainingGeoLon)],
                                ";",
                                options[o,.(ContainingGeoZoom)],
                                ";",
                                options[o,.(DataSource)],
                                ";",
                                options[o,.(DataTable)],
                                ";",
                                options[o,.(DataColumnDescription)],
                                ";",
                                options[o,.(ContainingGeoName)],
                                ";",
                                options[o,.(Measure)],
                                "' />&nbsp;&nbsp;",
                                paste(options[o,.(gsub("__", ": ", DataColumnDisplay))])),
                         selection_menu_data.html,
                         append = TRUE)
    }

    # close last list
    readr::write_lines("</ul>",
                       selection_menu_data.html,
                       append = TRUE)

    # close last collapsible div
    readr::write_lines("</div>",
                       selection_menu_data.html,
                       append = TRUE)

    # close top div
    readr::write_lines("</div>",
                       selection_menu_data.html,
                       append = TRUE)

    # read out html
    readr::read_lines(selection_menu_data.html)

    readr::write_lines(readr::read_lines(selection_menu_data.html), sprintf("menus/selection_menu_data_%s_%s.html", geo_data_level, containing_geo_id))
  } else {
    readr::read_lines(sprintf("menus/selection_menu_data_%s_%s.html", geo_data_level, containing_geo_id))
  }
}

con <- con_db(dbname = "dashboard", user = "aschroed", pass = "Iwnftp$2")
menu_data_set <- setDT(dbGetQuery(con, "select * from menu_data_set"))
menu_data_set_cols <- setDT(dbGetQuery(con, "select distinct \"ContainingGeoID\", \"GeoDataLevel\" from menu_data_set"))
res <- mapply(x=menu_data_set_cols$GeoDataLevel, y=menu_data_set_cols$ContainingGeoID, function(x,y) createSelectionMenuData(geo_data_level = x, containing_geo_id = y))
