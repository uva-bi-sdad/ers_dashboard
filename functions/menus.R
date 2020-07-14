# selection_menus.R
library(data.table)
library(sdalr)
library(DBI)

con <-
  con_db(dbname = "dashboard",
         user = "aschroed",
         pass = "Iwnftp$2")

menu_data_set <-
  setDT(dbGetQuery(con, "select * from menu_data_set"))

# build selection_menu_map_level.html
createSelectionMenuMapLevel <- function() {
  if (!file.exists("menus/selection_menu_map_level.html")) {
    selection_menu_map_level.html <- tempfile()
    readr::write_lines(
      "<div class='attr-col shiny-input-radiogroup' id='map_level_choice' style='margin-top:10px; margin-bottom:10px;'>",
      selection_menu_map_level.html
    )
    readr::write_lines("<ul>", selection_menu_map_level.html, append = TRUE)
    readr::write_lines(
      "<li><input type='radio' name='map_level_choice' value='County' checked='checked' />&nbsp;&nbsp;State Counties",
      selection_menu_map_level.html,
      append = TRUE
    )
    readr::write_lines(
      "<li><input type='radio' name='map_level_choice' value='Census Block Group' />&nbsp;&nbsp;County Block Groups",
      selection_menu_map_level.html,
      append = TRUE
    )
    readr::write_lines("</ul>", selection_menu_map_level.html, append = TRUE)
    readr::write_lines("</div>", selection_menu_map_level.html, append = TRUE)
    readr::read_lines(selection_menu_map_level.html)

    readr::write_lines(
      readr::read_lines(selection_menu_map_level.html),
      "menus/selection_menu_map_level.html"
    )
  } else {
    readr::read_lines("menus/selection_menu_map_level.html")
  }
}

# build selection_menu_states.html
createSelectionMenuStates <- function(menu_data = menu_data_set) {
  if (!file.exists("menus/selection_menu_states.html")) {
    state_substate_fips <-
      setDT(dbGetQuery(con, "select * from state_substate_fips"))

    options <-
      menu_data[, .(stfp = unique(substr(ContainingGeoID, 1, 2)))]
    options <-
      leftJoin(options, state_substate_fips, "stfp", "STATEFP")
    options <-
      options[, .(STATE, STATEFP), c("STATE", "STATEFP")][, 1:2]
    options[, STATEFP := padFIPS(STATEFP)]
    options <- setorder(options, STATE)

    selection_menu_states.html <- tempfile()

    readr::write_lines(
      "<div class='countySelect' id='map_state_choice_div'>",
      selection_menu_states.html
    )
    readr::write_lines(
      "<select name='map_state_choice' class='countySelect'>",
      selection_menu_states.html,
      append = TRUE
    )
    for (o in 1:nrow(options)) {
      readr::write_lines(
        paste0("<option value='", options[, STATEFP], "'>", options[, STATE], "</option>"),
        selection_menu_states.html,
        append = TRUE
      )
    }
    readr::write_lines("</select>", selection_menu_states.html, append = TRUE)
    readr::write_lines("</div>", selection_menu_states.html, append = TRUE)

    readr::read_lines(selection_menu_states.html)

    readr::write_lines(
      readr::read_lines(selection_menu_states.html),
      "menus/selection_menu_states.html"
    )
  } else {
    readr::read_lines("menus/selection_menu_states.html")
  }
}

# build selection_menu_state_counties.html
createSelectionMenuStateCounties <-
  function(menu_data = menu_data_set,
           state_fips = "51") {
    if (!file.exists(sprintf("menus/selection_menu_state_%s_counties.html", state_fips))) {
      options <-
        menu_data[substr(ContainingGeoID, 1, 2) == state_fips &
                    nchar(ContainingGeoID) == 5,
                  .(ContainingGeoID, ContainingGeoName),
                  by = c("ContainingGeoID", "ContainingGeoName")][, 1:2]
      options <- setorder(options, ContainingGeoName)

      selection_menu_state_counties.html <- tempfile()

      readr::write_lines("<div class='countySelect'>",
                         selection_menu_state_counties.html)
      readr::write_lines(
        "<select name='map_county_choice' class='countySelect'>",
        selection_menu_state_counties.html,
        append = TRUE
      )
      for (o in 1:nrow(options)) {
        readr::write_lines(
          paste0("<option value='", options[, ContainingGeoID], "'>", options[, ContainingGeoName], "</option>"),
          selection_menu_state_counties.html,
          append = TRUE
        )
      }
      readr::write_lines("</select>", selection_menu_state_counties.html, append = TRUE)
      readr::write_lines("</div>", selection_menu_state_counties.html, append = TRUE)

      readr::read_lines(selection_menu_state_counties.html)

      readr::write_lines(
        readr::read_lines(selection_menu_state_counties.html),
        sprintf(
          "menus/selection_menu_state_%s_counties.html",
          state_fips
        )
      )
    } else {
      readr::read_lines(sprintf(
        "menus/selection_menu_state_%s_counties.html",
        state_fips
      ))
    }
  }

# build selection_menu_data.html
createSelectionMenuData <-
  function(menu_data = menu_data_set,
           containing_geo_id = "51013",
           geo_data_level = "Census Block Group") {
    if (!file.exists(
      sprintf(
        "menus/selection_menu_data_%s_%s.html",
        geo_data_level,
        containing_geo_id
      )
    )) {
      options <-
        menu_data[GeoDataLevel == geo_data_level &
                    ContainingGeoID == containing_geo_id &
                    DataColumnName %like% ".*E$"]
      options <- setorder(options, DataSource, DataTable)
      options[, ds_lead := shift(DataTable, 1, type = 'lead'), by = DataSource]
      options[, ds_lag := shift(DataTable, 1), by = DataSource]

      t <-
        options[, .(
          result = sprintf(
            "%s<li><input type='radio' name='map_poly_data_choice' value='%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s' />&nbsp;&nbsp;%s %s",
            ifelse(
              is.na(ds_lag),
              sprintf(
                "<button type='button' class='mapLevelButton' data-toggle='collapse' data-target='#%s'>%s</button><div class='collapse' id='%s'><ul>",
                gsub(" ", "_", trimws(tolower(
                  sprintf("%s_collapsible", DataSource)
                ), which = "both")),
                DataSource,
                gsub(" ", "_", trimws(tolower(
                  sprintf("%s_collapsible", DataSource)
                ), which = "both"))
              ),
              ""
            ),
            sprintf("%s %s", DataSource, DataColumnName),
            sprintf("%s %s", DataSource, DataColumnName),
            DataColumnName,
            ContainingGeoID,
            ContainingGeoLat,
            ContainingGeoLon,
            ContainingGeoZoom,
            DataSource,
            DataTable,
            DataColumnDescription,
            ContainingGeoName,
            Measure,
            gsub("__", ": ", DataColumnDisplay),
            ifelse(is.na(ds_lead), "</ul></div>",  "")
          )
        )]

      selection_menu_data.html <-
        sprintf(
          "<div class='attr-col shiny-input-radiogroup' id='map_poly_data_choice' style='margin-top:10px; margin-bottom:10px;'>%s</div>",
          paste(t[[1]], collapse = " ")
        )

      readr::write_file(
        selection_menu_data.html,
        sprintf(
          "menus/selection_menu_data_%s_%s.html",
          geo_data_level,
          containing_geo_id
        )
      )

    }

    readr::read_file(
      sprintf(
        "menus/selection_menu_data_%s_%s.html",
        geo_data_level,
        containing_geo_id
      )
    )

  }

# createSelectionMenuData <- function(menu_data = menu_data_set, containing_geo_id = "51013", geo_data_level = "Census Block Group") {
#
#   if (!file.exists(sprintf("menus/selection_menu_data_%s_%s.html", geo_data_level, containing_geo_id))) {
#
#   options <- menu_data[GeoDataLevel==geo_data_level & ContainingGeoID==containing_geo_id]
#   options <- setorder(options, DataSource, DataTable)
#   selection_menu_data.html <- tempfile()
#
#   # open top div
#   readr::write_lines("<div class='attr-col shiny-input-radiogroup' id='map_poly_data_choice' style='margin-top:10px; margin-bottom:10px;'>",
#                      selection_menu_data.html)
#
#   ds <- ""
#   for (o in 1:nrow(options)) {
#
#     if (ds != options[o,.(DataSource)][[1]]) {
#
#       if (ds != "") {
#         # close list
#         readr::write_lines("</ul>",
#                            selection_menu_data.html,
#                            append = TRUE)
#
#         # close collapsible div
#         readr::write_lines("</div>",
#                            selection_menu_data.html,
#                            append = TRUE)
#       }
#
#
#       ds <- options[o,.(DataSource)][[1]]
#
#       # create collapsible div name
#       collapsible_div_name <- gsub(" ", "_", trim(tolower(sprintf("%s_collapsible", ds))))
#
#       # create data source button
#       readr::write_lines(sprintf("<button type='button' class='mapLevelButton' data-toggle='collapse' data-target='#%s'>%s</button>", collapsible_div_name, ds),
#                          selection_menu_data.html,
#                          append = TRUE)
#
#       # open data source collapsible div
#       readr::write_lines(sprintf("<div class='collapse' id='%s'>", collapsible_div_name),
#                          selection_menu_data.html,
#                          append = TRUE)
#
#       # open list
#       readr::write_lines("<ul>",
#                          selection_menu_data.html,
#                          append = TRUE)
#     }
#
#       readr::write_lines(paste0("<li><input type='radio' name='map_poly_data_choice' value='",
#                                 paste(options[o,.(DataSource)], options[o,.(DataColumnName)]),
#                                 ";",
#                                 paste(options[o,.(DataSource)], options[o,.(DataColumnName)]),
#                                 ";",
#                                 options[o,.(DataColumnName)],
#                                 ";",
#                                 options[o,.(ContainingGeoID)],
#                                 ";",
#                                 options[o,.(ContainingGeoLat)],
#                                 ";",
#                                 options[o,.(ContainingGeoLon)],
#                                 ";",
#                                 options[o,.(ContainingGeoZoom)],
#                                 ";",
#                                 options[o,.(DataSource)],
#                                 ";",
#                                 options[o,.(DataTable)],
#                                 ";",
#                                 options[o,.(DataColumnDescription)],
#                                 ";",
#                                 options[o,.(ContainingGeoName)],
#                                 ";",
#                                 options[o,.(Measure)],
#                                 "' />&nbsp;&nbsp;",
#                                 paste(options[o,.(gsub("__", ": ", DataColumnDisplay))])),
#                          selection_menu_data.html,
#                          append = TRUE)
#   }
#
#   # close last list
#   readr::write_lines("</ul>",
#                      selection_menu_data.html,
#                      append = TRUE)
#
#   # close last collapsible div
#   readr::write_lines("</div>",
#                        selection_menu_data.html,
#                        append = TRUE)
#
#   # close top div
#   readr::write_lines("</div>",
#                      selection_menu_data.html,
#                      append = TRUE)
#
#   # read out html
#   readr::read_lines(selection_menu_data.html)
#
#   readr::write_lines(readr::read_lines(selection_menu_data.html), sprintf("menus/selection_menu_data_%s_%s.html", geo_data_level, containing_geo_id))
#   } else {
#     readr::read_lines(sprintf("menus/selection_menu_data_%s_%s.html", geo_data_level, containing_geo_id))
#   }
# }


# Location Points Menu
selectionMenuDataPoints <- function() {

  if (!file.exists("menus/selection_menu_data_point_level.html")) {
    selection_menu_data_point_level.html <- tempfile()

    readr::write_lines("<div class='attr-col shiny-input-checkboxgroup' id='locationData'><ul>", selection_menu_data_point_level.html)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_snap_retailers_spdf;Store_Name;Address;City;Green'  />&nbsp;&nbsp;Snap Retailers</li>", selection_menu_data_point_level.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_grocery_stores_spdf;name;address;locality;Blue'  />&nbsp;&nbsp;Grocery Stores</li>", selection_menu_data_point_level.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_places_of_worship_spdf;name;address;locality;Black'  />&nbsp;&nbsp;Places of Worship</li>", selection_menu_data_point_level.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_hospitals_spdf;NAME;ADDRESS;CITY;Red'  />&nbsp;&nbsp;Hospitals</li>", selection_menu_data_point_level.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_schools_spdf;NAME;ADDRESS;CITY;Orange'  />&nbsp;&nbsp;Schools</li>", selection_menu_data_point_level.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_mobile_home_parks_spdf;NAME;ADDRESS;CITY;Violet'  />&nbsp;&nbsp;Mobile Home Parks</li>", selection_menu_data_point_level.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_libraries_spdf;name;address;county_name;Brown'  />&nbsp;&nbsp;Libraries</li>", selection_menu_data_point_level.html, append = TRUE)
    # readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_fast_food_spdf;name;address;county_name;Purple'  />&nbsp;&nbsp;Fast Food</li>", selection_menu_data_point_level.html, append = TRUE)
    readr::write_lines("</ul></div>", selection_menu_data_point_level.html, append = TRUE)

    readr::write_lines(
      readr::read_lines(selection_menu_data_point_level.html),
      "menus/selection_menu_data_point_level.html"
    )
  } else {
    readr::read_lines("menus/selection_menu_data_point_level.html")
  }
}

selectionMenuDataPointsDisabled <- function() {
  if (!file.exists("menus/selection_menu_data_point_level_disabled.html")) {
    selection_menu_data_point_level_disabled.html <- tempfile()

    readr::write_lines("<div class='attr-col shiny-input-checkboxgroup' id='locationData'><ul>", selection_menu_data_point_level_disabled.html)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_snap_retailers_spdf;Store_Name;Address;City;Green' disabled />&nbsp;&nbsp;Snap Retailers</li>", selection_menu_data_point_level_disabled.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_grocery_stores_spdf;name;address;locality;Blue' disabled />&nbsp;&nbsp;Grocery Stores</li>", selection_menu_data_point_level_disabled.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_places_of_worship_spdf;name;address;locality;Black' disabled />&nbsp;&nbsp;Places of Worship</li>", selection_menu_data_point_level_disabled.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_hospitals_spdf;NAME;ADDRESS;CITY;Red' disabled />&nbsp;&nbsp;Hospitals</li>", selection_menu_data_point_level_disabled.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_schools_spdf;NAME;ADDRESS;CITY;Orange' disabled />&nbsp;&nbsp;Schools</li>", selection_menu_data_point_level_disabled.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_mobile_home_parks_spdf;NAME;ADDRESS;CITY;Violet' disabled />&nbsp;&nbsp;Mobile Home Parks</li>", selection_menu_data_point_level_disabled.html, append = TRUE)
    readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_libraries_spdf;name;address;county_name;Brown' disabled />&nbsp;&nbsp;Libraries</li>", selection_menu_data_point_level_disabled.html, append = TRUE)
    # readr::write_lines("<li><input type='checkbox' name='locationData' id='var1' value='va_fast_food_spdf;name;address;county_name;Purple' disabled />&nbsp;&nbsp;Fast Food</li>", selection_menu_data_point_level_disabled.html, append = TRUE)
    readr::write_lines("</ul></div>", selection_menu_data_point_level_disabled.html, append = TRUE)

    readr::write_lines(
      readr::read_lines(selection_menu_data_point_level_disabled.html),
      "menus/selection_menu_data_point_level_disabled.html"
    )
  } else {
    readr::read_lines("menus/selection_menu_data_point_level_disabled.html")
  }
}

# Correlation DropDown Menu
# createCorrelationDropDown <- function(cont_geo_id,
#                                       geo_data_level,
#                                       menu_data = menu_data_set) {
#   print("!@!@!@ Getting correlation dropdown values @#@#@#")
#   print('cont_geo_id')
#   print(cont_geo_id)
#
#   print('geo_data_level')
#   print(geo_data_level)
#
#   # print('menu_data')
#   # print(head(menu_data))
#
#   menu_data_sub <- menu_data[menu_data$ContainingGeoID == cont_geo_id &
#                                menu_data$GeoDataLevel == geo_data_level, ]
#
#   # print(head(menu_data_sub))
#
#   drop_down_values = menu_data_sub$DataColumnDisplay
#
#   return(drop_down_values)
#
# }

# Build menu_data_set ----------------------------------------------
CreateDataSourceMenuItems <- function(dataTableName, dataSource, dataSourceAbrv, geoDataLevel, dataColumns, containingGeoIDCol) {
  dt <- data.table::setDT(dbGetQuery(db_con, paste0("select * from ", dataTableName)))

  md_column <- data.table::setDT(dbGetQuery(db_con, "select * from md_column"))

  if (geoDataLevel=="Census Block Group") {
    fips_tbl <-
      dbGetQuery(
        db_con,
        'select "ST_SUBST_FIPS" \"ContainingGeoID\", "SUBSTNAME" \"ContainingGeoName\", "INTPTLAT" \"ContainingGeoLat\", "INTPTLON" \"ContainingGeoLon\", "ZOOM" \"ContainingGeoZoom\" from state_substate_fips'
      )
  } else {
    fips_tbl <-
      dbGetQuery(
        db_con,
        'select "STATEFP" \"ContainingGeoID\", "STATE" \"ContainingGeoName\", "INTPTLAT" \"ContainingGeoLat\", "INTPTLON" \"ContainingGeoLon\", "ZOOM" \"ContainingGeoZoom\" from state_substate_fips where "SUBSTTYPE" = \'State\''
      )
  }


  dt_f <- data.table::data.table(
    ContainingGeoID = as.character(),
    ContainingGeoName = as.character(),
    DataColumnName = as.character(),
    DataSource = as.character(),
    DataSourceAbrv = as.character(),
    DataTable = as.character(),
    GeoDataLevel = as.character(),
    ContainingGeoLat = as.character(),
    ContainingGeoLon = as.character(),
    ContainingGeoZoom = as.character()
  )
  #dt_f <- list(dt_0)
  for (col in dataColumns) {
    dt_1 <- data.table::setDT(unique(dt[, .(
      ContainingGeoID = dt[, get(containingGeoIDCol)],
      DataColumnName = col,
      DataSource = dataSource,
      DataSourceAbrv = dataSourceAbrv,
      DataTable = dataTableName,
      GeoDataLevel = geoDataLevel
    )]))

    dt_2 <- leftJoin(dt_1, fips_tbl, "ContainingGeoID", "ContainingGeoID")
    dt_2 <- dt_2[,.(ContainingGeoID, ContainingGeoName, DataColumnName, DataSource, DataSourceAbrv, DataTable, GeoDataLevel, ContainingGeoLat, ContainingGeoLon, ContainingGeoZoom)]
    dt_f <- data.table::rbindlist(list(dt_f, dt_2))
  }

  dt_f

  # dt_f <- leftJoin(dt_f, md_column, "DataColumnName", "column_name")
  # dt_f[, .(ContainingGeoID, ContainingGeoName, DataColumnName=column_name, DataSource, DataSourceAbrv, DataTable, GeoDataLevel, ContainingGeoLat, ContainingGeoLon, ContainingGeoZoom, Measure=measure)]

}

## PREBUILD DATA SELECTION MENUS ----------------------------------------------
prebuild_menu_data_items <- function() {
  library(sdalr)
  library(data.table)
  library(sdalr)
  library(DBI)
  source("functions/metadata.R")
  source("functions/dataset.R")
  source("functions/menus.R")
  source("functions/db.R")
  source("functions/dataset.R")
  db_con <- get_con("dashboard")
  db_user <- "aschroed"

  menu_data_set <- setDT(dbGetQuery(db_con, "select * from menu_data_set"))
  menu_data_set_cols <- setDT(dbGetQuery(db_con, "select distinct \"ContainingGeoID\", \"GeoDataLevel\" from menu_data_set"))
  res <- mapply(x=menu_data_set_cols$GeoDataLevel, y=menu_data_set_cols$ContainingGeoID, function(x,y) createSelectionMenuData(geo_data_level = x, containing_geo_id = y))
}
