n <- 30
dt <- data.table(
  date=rep(seq(as.Date('2010-01-01'), as.Date('2015-01-01'), by='year'), n/6),
  ind=rpois(n, 5),
  entity=sort(rep(letters[1:5], n/5))
)

setkey(dt, entity, date) # important for ordering
dt[,indpct_fast:=(ind/shift(ind, 1))-1, by=entity]

lagpad <- function(x, k) c(rep(NA, k), x)[1:length(x)]
dt[,indpct_slow:=(ind/lagpad(ind, 1))-1, by=entity]

head(dt, 10)


dt <- data.table(mtcars)[,.(mpg, cyl)]
setkey(dt, cyl)
dt[,mpg_lag1:=shift(mpg, 1, type='lead'), by=cyl]
dt[,mpg_forward1:=shift(mpg, 1, type='lead')]
head(dt, n=20)




library(data.table)
library(DBI)
source("functions/db.R")

menu_data <-
  setDT(dbGetQuery(get_con("dashboard"), "select * from menu_data_set"))
containing_geo_id = "51013"
geo_data_level = "Census Block Group"

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

    readr::read_lines(
      sprintf(
        "menus/selection_menu_data_%s_%s.html",
        geo_data_level,
        containing_geo_id
      )
    )

  }
