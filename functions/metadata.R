# CREATE metadata DATABASE ----------------------------------------------
# metadata_create_db <- function(db_con, overwrite = FALSE) {
#
#   db_cnt <- RPostgreSQL::dbGetQuery(db_con, "select count(*) from pg_catalog.pg_database where datname = 'metadata'")
#   if (db_cnt == 0) {
#     RPostgreSQL::dbSendQuery(db_con, "create database metadata")
#   } else if (overwrite == TRUE) {
#     RPostgreSQL::dbSendQuery(db_con, "drop database metadata")
#     RPostgreSQL::dbSendQuery(db_con, "create database metadata")
#   }
# }


# CREATE metadata TABLES ----------------------------------------------

# create md_datasource
metadata_create_md_datasource <- function(db_con, overwrite = FALSE) {

  md_datasource <- data.table::data.table(
    data_source = character(),
    data_source_abrv = character(),
    friendly_name = character(),
    description = character(),
    critical_changes = character(),
    created = as.Date(character()),
    created_by = character(),
    last_updated = as.Date(character()),
    last_updated_by = character()
  )

  if (overwrite == TRUE) {
    RPostgreSQL::dbSendQuery(db_con, "DROP TABLE IF EXISTS md_datasource CASCADE")
  }
  RPostgreSQL::dbWriteTable(db_con, "md_datasource", md_datasource, row.names=FALSE)
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_datasource ADD PRIMARY KEY (data_source)")
}

# create md_table
metadata_create_md_table <- function(db_con, overwrite = FALSE) {

  md_table <- data.table::data.table(data_source=character(),
                                     table_name=character(),
                                     friendly_name=character(),
                                     description=character(),
                                     critical_changes=character(),
                                     created=as.Date(character()),
                                     created_by=character(),
                                     last_updated=as.Date(character()),
                                     last_updated_by=character())

  if (overwrite == TRUE) {
    RPostgreSQL::dbSendQuery(db_con, "DROP TABLE IF EXISTS md_table CASCADE")
  }
  RPostgreSQL::dbWriteTable(db_con, "md_table", md_table, row.names=FALSE)
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_table ADD PRIMARY KEY (data_source, table_name)")
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_table ADD FOREIGN KEY (data_source) REFERENCES md_datasource(data_source)")
}

# create md_column
metadata_create_md_column <- function(db_con, overwrite = FALSE) {

  md_column <- data.table::data.table(data_source=character(),
                                      table_name=character(),
                                      column_name=character(),
                                      friendly_name=character(),
                                      description=character(),
                                      required=character(),
                                      demographic_type=character(),
                                      domain_type=character(),
                                      measure=character(),
                                      numeric_range_min=numeric(),
                                      numeric_range_max=numeric(),
                                      date_range_min=as.Date(character()),
                                      date_range_max=as.Date(character()),
                                      valid_use_begin_date=as.Date(character()),
                                      valid_use_end_date=as.Date(character()),
                                      original_entry_by=character(),
                                      critical_changes=character(),
                                      created=as.Date(character()),
                                      created_by=character(),
                                      last_updated=as.Date(character()),
                                      last_updated_by=character())

  if (overwrite == TRUE) {
    RPostgreSQL::dbSendQuery(db_con, "DROP TABLE IF EXISTS md_column CASCADE")
  }
  RPostgreSQL::dbWriteTable(db_con, "md_column", md_column, row.names=FALSE)
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_column ADD PRIMARY KEY (data_source, table_name, column_name)")
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_column ADD FOREIGN KEY (data_source) REFERENCES md_datasource(data_source)")
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_column ADD FOREIGN KEY (data_source, table_name) REFERENCES md_table(data_source, table_name)")
}

# create md_valid_values
metadata_create_md_valid_value <- function(db_con, overwrite = FALSE) {

  md_valid_values <- data.table::data.table(data_source=character(),
                                            table_name=character(),
                                            column_name=character(),
                                            value=character(),
                                            description=character(),
                                            valid_use_begin_date=as.Date(character()),
                                            valid_use_end_date=as.Date(character()),
                                            created=as.Date(character()),
                                            created_by=character(),
                                            last_updated=as.Date(character()),
                                            last_updated_by=character())
  if (overwrite == TRUE) {
    RPostgreSQL::dbSendQuery(db_con, "DROP TABLE IF EXISTS md_valid_values CASCADE")
  }
  RPostgreSQL::dbWriteTable(db_con, "md_valid_values", md_valid_values, row.names=FALSE, overwrite=overwrite)
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_valid_values ADD PRIMARY KEY (data_source, table_name, column_name)")
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_valid_values ADD FOREIGN KEY (data_source) REFERENCES md_datasource(data_source)")
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_valid_values ADD FOREIGN KEY (data_source, table_name) REFERENCES md_table(data_source, table_name)")
  RPostgreSQL::dbSendQuery(db_con, "ALTER TABLE md_valid_values ADD FOREIGN KEY (data_source, table_name, column_name) REFERENCES md_column(data_source, table_name, column_name)")
}


# ADD METADATA TO metadata ----------------------------------------------

# add datasource metadata
metadata_add_datasource <- function(db_con,
                                   db_user,
                                   data_source,
                                   data_source_abrv,
                                   friendly_name = "",
                                   description = "",
                                   critical_changes = "") {
  md_datasource_row <- data.table::data.table(
    data_source = data_source,
    data_source_abrv = data_source_abrv,
    friendly_name = friendly_name,
    description = description,
    critical_changes = critical_changes,
    created = as.Date(Sys.Date()),
    created_by = db_user,
    last_updated = as.Date(Sys.Date()),
    last_updated_by = db_user
  )
  RPostgreSQL::dbWriteTable(db_con, "md_datasource", md_datasource_row, row.names = FALSE, append = TRUE)
}

# add table metadata
metadata_add_table <- function(db_con,
                              db_user,
                              data_source,
                              table_name,
                              friendly_name = "",
                              description = "",
                              critical_changes = "") {
  md_table_row <- data.table::data.table(
    data_source = data_source,
    table_name = table_name,
    friendly_name = friendly_name,
    description = description,
    critical_changes = critical_changes,
    created = as.Date(Sys.Date()),
    created_by = db_user,
    last_updated = as.Date(Sys.Date()),
    last_updated_by = db_user
  )

  cnt <- RPostgreSQL::dbGetQuery(db_con, sprintf("select count(*) from md_table where table_name = '%s'", table_name))
  if (cnt > 0) {
    RPostgreSQL::dbSendQuery(db_con, sprintf("delete from md_table where table_name = '%s'", table_name))
  }
  RPostgreSQL::dbWriteTable(db_con, "md_table", md_table_row, row.names = FALSE, append = TRUE)
}

# add column metadata
metadata_add_column <- function(db_con,
                               db_user,
                               data_source,
                               table_name,
                               column_name,
                               friendly_name = "",
                               description = "",
                               measure = "",
                               critical_changes = "") {
  md_column_row <- data.table::data.table(
    data_source = data_source,
    table_name = table_name,
    column_name = column_name,
    friendly_name = friendly_name,
    description = description,
    measure = measure,
    critical_changes = critical_changes,
    created = as.Date(Sys.Date()),
    created_by = db_user,
    last_updated = as.Date(Sys.Date()),
    last_updated_by = db_user
  )

  cnt <- RPostgreSQL::dbGetQuery(db_con, sprintf("select count(*) from md_column where table_name = '%s' and column_name = '%s'", table_name, column_name))
  if (cnt > 0) {
    RPostgreSQL::dbSendQuery(db_con, sprintf("delete from md_column where table_name = '%s' and column_name = '%s'", table_name, column_name))
  }
  RPostgreSQL::dbWriteTable(db_con, "md_column", md_column_row, row.names = FALSE, append = TRUE)
}

# add valid value metadata
metadata_add_valid_value <- function(db_con,
                                    db_user,
                                    data_source,
                                    table_name,
                                    column_name,
                                    value,
                                    description) {
  md_valid_value_row <- data.table::data.table(
    data_source = data_source,
    table_name = table_name,
    column_name = column_name,
    value = value,
    description = description,
    created = as.Date(Sys.Date()),
    created_by = db_user,
    last_updated = as.Date(Sys.Date()),
    last_updated_by = db_user
  )

  RPostgreSQL::dbWriteTable(db_con, "md_valid_value", md_valid_value_row, row.names = FALSE, append = TRUE)
}

friendly_name <- function(str) {
  library(lettercase)
  str %>%
    gsub("!", " ", .) %>%
    gsub("  ", " ", .) %>%
    gsub("__+", ": ", .) %>%
    gsub("_", " ", .) %>%
    str_lower(.) %>%
    str_title_case(.)
}
