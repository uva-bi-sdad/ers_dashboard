get_kidscount <-
  function(data_source,
           data_source_abrev,
           category,
           sub_category,
           url,
           by = "",
           definitions = "",
           location_col = "Location",
           containing_geo_name = "VA",
           out_dir = "data/dash_tables") {
    # download and rename file
    filepath <- file.path(out_dir, "temp_file")
    tmp <- httr::GET(url, httr::write_disk(filepath, overwrite = TRUE))
    file_info <-
      stringr::str_match(httr::headers(tmp)$`content-disposition`, "\"(.*)[.](.*)\"")
    file_name <- file_info[2]
    file_type <- file_info[3]
    filename <-
      tolower(make.names(
        paste(data_source_abrev, file_name, containing_geo_name, file_type)
      ))
    newfilepath <- file.path(out_dir, filename)
    file.rename(filepath, newfilepath)

    rtrn <- ""
    # read file to data.table, add columns
    if (file_type == "xlsx") {
      data <- data.table::setDT(readxl::read_excel(newfilepath))
      data$display_name <- file_name
      data$data_source <- data_source
      data$data_source_abrev <- data_source_abrev
      data$by <- by
      data$by_label <- ifelse(by == "", by, data[, .(get(by))])
      data[, (location_col) := toupper(get(location_col))]

      # get Virginia counties
      va_county_name <-
        data.table::setDT(readRDS("data/CENSUS/va_counties_simplified_spdf.RDS")@data)[, .(NAME, NAMELSAD, COUNTYFP)]
      va_county_namelsad <-
        data.table::setDT(readRDS("data/CENSUS/va_counties_simplified_spdf.RDS")@data)[, .(NAMELSAD, NAMELSAD, COUNTYFP)]
      va_counties <-
        data.table::rbindlist(list(va_county_name, va_county_namelsad))
      va_counties[, NAME := toupper(NAME)]

      # set keys
      data.table::setkeyv(data, location_col)
      data.table::setkey(va_counties, NAME)

      # join tables, set FIPS Codes
      joined <- data[va_counties, nomatch = 0]
      joined[, ST_SUBST_FIPS := paste0("51", COUNTYFP)]
      joined[, CONTAINING_GEOID := "51"]
      joined[, CATEGORY := category]
      joined[, SUB_CATEGORY := sub_category]
      joined[, DEFINITIONS := definitions]

      # set final data.table to return
      final_dt <-
        joined[, .(
          NAMELSAD,
          YEAR = TimeFrame,
          CATEGORY,
          SUB_CATEGORY,
          DISPLAY_NAME = display_name,
          BY = by,
          BY_LABEL = by_label,
          MEASURE = DataFormat,
          DESCRIPTION = paste0(
            display_name,
            ", ",
            NAMELSAD,
            ", ",
            TimeFrame,
            ", ",
            data_source
          ),
          DEFINITIONS,
          DATA_SOURCE = data_source,
          DATA_SOURCE_ABREV = data_source_abrev,
          ST_SUBST_FIPS,
          CONTAINING_GEOID,
          VALUE = Data
        )]

      # return
      rtrn <- final_dt
    }

    csv_filename <-
      tolower(make.names(
        paste(data_source_abrev, file_name, containing_geo_name, "csv")
      ))
    csv_filepath <- file.path(out_dir, csv_filename)
    write.csv(rtrn, csv_filepath, row.names = FALSE)
    unlink(newfilepath)
    rtrn
  }
