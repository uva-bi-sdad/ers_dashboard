
get_osm_places <- function(q = "fast food arlington va",
                    key = "9FbEBUMQ1lb1DF4c6BLAO7ihPSl0Dk4g",
                    format = "json",
                    addressdetails = "1",
                    limit = "50",
                    url_base = "http://open.mapquestapi.com/nominatim/v1/search.php?",
                    exclude_place_ids = "") {

  if (exists("response_dt")) rm(response_dt)

  response_dt <- data.table::data.table()

  for (i in 1:10) {
    if (exists("resp") == TRUE) {
      if (nrow(resp) == 0) {
        break()
      }
    }

    url <- utils::URLencode(sprintf("%skey=%s&format=%s&q=%s&addressdetails=%s&limit=%s&exclude_place_ids=%s",
                                    url_base,
                                    key,
                                    format,
                                    q,
                                    addressdetails,
                                    limit,
                                    exclude_place_ids))

    # print(nchar(url))

    resp_j <- RCurl::getURL(url)
    resp <- data.table::setDT(jsonlite::fromJSON(resp_j, flatten = TRUE))

    print(nrow(resp))

    if (nrow(resp) > 0) {
      ifelse(
        exists("response_dt") == FALSE,
        response_dt <- resp,
        response_dt <- data.table::rbindlist(list(response_dt, resp), fill = TRUE)
      )
      ifelse(
        nchar(exclude_place_ids) > 0,
        exclude_place_ids <- paste0(exclude_place_ids, ",", paste0(response_dt$place_id, collapse = ",")),
        exclude_place_ids <- paste0(response_dt$place_id, collapse = ",")
      )

      i <- i + 1
    }
  }

  response_dt
}


