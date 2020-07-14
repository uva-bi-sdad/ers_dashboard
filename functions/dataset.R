
# Dataset Utility Functions --------------------------------------------------------
library(data.table)
library(magrittr)

# given a data.table, fill column NAs with preceeding value (top-to-bottom)
fill_column_nas <- function(dt, col_name){
  dt <- setDT(dt)
  for (row in 1:nrow(dt)) {
    if (is.na(dt[row, .(get(col_name))])) {
      dt[row, eval(col_name) := dt[row - 1, get(col_name)]]
    }
  }
  dt
}

# create usable column names
create_colnames <- function(column_names) {
  col_abbrevs <- data.table::fread("settings/column_abbreviations.csv")
  upper_safe_col_names <- 
    column_names %>%
    gsub("#", "NUM", .) %>%
    gsub("%", "PCT", .) %>%
    make.names(., unique = TRUE) %>% 
    gsub("_", "__", .) %>%
    gsub("[.][.][.][.]+", "__", .) %>%
    gsub("[.]+", "_", .) %>%
    gsub("_$", "", .) %>%
    toupper(.)
  for (i in c(1:nrow(col_abbrevs))) {
    upper_safe_col_names <-
      gsub(col_abbrevs[i]$text,
           col_abbrevs[i]$abbreviation,
           upper_safe_col_names)
  }
  upper_safe_col_names
}

# Join data frames/tables
library(data.table)
leftJoin <- function(tbl_lt, tbl_rt, on_lt, on_rt) {
  dt_l <- setDT(tbl_lt)
  dt_r <- setDT(tbl_rt)
  setkeyv(dt_l, on_lt)
  setkeyv(dt_r, on_rt)
  dt_r[dt_l]
}


#' Get unique combinations of a column (col_a) and the values from a vector of column indexes (cols_b)
#'
#' @param data data.frame/data.table.
#' @param col_a A column index/name.
#' @param cols_b A vector of column indexes/names.
#' @return data.table.
#' @examples
#' result <- collapse_concatenate_group(va_blockgroups_spdf@data, "COUNTYFP", grep("^POP" , names(va_blockgroups_spdf)))
collapse_concatenate_group <- function(data, col_a, cols_b) {
  res_dt <- data.table(col = character(), col_val = character())
  data_dt <- as.data.table(data)
  col_name <- colnames(data_dt[, ..col_a])
  
  for (c in colnames(data_dt[, ..cols_b])) {
    new_dt <-
      data_dt[grep("[[:alnum:]]", data_dt[, get(c)]), .(c, get(col_name))]
    res_dt <- rbindlist(list(res_dt, new_dt))
  }
  res_dt <- unique(res_dt)
  
  res_dt <-
    res_dt[, .(col_val = paste(res_dt[, col_val], collapse = ", ")), by = col]
  res_dt <- res_dt[, , by = col_val]
  res_dt
}

#' Generate abbreviated column names from census column descriptions
#'
#' @param census_file_w_descriptions CENSUS ACS or DEC with column descriptions data.frame/data.table.
#' @return data.table.
#' @examples
#' result <- census_column_rename(DEC_10)
library(magrittr)
census_column_rename <- function(census_file_w_descriptions) {
  census_abbrevs <- data.table::fread("settings/census_column_abbreviations.csv")
  upper_safe_names <-
    data.table::as.data.table(census_file_w_descriptions) %>% .[order(-GEO.id),] %>%
    .[1,] %>% make.names(unique = TRUE) %>%
    gsub("[.][.][.][.]+", "__", .) %>% gsub("[.]+", "_", .) %>%
    gsub("_$", "", .) %>% toupper()
  for (i in c(1:nrow(census_abbrevs))) {
    upper_safe_names <-
      gsub(census_abbrevs[i]$text,
           census_abbrevs[i]$abbreviation,
           upper_safe_names)
  }
  upper_safe_names
}

