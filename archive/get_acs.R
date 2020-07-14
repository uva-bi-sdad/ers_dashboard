# get block groups for multiple counties with multiple associated value columns
library("acs")

get_acs_mult_cnty_mult_tbl_vars <-
  function(county_fips_list,
           census_table_nums_list) {
    dt_list <- list()
    for (cf in county_fips_list) {
      dt <- get_acs_cnty_mult_tbl_vars(cf, census_table_nums_list)
      dt_list <- c(dt_list, list(dt))
    }
    combined <- data.table::rbindlist(dt_list)
    combined
  }

get_acs_cnty_mult_tbl_vars <-
  function(county_fips, census_tbl_num_list) {
    l <-
      mapply(get_acs_cnty_tbl_vars, cnty = county_fips, tbl_num = census_tbl_num_list)
    Reduce(merge, l)
  }

get_acs_cnty_tbl_vars <-
  function(cnty = 13,
           tbl_num = "B01003",
           st = "VA") {
    my_vars <-
      acs::acs.lookup(
        endyear = 2015,
        table.number = tbl_num,
        case.sensitive = FALSE
      )
    my_geo <-
      acs::geo.make(
        state = st,
        county = cnty,
        tract = "*",
        block.group = "*"
      )
    my_data <-
      acs::acs.fetch(
        endyear = 2015,
        geo = my_geo,
        variable = my_vars,
        col.names = "pretty"
      )
    my_dt <-
      data.table::setDT(data.frame(
        geography(my_data),
        estimate(my_data),
        1.645 * standard.error(my_data)
      ))
    colnames(my_dt) <-
      sub(pattern = "1$",
          replacement = "MOE90",
          x = colnames(my_dt))
    my_dt
  }
