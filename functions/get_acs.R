library(tidyverse)
library(tidycensus)
library(data.table)

census_api_key("636a0df8dabd6c37220aeca7f1da41bbe1c5b30e")

get_acs_block_groups <- function(year = "2015", state = "VA",
    county = "001", variables = "B01001_026", output = "tidy") {
    data.table::setDT(get_acs(geography = "block group", year = year,
        state = state, county = county, variables = variables, output = output))
}

get_acs_block_groups_multiple_counties <- function(year = "2015",
                                                   state = "VA", counties_fips = c("001", "003"), variables = "B01001_026", output = "tidy",
                                                   category = "Demographics", sub_category = "Population", display_name = "Population by Sex",
                                                   by = "Sex", by_label = "Female", measure = "Number", data_source = "U.S. Census Bureau, American Community Survey (ACS) 5 year estimates",
                                                   data_source_abrev = "CENSUS:ACS5", description = "", definitions = "") {
    for (c in counties_fips) {
        print(c)
        result <- get_acs_block_groups(year = year, state = state, county = c, variables = variables, output = output)
        if (!exists("acs_bg")) {
            acs_bg <- result
        } else {
            acs_bg <- data.table::rbindlist(list(acs_bg, result))
        }
    }

  acs_bg[, c("NAMELSAD", "YEAR", "CATEGORY", "SUB_CATEGORY",
                  "DISPLAY_NAME", "BY", "BY_LABEL", "MEASURE", "DESCRIPTION",
                  "DEFINITIONS", "DATA_SOURCE", "DATA_SOURCE_ABREV", "ST_SUBST_FIPS",
                  "CONTAINING_GEOID", "VALUE", "MOE") := list(NAME, year,
                                                            category, sub_category, display_name, by, by_label, measure,
                                                            description, definitions, data_source, data_source_abrev,
                                                            GEOID, substr(GEOID, 1, 5), estimate, moe)]

  acs_bg <- acs_bg[, .(NAMELSAD, YEAR, CATEGORY, SUB_CATEGORY,
                               DISPLAY_NAME, BY, BY_LABEL, MEASURE, DESCRIPTION, DEFINITIONS,
                               DATA_SOURCE, DATA_SOURCE_ABREV, ST_SUBST_FIPS, CONTAINING_GEOID,
                               VALUE, MOE)]
  acs_bg
}

get_acs_counties <- function(year = "2015", state = "VA", variables = "B01001_026") {
    data.table::setDT(get_acs(geography = "county", year = year,
        state = state, variables = variables))
}

counties_fips <- function() {
    c("001", "003", "005", "007", "009", "011", "013", "015",
        "017", "019", "021", "023", "025", "027", "029", "031",
        "033", "035", "036", "037", "041", "043", "045", "047",
        "049", "051", "053", "057", "059", "061", "063", "065",
        "067", "069", "071", "073", "075", "077", "079", "081",
        "083", "085", "087", "089", "091", "093", "095", "097",
        "099", "101", "103", "105", "107", "109", "111", "113",
        "115", "117", "119", "121", "125", "127", "131", "133",
        "135", "137", "139", "141", "143", "145", "147", "149",
        "153", "155", "157", "159", "161", "163", "165", "167",
        "169", "171", "173", "175", "177", "179", "181", "183",
        "185", "187", "191", "193", "195", "197", "199", "510",
        "520", "530", "540", "550", "570", "580", "590", "595",
        "600", "610", "620", "630", "640", "650", "660", "670",
        "678", "680", "683", "685", "690", "700", "710", "720",
        "730", "735", "740", "750", "760", "770", "775", "790",
        "800", "810", "820", "830", "840")
}
