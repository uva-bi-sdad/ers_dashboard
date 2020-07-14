words <- "table: RWJF CNTY_HLTH_RANK 2014 - column: SOME_COLLEGE_POST_SECONDARY_EDUCATION___NUM_SOME_COLLEGE"
# words %>%
# gsub("!", " ", .) %>% gsub("  ", " ", .) %>%
#   substr(., regexpr(": [A-Za-z_ ]*$", .) + 2, nchar(.))

legend_title <- words %>%
  gsub("!", " ", .) %>%
  gsub("  ", " ", .) %>%
  substr(., regexpr("olumn: ", .) + 7, nchar(.)) %>%
  gsub("__+", ": ", .) %>%
  gsub("_", " ", .)

legend_title <- paste(strwrap(legend_title,16), collapse="<br />") %>%
  htmltools::HTML()

legend_title





names(ChickWeight) <- tolower(names(ChickWeight))
DT <- melt(as.data.table(ChickWeight), id=2:4) # calls melt.data.table
cw <- ChickWeight

dcast(DT, time ~ variable, fun=mean)

db_pass_file <- sprintf("/home/%s/.dbpass", Sys.getenv("LOGNAME"))
con <- sdalr::con_db(dbname = "dashboard", user = "aschroed", pass=readr::read_lines(db_pass_file))

sql <- gsub("[\r\n]", "", "
SELECT \"COMMODITY_DESC\", \"STATE_ANSI\", \"COUNTY_ANSI\", \"COUNTY_NAME\", \"ASD_DESC\", \"VALUE\", \"ST_SUBST_FIPS\", \"CONTAINING_GEOID\", \"UNIT_DESC\" \"MEASURE\"
FROM \"dt_nass_qs_crops_20170825\"
WHERE \"STATE_FIPS_CODE\" = '51'
AND \"YEAR\" = '2014'
AND \"STATISTICCAT_DESC\" = 'PRODUCTION'
AND \"COUNTY_ANSI\" IS NOT NULL
 ORDER BY \"COMMODITY_DESC\"
")

nass <- DBI::dbGetQuery(con, sql)
data.table::setDT(nass)

nass_sub <- nass[VALUE != "(D)",.(CONTAINING_GEOID, ST_SUBST_FIPS, COUNTY_NAME, COMMODITY_DESC, VALUE, MEASURE)]
head(nass_sub)
nass_sub[, VALUE := as.numeric(gsub(",", "", nass_sub$VALUE), na.rm = TRUE)]


nass_cst <- data.table::dcast(nass_sub, CONTAINING_GEOID + ST_SUBST_FIPS + COUNTY_NAME ~ paste0(COMMODITY_DESC, "(", MEASURE, ")"), value.var = "VALUE", fun.aggregate = mean)
nass_cst[is.na(nass_cst)] <- 0
