library(acs)

api.key.install("636a0df8dabd6c37220aeca7f1da41bbe1c5b30e")

my_vars<-acs.lookup(endyear=2015, table.number = "B01003", case.sensitive = FALSE)

my_geo<-geo.make(state="VA", county="Arlington", check = TRUE)
my_geo<-geo.make(state="VA",county=c("Arlington"), tract="*", block.group="*", check = TRUE)

my_bg_pop<-acs.fetch(endyear=2015, geo=my_geo, variable = my_vars, key = "home/aschroed/key.rda")


#  001,003,005,007,009,011,013,015,017,019,021,023,025,027,029,031,033,035,036
# ,037,041,043,045,047,049,051,053,057,059,061,063,065,067,069,071,073,075,077
# ,079,081,083,085,087,089,091,093,095,097,099,101,103,105,107,109,111,113,115
# ,117,119,121,125,127,131,133,135,137,139,141,143,145,147,149,153,155,157,159
# ,161,163,165,167,169,171,173,175,177,179,181,183,185,187,191,193,195,197,199
# ,510,520,530,540,550,570,580,590,595,600,610,620,630,640,650,660,670,678,680
# ,683,685,690,700,710,720,730,735,740,750,760,770,775,790,800,810,820,830,840


gg<-county_health_col_titles$X1 %>% gsub("#", "NUM", .) %>% gsub("%", "PCT", .) %>% make.names(., unique = TRUE) %>% gsub("_", "__", .) %>% gsub("[.][.][.][.]+", "__", .) %>% gsub("[.]+", "_", .) %>%gsub("_$", "", .) %>% toupper(.)