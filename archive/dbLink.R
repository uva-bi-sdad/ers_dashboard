library(RPostgreSQL) # load the database connection library
library(readxl)
library(stringr)

drv <- dbDriver("PostgreSQL") # create instance of database driver
all_cons <- dbListConnections(drv)
for(con in all_cons)
{dbDisconnect(con)}
con <- dbConnect(drv, 
                 dbname="aschroed", 
                 host="localhost", 
                 port=5432, 
                 user="aschroed", 
                 password="12Character$")

fixColNames <- function(df) {
  paste0("_", tolower(make.names(names(df)), unique=TRUE), "")
}
space(), "")
files <- list(
  "2010 County Health Ranking Virginia Data - v2.xls",
  "2011 County Health Ranking Virginia Data - v2.xls",
  "2012 County Health Ranking Virginia Data - v2.xls",
  "2013 County Health Ranking Virginia Data - v1_0.xls",
  "2014 County Health Rankings Virginia Data - v3.xls",
  "2015 County Health Rankings Virginia Data - v1_0.xls",
  "2016 County Health Rankings Virginia Data - v2.xls"
)

createTableNames <- lapply(files, function(x){ tolower(paste0("_", substr(x, 1, 26), "")) })

df1 <- read_excel("data/2010 County Health Ranking Virginia Data - v2.xls", sheet = "Measure Data", skip = 1)
names(df1) <- fixColNames(df1)
dbWriteTable(con,"county_health_ranking_2010",as.data.frame(df1))

df2 <- read_excel("data/2011 County Health Ranking Virginia Data - v2.xls", sheet = "Ranked Measure Data", skip = 1)
names(df2) <- fixColNames(df2)
dbWriteTable(con,"county_health_ranking_2011",as.data.frame(df2))

df3 <- read_excel("data/2012 County Health Ranking Virginia Data - v2.xls", sheet = "Ranked Measure Data", skip = 1)
names(df3) <- fixColNames(df3)
dbWriteTable(con,"county_health_ranking_2012",as.data.frame(df3))

df4 <- read_excel("data/2013 County Health Ranking Virginia Data - v1_0.xls", sheet = "Ranked Measure Data", skip = 1)
names(df4) <- fixColNames(df4)
dbWriteTable(con,"county_health_ranking_2013",as.data.frame(df4))

df5 <- read_excel("data/2016 County Health Rankings Virginia Data - v2.xls", sheet = "Ranked Measure Data", skip = 1)
names(df5) <- fixColNames(df5)
dbWriteTable(con,"county_health_ranking_2016",as.data.frame(df5))

