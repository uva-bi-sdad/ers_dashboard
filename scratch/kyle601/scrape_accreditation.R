### A collection of failed attempts at scraping accreditation scores for all schools
library(XML)
url1 <- "http://schoolquality.virginia.gov/schools/a-henderson-elementary#fndtn-desktopTabs-accountability"
table1 <- XML::readHTMLTable(url1, header = TRUE, which = 1)
### ^^ This is reading in a table that gives more than 1 thing attached to entries in the df.
### Example:
table1[3,3]

df <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)
accreditation_df <- data.frame(School=character(),eng_1yr_14_15=integer(),
                               eng_3yr_14_15=integer(),eng_1yr_15_16=integer(),
                               eng_3yr_15_16=integer(),eng_1yr_16_17=integer(),
                               eng_3yr_16_17=integer(),eng_met_bench=factor())

for (i in 1:10) {
  School <- school_list_final$School[i]
  url_1 <- paste0("http://schoolquality.virginia.gov/schools/", 
                  school_list_final$School[i], "#fndtn-desktopTabs-accountability")
  table_1 <- XML::readHTMLTable(url_1, header = TRUE, which = 1)
  eng_1yr_14_15 <- as.numeric(table_1[3,3])
  eng_3yr_14_15 <- table_1[3,4]
  eng_1yr_15_16 <- table_1[3,5]
  eng_3yr_15_16 <- table_1[3,6]
  eng_1yr_16_17 <- table_1[3,7]
  eng_3yr_16_17 <- table_1[3,8]
  eng_met_bench <- table_1[3,9]
  school_vector <- c(Schoo)
  print(i)
}
accreditation_df
for (i in 1:10) {
  accreditation_df$School <- school_list_final$School[i]
  url_1 <- paste0("http://schoolquality.virginia.gov/schools/", 
                  school_list_final$School[i], "#fndtn-desktopTabs-accountability")
  table_1 <- XML::readHTMLTable(url_1, header = TRUE, which = 1)
  accreditation_df$eng_1yr_14_15 <- table_1[3,3]
  accreditation_df$eng_3yr_14_15 <- table_1[3,4]
  accreditation_df$eng_1yr_15_16 <- table_1[3,5]
  accreditation_df$eng_3yr_15_16 <- table_1[3,6]
  accreditation_df$eng_1yr_16_17 <- table_1[3,7]
  accreditation_df$eng_3yr_16_17 <- table_1[3,8]
  accreditation_df$eng_met_bench <- table_1[3,9]
  print(i)
}













