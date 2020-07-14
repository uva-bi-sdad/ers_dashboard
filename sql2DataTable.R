library(data.table)
attach(airquality)
summary(airquality)
aq <- as.data.table(airquality)

aq[Month==5]

aq[Month==5 & Ozone<18]

aq[Month!=5 & Ozone<18]

print(setorder(aq[Month!=5 & Ozone<18], Ozone))

print(setorder(aq[Month==5 & Day<=10, .(Wind, Temp)], Wind))

print(setorder(aq[Month==5 & Day<=10, .(Wind, Temp, TempC = (Temp - 32) * 5 / 9)], Wind))

aq[,.(mean_temp = mean(Temp)), Month]

aq[Month==5 & Day<=10, .(Wind, Temp, TempC = (Temp - 32) * 5 / 9)][order(Wind)]


sqldt <- function(sql = "select col_a, Col_b from dt where col_a < 7 and Col_b = 'c' group by Col_b") {
  SQL <- toupper(sql)
  select_pos <- regexpr("SELECT", SQL)
  from_pos <- regexpr("FROM", SQL)
  where_pos <- regexpr("WHERE", SQL)
  group_pos <- regexpr("GROUP", SQL)
  end_pos <- regexpr("$", SQL)
  
  select_cols <- trim(substr(sql, select_pos + attr(select_pos, "match.length"), from_pos - 1))
  from_dt <- trim(substr(sql, from_pos + attr(from_pos, "match.length"), if (where_pos!=-1) where_pos - 1 else end_pos))
  where_cond <- trim(substr(sql, where_pos + attr(where_pos, "match.length"), if (group_pos!=-1) group_pos - 1 else end_pos))
  where_cond <- gsub("and", "&", where_cond, ignore.case = TRUE)
  where_cond <- gsub("[^<>]=", " == ", where_cond)
  where_cond <- gsub(" +", " ", where_cond)
  
  sprintf("%s[%s, .(%s)]", from_dt, where_cond, select_cols)
}

sqldt("select col_a, Col_b from dt where col_a < 7 and Col_b = 'c' group by Col_b")
