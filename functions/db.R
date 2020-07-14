get_con <- function(db,
                    db_pass_file = sprintf("/home/%s/.dbpass", Sys.getenv("LOGNAME")),
                    db_user = Sys.getenv("LOGNAME")) {
    con <- sdalr::con_db(dbname = db,
                         user = db_user,
                         pass = readr::read_lines(db_pass_file))
}


