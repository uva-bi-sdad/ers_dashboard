# project_root <- dirname(sys.frame(1)$ofile)

setwd(normalizePath("."))


# thisFile <- function() {
#   cmdArgs <- commandArgs(trailingOnly = FALSE)
#   needle <- "--file="
#   match <- grep(needle, cmdArgs)
#   if (length(match) > 0) {
#     # Rscript
#     return(normalizePath(sub(needle, "", cmdArgs[match])))
#   } else {
#     # 'source'd via R console
#     return(normalizePath(sys.frames()[[1]]$ofile))
#   }
# }
# 
# script.name <- basename(strsplit(commandArgs(trailingOnly = FALSE)[4],"=")[[1]][2])
