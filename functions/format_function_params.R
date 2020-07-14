format_function_params <- function(txt, function_name = "") {

  v <- strsplit(txt, split = "")[[1]]

  start_pos = 1

  if (function_name != "") start_pos = regexpr(function_name, txt)[[1]]

  pdpt <- plst <- pfst <- pcnt <- 0

  for (i in 1:length(v)) {
    if (v[i] == "(") {
      pcnt = pcnt + 1
      pdpt = pdpt + 1
      if (pcnt == 1) pfst = i
    }
    if (v[i] == ")") {
      pdpt = pdpt - 1
      if (pdpt == 0) plst = i
    }
  }

  btwn <- substr(txt, pfst + 1, plst - 1)

  args <- gregexpr('([a-zA-Z0-9]*\\([a-zA-Z _$",0-9]*\\))|\\"[a-zA-Z _$%",0-9]*\\"', btwn)[[1]]
  args_v <- list()
  for (i in 1:length(args)) {
    arg <- list(args[[i]], attr(args, "match.length")[[i]])
    args_v <- rbindlist(list(args_v, arg))
  }
  args_out <- ""
  for (i in 1:nrow(args_v)) {
    start <- args_v[i][[1]]
    length <- args_v[i][[2]]
    delim <- ",\n"
    if (i == nrow(args_v)) delim = ""
    args_out <- paste0(args_out, substr(btwn, start, start + length - 1), delim)
  }
  cat(args_out)
}
