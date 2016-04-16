#' Adds an roxygen2 comment block
#'
#' Adds an roxygen2 comment block
#' @export
roxygen2_block = function() {
  location = rstudioapi::getActiveDocumentContext()
  ## Get the row number

  r_start = location$selection[[1]]$range$start[1]
  r_end = location$selection[[1]]$range$end[1]
  r = r_start:r_end

  line = location$contents[r_start]

  # If roxygen2 block remove #'
  # If not remove, add #'
  is_roxy2 = substr(line, 1L, 2L) == "#'"
  if(is_roxy2) {
    rng = Map(c, Map(c, r, 1), Map(c, r, 3))
    modifyRange(rng, "")
  } else {
    pos = Map(c, r_start:r_end, 1)
    insertText(pos, "#' ")
  }
}






