#' Adds an roxygen2 comment block
#'
#' Adds an roxygen2 comment block
#' @import rstudioapi
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
  comment = grep(
    "^#'\\s?" # line starts with "#'", has no or one trailing whitespace
    , line
    , value = TRUE
  )

  is_roxy2 = length(comment > 0)
  if(is_roxy2) {
    rng = Map(
      c
      , Map(c, r, 1)
      , Map(c, r, ifelse(grepl("\\s", comment), 4, 3)) # if applicable, strip trailing whitespace
    )
    modifyRange(rng, "")
  } else {
    pos = Map(c, r_start:r_end, 1)
    insertText(pos, "#' ")
  }
}






