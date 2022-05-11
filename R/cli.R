#' @rdname draw_search
#' @importFrom cli boxx
#' @export
boxx_search <- function(labels = search(), width = NULL) {
  if (is.null(width)) width <- max(nchar(labels)) + 4L

  boxes <- lapply(labels, FUN = function(label) {
    pad <- width - 2L - nchar(label)
    pad_left <- pad %/% 2
    pad_right <- pad - pad_left
    cli::boxx(label, padding = c(0L, pad_left, 0L, pad_right), align = "center")
  })
  attr(boxes, "width") <- width
  boxes
}


#' Draw the current search() path in the terminal
#'
#' @param labels Character vector
#'
#' @param width The common width of the boxes in number of characters.
#' If NULL, the width of the widest box will be used.
#'
#' @return Nothing.
#'
#' @examples
#' draw_search()
#'
#' @export
draw_search <- function(labels = rev(search()), width = NULL) {
  boxes <- boxx_search(labels = labels, width = width)
  width <- attr(boxes, "width")
  
  vline <- substr(strsplit(boxes[[1]], split = "\n", fixed = TRUE)[[1]][[2]], 1L, 1L)
  pad <- width - 1L
  pad_left <- pad %/% 2
  pad_right <- pad - pad_left
  arrow <- sprintf("%*s%s%*s\n", pad_left, "", c("^", vline), pad_right, "")
  arrow <- paste(arrow, collapse = "")
  
  for (kk in seq_along(boxes)) {
    box <- boxes[[kk]]
    print(box)
    if (kk < length(boxes)) cat(arrow)
  }
}
