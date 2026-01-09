#' @title Coerce objects to class `reeb_graph`
#'
#' @description Coerce objects to [reeb_graph]-class objects.
#'
#' @details The `as_reeb_graph()` methods require a network (mathematical graph)
#'   structure and a real-valued function on the vertex set.
#'
#' @param x An R object to be coerced. See Details.
#' @param values A character value or a numeric vector; the node attribute to
#'   use as the Reeb graph value function, or the values in order of node
#'   indices. If `NULL` (the default), the first numeric node attribute is used.
#' @param ... Additional arguments passed to methods.
#' @returns A [reeb_graph] object.
#' @examples
#' library(igraph)
#' ( g <- make_kautz_graph(2, 1) )
#' ( l <- layout_nicely(g) )
#' plot(g, layout = l)
#' ( rg <- as_reeb_graph(g, l[, 1]) )
#' # TODO: Coerce back to 'igraph' class and plot using Sugiyama layout.
#'
#' @export
as_reeb_graph <- function(x, ...) UseMethod("as_reeb_graph")

#' @rdname reeb_graph
#' @export
as_reeb_graph.igraph <- function(x, values = NULL, ...) {
  if (is.null(values)) {
    x_attr <- vertex_attr_names(x)
    x_attr <-
      which(vapply(x_attr, function(nm) is.numeric(vertex_attr(x, nm)), FALSE))
    if (length(x_attr) == 0L)
      stop("Input 'igraph' object has no numeric vertex attributes.")
    values <- vertex_attr(x, names(x_attr))
  } else if (is.character(values)) {
    values <- vertex_attr(x, values)
  }

  reeb_graph(values, as_edgelist(x) - 1L)
}
