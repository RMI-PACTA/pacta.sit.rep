# check if all elements of x are in y, recursively
list_is_subset <- function(x, y) {
  if (is.list(x) && is.list(y)) {
    all(vapply(x, function(x_elem) {
      any(vapply(y, function(y_elem) {
        list_is_subset(x_elem, y_elem)
      }, logical(1L)))
    }, logical(1L)))
  } else {
    identical(x, y)
  }
}
