expandRows <- function(dataset, count, count.is.col = TRUE) {
  if (!isTRUE(count.is.col)) {
    if (length(count) == 1) {
      dataset[rep(rownames(dataset), each = count), ]
    } else {
      if (length(count) != nrow(dataset)) {
        stop("Expand vector does not match number of rows in data.frame")
      }
      dataset[rep(rownames(dataset), count), ]
    }
  } else {
    dataset[rep(rownames(dataset), dataset[[count]]), 
            setdiff(names(dataset), names(dataset[count]))]
  }
}