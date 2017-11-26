column_names_as_string <- function(column.names,
                                  library.column,
                                  target.column,
                                  surrogate.column) {

  # Refer to library and target columns by name, not index.
  if (is.numeric(library.column) &
      is.numeric(target.column) &
      is.numeric(surrogate.column)) {

    library.column <- column.names[library.column]
    target.column <- column.names[target.column]
    surrogate.column <- column.names[surrogate.column]

  } else {
    library.column <- ifelse(test = is.numeric(library.column),
                            yes = column.names[library.column],
                            no = library.column)

    target.column <- ifelse(test = is.numeric(target.column),
                           yes = column.names[target.column],
                           no = target.column)

    surrogate.column <- ifelse(test = is.numeric(surrogate.column),
                              yes = column.names[surrogate.column],
                              no = surrogate.column)
  }

  return(c("library.column" = library.column,
           "target.column" = target.column,
           "surrogate.column" = surrogate.column))
}
