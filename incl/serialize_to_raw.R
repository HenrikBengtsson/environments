object_to_raw <- function(obj) {
  con <- rawConnection(raw(), open = "w")
  on.exit(close(con))
  suppressWarnings(serialize(obj, connection = con))
  rawConnectionValue(con)
}

raw_to_object <- function(raw) {
  con <- rawConnection(raw, open = "r")
  on.exit(close(con))
  suppressWarnings(unserialize(con))
}
