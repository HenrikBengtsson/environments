draw_search()


fcn <- local({
  a <- 2
  function() {
    pi * a
  }
})

message("fcn():")
print(fcn)


message("All parent environments of fcn():")
envs <- parent_envs(environment(fcn))
str(envs)

message("Free variables of fcn():")
globals <- globals::findGlobals(fcn)
message(sprintf("- variables: %s", paste(sQuote(globals), collapse = ", ")))

for (name in globals) {
  envir <- find_variable(name, from = environment(fcn))
  where <- if (is.null(envir)) "<not found>" else environment_name(envir)
  message(sprintf("- Location of free variable '%s': %s", name, where))
}

str(globals::globalsOf(fcn))
