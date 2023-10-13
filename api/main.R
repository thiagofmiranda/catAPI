library(plumber)
r <- plumb("api.R")
r$run(port=1994, host="0.0.0.0",swagger = F)