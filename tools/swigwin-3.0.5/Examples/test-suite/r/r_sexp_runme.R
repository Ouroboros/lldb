source("unittest.R")
dyn.load(paste("r_sexp", .Platform$dynlib.ext, sep=""))
source("r_sexp.R")
cacheMetaData(1)

obj <- return_sexp(1);
unittest(obj, 1)
