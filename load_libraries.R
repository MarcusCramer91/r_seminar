install.packages(c("BBmisc", "stringr")) #for bbob
install.packages("Rtools")
fn = file.path(tempdir(), "bbob_current.tar.gz")

URL = "http://coco.lri.fr/downloads/download15.02/bbobr.tar.gz"

download.file(URL, destfile=fn)
#ATTENTION: requires Rtools for windows
install.packages(fn, repos=NULL)
file.remove(fn)

library("bbob")

?bbo_benchmark

#load CMA-ES based on R
fn = file.path(tempdir(), "CMA-ES-R.tar.gz")
URL = "https://cran.r-project.org/src/contrib/cmaes_1.0-11.tar.gz"
download.file(URL, destfile = fn)
install.packages(fn, repos = NULL)
file.remove(fn)


#load CMA-ES based on Java
fn = file.path(tempdir(), "CMA-ES-Java.tar.gz")
URL = "https://cran.r-project.org/src/contrib/rCMA_1.1.tar.gz"
download.file(URL, destfile = fn)
install.packages(fn, repos = NULL)
file.remove(fn)
