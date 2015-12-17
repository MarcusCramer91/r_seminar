loadRelevantLibraries = function () {
  if (!"BBmisc" %in% rownames(installed.packages())) install.packages("BBmisc")
  if (!"stringr" %in% rownames(installed.packages())) install.packages("stringr")
  if (!"smoof" %in% rownames(installed.packages())) install.packages("smoof")
  if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
  require(BBmisc)
  require(stringr)
  require(smoof)
  require(devtools)
  
  
  #get BBOB
  if (!"bbob" %in% rownames(installed.packages())) {
    fn = file.path(tempdir(), "bbob_current.tar.gz")
    URL = "http://coco.lri.fr/downloads/download15.03/bbobr.tar.gz"
    download.file(URL, destfile=fn)
    #ATTENTION: requires Rtools for windows
    install.packages(fn, repos=NULL)
    tryCatch(install.packages(fn, repost = NULL), 
      stop(simpleError("automatic installation done according to the bbob documentation. However, it sometimes does not work. 
             Manual download of just the R files and then manual installation via install.packages(filepath) does the trick"))) 
    
    #above is from the official documentation but does not work for me, this does
    #after manually downloading just the r file
    #install.packages("bbobrpackage.15.03.tar.gz", repos = NULL)
    file.remove(fn)
  }
  require(bbob)
  
  
  #load CMA-ES based on R
  if (!"cmaes" %in% rownames(installed.packages())) {
    fn = file.path(tempdir(), "CMA-ES-R.tar.gz")
    URL = "https://cran.r-project.org/src/contrib/cmaes_1.0-11.tar.gz"
    download.file(URL, destfile = fn)
    install.packages(fn, repos = NULL)
    file.remove(fn)
  }
  require(cmaes)
  
  
  #load CMA-ES based on Java
  if (!"rCMA" %in% rownames(installed.packages())) {
    fn = file.path(tempdir(), "CMA-ES-Java.tar.gz")
    URL = "https://cran.r-project.org/src/contrib/rCMA_1.1.tar.gz"
    download.file(URL, destfile = fn)
    install.packages(fn, repos = NULL)
    file.remove(fn)
  }
  require(rCMA)
  
  #get Bossek's CMA-ES
  if (!"cmaesr" %in% rownames(installed.packages())) {
    install_github(repo = "jakobbossek/cmaesr")
  }
  require(cmaesr)
}
