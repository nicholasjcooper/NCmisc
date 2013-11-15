dir <- "/home/ncooper/cransubmission/"

make.package <- function(pkg,dir,dscr=NULL,pkg.rd=NULL,int.rd=NULL) {
  require(NCmisc)
  cur <- getwd()
  setwd(dir)
  must.use.package("roxygen2")
  package.skeleton(name = pkg,code_files=paste(pkg,".R",sep=""),force=T)
  roxygenise(pkg,roxygen.dir=pkg,copy.package=FALSE,
             unlink.target=FALSE)
  dsc <- paste("DESCRIPTION",pkg,sep="_"); if(file.exists(dsc)) { dscr <- dsc }
  dsc <- paste(pkg,"internal.Rd",sep="-"); if(file.exists(dsc)) { int.rd <- dsc }
  dsc <- paste(pkg,"package.Rd",sep="-"); if(file.exists(dsc)) { pkg.rd <- dsc }
  if(!is.null(dscr)) {
    dscr.fn <- cat.path(pkg,"DESCRIPTION")
    unlink(dscr.fn)
    file.copy(dscr, dscr.fn, overwrite = T)
  }
  if(!is.null(pkg.rd)) {
    dest.fn <- cat.path(pkg,pref="man/",basename(pkg.rd))
    file.copy(pkg.rd, dest.fn, overwrite = T)
  }
  if(!is.null(int.rd)) {
    dest.fn <- cat.path(pkg,pref="man/",basename(int.rd))
    cont <- suppressWarnings(readLines(int.rd))
    which.lines <- grep("\\alias{",cont,fixed=T)
    if(length(which.lines)>0) {
      filz <- gsub("}","",gsub("\\alias{","",cont[which.lines],fixed=T),fixed=T)
      del.fn <- cat.path(pkg,pref="man/",filz,suf=".Rd",ext="")
      cat("deleting manual files for",length(del.fn),"internal functions...\n")
      #prv(del.fn)
      unlink(del.fn)
    }
    file.copy(int.rd, dest.fn, overwrite = T)
  }
  setwd(cur)
  return(del.fn)
}



## bit of a mess  - give it a  goo!!
# useful for package prep, but a pain with 'quotes'
rox.args <- function(txt,PRE=T,POST=T,author='Nicholas Cooper') {
  # must change (")s to (')
  tspl <- strsplit(txt,",",fixed=T)  
  tspl2 <- sapply(tspl,strsplit,split="=",fixed=T)
  pars <- sapply(tspl2,"[",1)
  pars <- rmv.spc(pars)
  pars <- paste("#' @param ",pars,"\n",sep="")
  pre <- paste("#' title\n#' \n#' description ...\n#' ...\n#' ...\n")
  post <- paste("#' @export\n#' @seealso ...\n#' @author",author,"\n#' @examples\n#' ...\n#' ...\n")
  if(PRE) { cat(pre) }
  cat(pars,sep="")
  if(POST) { cat(post) }
}


require(roxygen2)

to.del <- make.package("reader",dir)


#R CMD build NCmisc
#R CMD check NCmisc_1.0.tar.gz
#R CMD INSTALL NCmisc_1.1.tar.gz 



