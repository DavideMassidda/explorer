.onLoad <-
function(libname, pkgname)
{
    where <- match(paste("package:", pkgname, sep = ""), search())
    ver <- read.dcf(file.path(libname, pkgname, "DESCRIPTION"), "Version")
    ver <- as.character(ver)
    packageStartupMessage(paste(paste("Package explorer",ver),"loaded"))
}
.packageName <-
"explorer"
