#' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    # Runs when attached to search() path such as by library() or require()
    if (interactive()) {


        pdesc <- packageDescription(pkgname)
        packageStartupMessage('')
        packageStartupMessage(pdesc$Package, " ", pdesc$Version, " by Vinnent Guyader")
        packageStartupMessage("->  For help type help('ztype')")
        packageStartupMessage('')
            }
}

# enleve les faux positifs du check
globalVariables(c(".")) # faudra mettre les autres pour que le check ne s'enflamme pas trop a cause des NSE

