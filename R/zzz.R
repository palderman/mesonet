.onAttach <- function(libname, pkgname) {

  startup_msg <- c(
    "",
    "                         *",
    "                      *     *",
    "                   *           *",
    "                *                 *",
    "             *      /         /#   | *",
    "          *        -|---------##   |    *",
    "       *            /    |    \\#   |       * ",
    "    *                    |         |          *",
    "    *                    #\\        |          *",
    "    *                   ## \\       |          *",
    "    *                  ###  \\     /|          *",
    "    *                 ####   \\   / |          *",
    "    *                #####====\\ /  |          *",
    "    *                #####    |/   |          *",
    "    *      ####      #####    |   /           *",
    "    *      ####      #####    |  /            *",
    "    *      ####      #####    | /             *",
    "       *   ####      #####    |/           *",
    "          * |========#####====|         *",
    "             *       #####    |      *",
    "                *    #####    |   *",
    "                   * #####     *",
    "                      *###  *",
    "                         * ",
    paste0("This is version ", utils::packageVersion(pkgname),
           " of ", pkgname),
    "",
    "Contact the package maintainer (Phillip Alderman <phillip.alderman@okstate.edu>) for any queries regarding the R package.",
    "",
    "All Oklahoma Mesonet data are copyrighted and have quality assurance pre-applied. With the exception of media users (see https://mesonet.org/about/data-access-and-pricing), no redistribution of the data is allowed. Penalty is termination of access to data and possible legal action to recover lost revenue.",
    "",
    "Further details about data access and pricing are available at: https://mesonet.org/about/data-access-and-pricing",
    "",
    "Use of this R package or Oklahoma Mesonet data should be cited in publications following the guidlines provided by typing 'citation(\"mesonet\")'.") |>
    paste0("\n")

  packageStartupMessage(startup_msg, appendLF = TRUE)
}
