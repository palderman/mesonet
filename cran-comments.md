## Test environments

* local Ubuntu 22.04.5 LTS, R 4.5.0, GCC
* win-builder (devel and release)
* mac-builder (release)

## R CMD check results

There was one NOTE:

 - Maintainer: ‘Phillip D. Alderman <phillip.alderman@okstate.edu>’

   New submission
   
   Possibly misspelled words in DESCRIPTION:
     MTS (13:36, 13:58)
     Mesonet (3:38, 12:16, 13:15)
     matric (14:61)
   Found the following (possibly) invalid DOIs:
     DOI: 10.32614/CRAN.package.mesonet
       From: inst/CITATION
       Status: 404
       Message: Not Found     

MTS is a standard abbreviation for Mesonet Time Series. Mesonet is a standard abbreviation of mesoscale network. The term "matric" is a standard term within soil physics.

The invalid DOI is due to the package not being on CRAN yet. Once the package is on CRAN the DOI will become valid.

There were no ERRORs or WARNINGs.

## Downstream dependencies

There are currently no downstream dependencies for this package.
