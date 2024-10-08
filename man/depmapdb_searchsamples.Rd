% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/depmapdb.r
\name{depmapdb_searchsamples}
\alias{depmapdb_searchsamples}
\title{depmapdb_searchsamples function allows users to search the DepMap database to see what cancer phenotypes are included and the depmapid's of those phenotypes.
The input to this function includes the disease names of interest
The output is a data-frame of meta data from DepMap that includes the depmapid's, the primary disease, the cancer subtype, and the cell line that was tested in DepMap}
\usage{
depmapdb_searchsamples(
  queries,
  searchinfields = c("samplecollectionsite", "primarydisease", "subtype", "lineage",
    "lineagesubtype", "cellosaurusncitdisease"),
  exact = FALSE,
  where = "",
  getfields = NULL
)
}
\description{
depmapdb_searchsamples function allows users to search the DepMap database to see what cancer phenotypes are included and the depmapid's of those phenotypes.
The input to this function includes the disease names of interest
The output is a data-frame of meta data from DepMap that includes the depmapid's, the primary disease, the cancer subtype, and the cell line that was tested in DepMap
}
