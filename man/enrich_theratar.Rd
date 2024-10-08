% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrich.r
\name{enrich_theratar}
\alias{enrich_theratar}
\title{Therapeutic target database includes protein/gene targets, their associated developed drugs, and phase of testing of drug. The user can query this database using enrich_theratarsuface.
The input to this function is the expression data-set, the diseases of interested, the mode of action for the drug.
The output is an added column to the expression data-set that includes scores for each gene/protein that is found in the database. The score is associated to the phase of development of the drug.}
\usage{
enrich_theratar(
  d,
  diseases = "\%",
  moa = NULL,
  mergefunc = max,
  onlysurface = F,
  immunomult = 1,
  immunoadd = 0
)
}
\description{
Therapeutic target database includes protein/gene targets, their associated developed drugs, and phase of testing of drug. The user can query this database using enrich_theratarsuface.
The input to this function is the expression data-set, the diseases of interested, the mode of action for the drug.
The output is an added column to the expression data-set that includes scores for each gene/protein that is found in the database. The score is associated to the phase of development of the drug.
}
