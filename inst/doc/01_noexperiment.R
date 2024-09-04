## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Create a project configuration:
proj = list(
	dataset='__ALLHUMANSURFACEGENES__'  # The starting point is all human surface genes
	,enrich=list(
		# Specify the databases that the dataset should be enriched with:
		enrichtypes='gtex,evodevo_pediatric,healthyprot,depmap,compartments_sp,cirfess_spc'
		# Some enrichment databases require additional inputs; e.g., Depmap database requires a disease name:
		,depmapids='Rhabdomyosarcoma')
);

# Perform dataset enrichment and prioritization analysis:
proj = immunotar::project_run(proj)

# Print the top ranking genes:
head(proj$datawithscore[,'score',drop=F])

