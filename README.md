# Immunotar

(Note: Do not change this file on GitHub.com; instead modify src.r/immunotar.README.md file and use immunotar_makepackage to push your changes)

## Introduction

General introduction. Motivation, goals, features.

Include image(s) -- workflow? example results?


## Installation

If you are new to the R programming language, install [R and Rstudio](https://posit.co/download/rstudio-desktop/). You will then need to type the commands below in RStudio Console.

```r
# Devtools will allow us to download/install packages from GitHub.
install.packages('devtools');

# Install immunotar
devtools:install_github('sacanlab/immunotar')
```


## Example Usage

### View the vignettes available in the immunotar package
```r
browseVignettes('immunotar)
```

### Prioritize genes for a cancer type without providing additional experimental data
```r
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
head(p$datawithscore[,'score',drop=F])
```


### Prioritize genes for a cancer type without providing additional experimental data
```r
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
head(p$datawithscore[,'score',drop=F])
```


### Prioritize genes with input experimental data
This example uses one of the demo experimental data files included in immunotar, (demo_experiment.xlsx)[inst/data/demo_experiment.xlsx]. You may provide your own data file.

```r
proj = list( dataset = 'demo_experiment.xlsx' )

proj = immunotar::project_run( proj )

head(p$datawithscore[,'score',drop=F])
```


### Use a YAML file for project configuration
An immunotar analysis project can have many configurable options. It is convenient to store the project configuration in a YAML file. The example below uses (demo_project.yml)[inst/data/demo_project.yml] that is included in the immunotar package. See that file for an explanation of the configurable options.

```r
proj = immunotar::project_run( 'demo_project.yml' )

head(p$datawithscore[,'score',drop=F])
```