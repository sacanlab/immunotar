# ImmunoTar


## Introduction

General introduction: Motivation, goals, and features.

To address the challenge of identifying and systematically prioritizing immunotherapeutic targets computationally, we developed ImmunoTar. This tool integrates multiple publicly available databases, analyzes cancer expression datasets, and quantitively prioritizes potential targets based on predefined ideal immunotherapeutic target criteria. In addition to adult derived databases, ImmunoTar is equipped with pediatric specific databases aiming to aid in identifying pediatric-specific antigens, providing a resource for developing novel therapies in pediatric oncology. 


The public databases queried within ImmunoTar are divided into four categories: normal tissue expression, protein localization, biological annotation, and reagent/therapeutic availability. 

1. Normal tissue experession - GTEx (https://gtexportal.org/home/), Evo-Devo (https://apps.kaessmannlab.org/evodevoapp/), Healthy proteomics map (PMID: 32916130)

2. Protein localization - COMPARTMENTS (https://compartments.jensenlab.org/Search), CIRFESS (https://www.cellsurfer.net/cirfess), UNIPROT (https://www.uniprot.org/)

3. Biological annotation - Gene ontology (GO) (https://geneontology.org/), DepMap (https://depmap.org/portal/)

4. Reagent/therapeutic availability - Therapeutic Target Database (TTD) (https://db.idrblab.net/ttd/), The Database of Antibody-drug Conjugates (ADC-db) (http://adcdb.idrblab.net/), th Pediatric Molecular Targets List (PMTL) (https://moleculartargets.ccdi.cancer.gov/fda-pmtl)


The tool extracts quantitative features from each of these databases to evaluate measured gene candidates against the ideal immunotherapeutic target criteria.


## Installation

If you are new to the R programming language, install [R and Rstudio](https://posit.co/download/rstudio-desktop/). You will then need to type the commands below in RStudio Console.

```r
# Devtools will allow us to download/install packages from GitHub.
install.packages('devtools');

# Install ImmunoTar
devtools::install_github('sacanlab/immunotar')
```



## Example Usage

View the vignettes available in the immunotar package [immunotar](https://htmlpreview.github.io/?https://github.com/sacanlab/immunotar/blob/master/vignettes/immunotar.html).

```r
browseVignettes('immunotar')
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
head(proj$datawithscore[,'score',drop=F])

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
head(proj$datawithscore[,'score',drop=F])
```


### Prioritize genes with input experimental data
This example uses one of the demo experimental data files included in immunotar, [demo_experiment.xlsx](inst/data/demo_experiment.xlsx). You may provide your own data file.

```r
proj = list( dataset = 'demo_experiment.xlsx' )

proj = immunotar::project_run( proj )

head(proj$datawithscore[,'score',drop=F])
```


### Use a YAML file for project configuration
An immunotar analysis project can have many configurable options. It is convenient to store the project configuration in a YAML file. The example below uses [demo_project.yml](inst/data/demo_project.yml) that is included in the immunotar package. See that file for an explanation of the configurable options.

```r
proj = immunotar::project_run( 'demo_project.yml' )

head(proj$datawithscore[,'score',drop=F])
```

### Visualization Functions within ImmunoTar
A few functions within ImmunoTar allow the user to visualize results in heatmaps, rankplots and also allow the user to generate a GUI to manually adjust weights and curve feature values based on their expertise to identify how these feature weights impact the scoring of their known-positive targets. 

```r
proj = immunotar::project_run( 'demo_project.yml' )


```
