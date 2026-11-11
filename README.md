
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ntriples

<!-- badges: start -->

[![R-CMD-check](https://github.com/dataobservatory-eu/ntriples/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dataobservatory-eu/ntriples/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/ntriples/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/ntriples)
<!-- badges: end -->

The **Resource Description Framework (RDF)** is a W3C standard for
describing resources and their relationships. It makes metadata about
datasets, visualisations, and other outputs created in R understandable
outside the R ecosystem. **N-Triples** is a simple, line-based RDF
format: each statement identifies a subject, predicate, and object. The
`ntriples` package provides lightweight tools for creating these
interoperable statements without a full RDF library.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("dataobservatory-eu/ntriples")
```

## Example: Growth of orange trees

The built-in `Orange` dataset records orange-tree growth. We describe
the published dataset [*Draper–Smith (1998): Growth of Orange
Trees*](https://doi.org/10.5281/zenodo.14917851), including its title
and creators, as RDF metadata. The observations themselves remain in R
(or can be exported separately as CSV).

``` r
library(ntriples)
data("Orange", package = "datasets")
head(Orange)
#>   Tree  age circumference
#> 1    1  118            30
#> 2    1  484            58
#> 3    1  664            87
#> 4    1 1004           115
#> 5    1 1231           120
#> 6    1 1372           142
```

Identify the dataset with its DOI, and represent its creators using
`utils::person()`. Draper has a VIAF authority identifier; Smith is
represented by a local RDF *blank node*.

``` r
dataset_iri <- iri("https://doi.org/10.5281/zenodo.14917851")

draper <- utils::person(
  given = "N.R.", family = "Draper", role = "cre",
  comment = c(VIAF = "84585260")
)
smith <- utils::person(given = "H.", family = "Smith", role = "cre")

draper_iri <- iri("http://viaf.org/viaf/84585260")
smith_id <- "_:smith"
```

We reuse standard RDF vocabularies rather than inventing metadata
properties: **DCAT** identifies datasets, **Dublin Core Terms (DCT)**
describes them, and **PROV-O** records provenance. `person_triples()`
also uses **RDFS** for agent labels, while `rdf_term()` uses **XSD**
datatypes for literal values. Namespace variables keep the code
readable; N-Triples itself writes complete IRIs.

``` r
prov <- "http://www.w3.org/ns/prov#"
dcat <- "http://www.w3.org/ns/dcat#"
dct <- "http://purl.org/dc/terms/"

triples <- n_triples(c(
  n_triple(dataset_iri, "a", paste0(dcat, "Dataset")),
  n_triple(dataset_iri, paste0(dct, "title"), "Growth of Orange Trees"),
  n_triple(dataset_iri, paste0(prov, "wasAttributedTo"), draper_iri),
  n_triple(dataset_iri, paste0(prov, "wasAttributedTo"), smith_id),
  person_triples(draper),
  person_triples(smith, subject = smith_id)
))
cat(triples, sep = "\n")
#> <https://doi.org/10.5281/zenodo.14917851> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/dcat#Dataset> .
#> <https://doi.org/10.5281/zenodo.14917851> <http://purl.org/dc/terms/title> "Growth of Orange Trees"^^<http://www.w3.org/2001/XMLSchema#string> .
#> <https://doi.org/10.5281/zenodo.14917851> <http://www.w3.org/ns/prov#wasAttributedTo> <http://viaf.org/viaf/84585260> .
#> <https://doi.org/10.5281/zenodo.14917851> <http://www.w3.org/ns/prov#wasAttributedTo> _:smith .
#> <https://viaf.org/viaf/84585260> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> .
#> <https://viaf.org/viaf/84585260> <http://www.w3.org/2000/01/rdf-schema#label> "N.R. Draper"^^<http://www.w3.org/2001/XMLSchema#string> .
#> _:smith <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> .
#> _:smith <http://www.w3.org/2000/01/rdf-schema#label> "H. Smith"^^<http://www.w3.org/2001/XMLSchema#string> .
```

Each line describes a resource or relationship using globally defined
terms. For example, `prov:wasAttributedTo` links the dataset to a
creator; the resulting N-Triples statement contains its full IRI. This
metadata can be used by other RDF-aware systems without reading R
objects.

To save the statements as a portable N-Triples file:

``` r
writeLines(triples, "orange-provenance.nt", useBytes = TRUE)
```

For a fuller explanation of RDF terms, identifiers, vocabularies, and
serialization, see the introductory vignette
(`vignette("intro", package = "ntriples")`).
