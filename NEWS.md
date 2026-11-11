# ntriples 0.1.0

The `ntriples` package extracts RDF N-Triples serialization functionality originally developed for the CRAN and rOpenSci package [`dataset`](https://docs.ropensci.org/dataset/) into a standalone, lightweight R utility.

Separating RDF serialization from dataset management makes this functionality reusable by other R packages and applications without requiring the higher-level data structures or dependencies of `dataset`. The package supports interoperable metadata and provenance for datasets, visualisations, and other resources created in R.

### Features

- Serialize R scalar values as RDF terms, including typed XSD literals, IRIs, and blank nodes.

- Construct and collect RDF N-Triples statements with `n_triple()` and `n_triples()`.

- Validate absolute IRIs using `iri()`.

- Serialize `utils::person()` objects as agents using PROV-O or FOAF.

- Support standard RDF vocabularies without introducing ontology-specific dependencies.

The package provides a minimal foundation for exchanging RDF metadata between R and other systems, while leaving higher-level data modelling to packages such as `dataset`.
