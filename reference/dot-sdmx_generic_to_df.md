# Flatten an SDMX-ML Generic data document into a data frame

A minimal SDMX-ML 2.1 "Generic" data reader, covering just the structure
OECD's SDMX REST API returns for a plain `Accept: application/xml`
request (no `dsd` or structure-specific negotiation): one `<Series>` per
dimension combination, each with a `<SeriesKey>` and optional series
level `<Attributes>`, holding one or more `<Obs>`, each with a single
`<ObsDimension>` (the time period), an `<ObsValue>` and optional
observation level `<Attributes>`. Elements are matched by local name
only, so the result does not depend on which namespace prefix a
particular server happens to use.

## Usage

``` r
.sdmx_generic_to_df(doc)
```

## Arguments

- doc:

  An `xml2` document or node, as read by
  [`read_xml`](http://xml2.r-lib.org/reference/read_xml.md).

## Value

A data frame with one row per observation: a column per SDMX dimension
and series/observation attribute, `obsTime` (the raw period string, e.g.
`"2021"` or `"2021-Q1"`) and `obsValue` (numeric).

## Details

A query that returns many thousands of `<Series>` (the OECD PDB industry
table, for instance, can be well over 100000 series, since it leaves the
industry dimension unfiltered) makes this function's own performance
matter: each `xml2`/`local-name()` XPath search here costs several
milliseconds in a document that size, seemingly regardless of how small
its own match is, so running one such search per `<Series>` or `<Obs>`
individually turned such a response into many minutes of parsing (and,
with a CI timeout in place, a failed build). Everything below instead
runs a handful of XPath searches against the whole document (or, for
`xml_find_first()`, against a whole nodeset at once, which is internally
vectorised over every node in it), counts each `<Series>`'s `<Obs>` from
its already-known child count rather than a further search, and
reassembles the result with ordinary vector/data-frame operations - no
XPath search is run per-Series or per-Obs at all.
