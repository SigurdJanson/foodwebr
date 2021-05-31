
<!-- README.md is generated from README.Rmd. Please edit that file -->

# foodwebr

<!-- badges: start -->

<!-- badges: end -->

`foodwebr` makes it easy to visualise the dependency graph of a set of
functions (i.e. who calls who). This can be useful for exploring an
unfamiliar codebase, or reminding yourself what you wrote ten minutes
ago

## Installation

You can install foodwebr from GitHub:

``` r
devtools::install_github("lewinfox/foodwebr")
```

## Basic usage

Say we have a bunch of functions in the global environment, some of
which call each other:

``` r
library(foodwebr)
#> Warning: replacing previous import 'vctrs::data_frame' by 'tibble::data_frame'
#> when loading 'dplyr'

f <- function() 1
g <- function() f()
h <- function() { f(); g() }
i <- function() { f(); g(); h() }
j <- function() j()
```

A call to `foodweb()` will calculate a graph of the dependencies.

``` r
fw <- foodweb()
```

Printing the object will show the [graphviz](https://graphviz.org/)
representation:

``` r
fw
#> # A `foodweb`: 5 nodes and 7 edges
#> digraph 'foodweb' {
#>   f()
#>   g() -> { f() }
#>   h() -> { f(), g() }
#>   i() -> { f(), g(), h() }
#>   j() -> { j() }
#> }
```

Plotting will draw the graph.

``` r
plot(fw)
```

<img src="man/figures/README-foodweb-plot-1.png" width="100%" />

`foodweb()` looks at its calling environment by default. If you want to
look at another environment you can either pass a function to the `FUN`
argument of `foodweb()` or pass an environment to the `env` argument. If
`FUN` is provided then the value of `env` is ignored, and the
environment of `FUN` will be used.

If a specific function is passed to `FUN`, the default behaviour is to
remove functions that are not descendants or antecedents of that
function.

``` r
# `j()` will not be included
foodweb(FUN = g)
#> # A `foodweb`: 4 nodes and 6 edges
#> digraph 'foodweb' {
#>   g() -> { f() }
#>   h() -> { g(), f() }
#>   i() -> { g(), h(), f() }
#>   f()
#> }

# Force inclusion of unconnected functions by using `filter = FALSE`
foodweb(FUN = g, filter = FALSE)
#> # A `foodweb`: 5 nodes and 7 edges
#> digraph 'foodweb' {
#>   f()
#>   g() -> { f() }
#>   h() -> { f(), g() }
#>   i() -> { f(), g(), h() }
#>   j() -> { j() }
#> }
```

You can use `foodweb()` to map all the functions in a package. I’m using
`cowsay` here as it’s small enough that the output is readable.

``` r
if (requireNamespace("cowsay", quietly = TRUE)) {
  plot(foodweb(cowsay::say))
}
```

<img src="man/figures/README-foodweb-plot-package-1.png" width="100%" />

In case you want to do something with the
[graphviz](https://graphviz.org/) output (make it prettier, for
example), use `as.text = TRUE`. This returns the graphviz specification
as a character vector.

``` r
foodweb(as.text = TRUE)
#> digraph 'foodweb' {
#>   "f()"
#>   "g()" -> { "f()" }
#>   "h()" -> { "f()", "g()" }
#>   "i()" -> { "f()", "g()", "h()" }
#>   "j()" -> { "j()" }
#> }
```

## Digging deeper

`foodwebr` also exposes the workhorse functions in case you want to play
around with them.

### `foodweb_matrix()`

The starting point is to compute the function matrix. This idea, and
much of the implementation, was taken from
[`mvbutils::foodweb()`](https://rdrr.io/cran/mvbutils/man/foodweb.html).
The function matrix is 1 if the function on the y-axis calls the
function on the x-axis, and 0 otherwise. `foodweb_matrix()` looks at
functions in the global environment by default, but you can specify
another environment using the `env` argument.

``` r
funmat <- foodweb_matrix()

funmat
#> # A foodweb matrix: 5 functions and 7 links
#>       CALLEE
#> CALLER f g h i j
#>      f 0 0 0 0 0
#>      g 1 0 0 0 0
#>      h 1 1 0 0 0
#>      i 1 1 1 0 0
#>      j 0 0 0 0 1
```

### `graphviz_spec_from_matrix()`

`graphviz_spec_from_matrix()` translates the function matrix into a
character string containing a [graphviz](https://graphviz.org/)
specification:

``` r
graphviz_spec <- graphviz_spec_from_matrix(funmat)

graphviz_spec
#> digraph 'foodweb' {
#>   "f()"
#>   "g()" -> { "f()" }
#>   "h()" -> { "f()", "g()" }
#>   "i()" -> { "f()", "g()", "h()" }
#>   "j()" -> { "j()" }
#> }
```

### Visualisation

We can visualise the graph specification using
`Diagrammer::grViz()`.

``` r
DiagrammeR::grViz(graphviz_spec)
```

<img src="man/figures/README-foodweb-plot-graph-spec-1.png" width="100%" />

## Using `tidygraph`

The [`tidygraph`](https://tidygraph.data-imaginist.com/) package
provides tools for graph analysis. A `foodweb` object can be converted
into a tidy graph object using `tidygraph::as_tbl_graph()` to allow more
sophisticated analysis and visualisation.

``` r
if (requireNamespace("tidygraph", quietly = TRUE)) {
  tg <- tidygraph::as_tbl_graph(foodweb())
  tg
}
#> # A tbl_graph: 5 nodes and 7 edges
#> #
#> # A directed multigraph with 2 components
#> #
#> # Node Data: 5 x 1 (active)
#>   name 
#>   <chr>
#> 1 f    
#> 2 g    
#> 3 h    
#> 4 i    
#> 5 j    
#> #
#> # Edge Data: 7 x 2
#>    from    to
#>   <int> <int>
#> 1     2     1
#> 2     3     1
#> 3     3     2
#> # … with 4 more rows
```
