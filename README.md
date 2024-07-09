
# plots.r

## Installation

You can install the development version of plots.r like so:

``` r
install.packages("https://github.com/mrvillage/plots.r/archive/refs/heads/master.tar.gz", repos=NULL) # use .zip for Windows
# OR
devtools::install_github("mrvillage/plots.r")
```

## Usage

### `plots.r::basic_forest_plot`

```r
plots.r::basic_forest_plot(
    x = x,
    se = se,
    width = width, # you will need to mess around with this to get it to look right
    height = height, # you will need to mess around with this to get it to look right
    name = name,
    header = c(left_header, right_header),
    slab = data_labels,
    xlab = x_axis_label,
    # other arguments from https://wviechtb.github.io/metafor/reference/forest.default.html
)

# Example
plots.r::basic_forest_plot(
    x = c(0.1, 0.2, 0.3, 0.4, 0.5),
    se = c(0.01, 0.02, 0.03, 0.04, 0.05),
    width = 500,
    height = 300,
    name = "Example",
    header = c("Left", "Right"),
    slab = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5"),
    xlab = "X-axis label",
    # other arguments from https://wviechtb.github.io/metafor/reference/forest.default.html
)
```

![Example forest plot](/Example.png)

### `plots.r::grouped_forest_plot`

```r
plots.r::grouped_forest_plot(
    x = x, # a list of vectors
    se = se, # a list of vectors
    width = width, # you will need to mess around with this to get it to look right
    height = height, # you will need to mess around with this to get it to look right
    name = name,
    header = c(left_header, right_header),
    slab = data_labels,
    xlab = x_axis_label,
    glab = group_labels,
    # other arguments from https://wviechtb.github.io/metafor/reference/forest.default.html
)

# Example
plots.r::grouped_forest_plot(
    x = list(c(0.1, 0.2, 0.3, 0.4, 0.5), c(0.1, 0.2, 0.3, 0.4, 0.5)),
    se = list(c(0.01, 0.02, 0.03, 0.04, 0.05), c(0.01, 0.02, 0.03, 0.04, 0.05)),
    width = 500,
    height = 500,
    name = "Grouped Example",
    header = c("Left", "Right"),
    slab = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5"),
    xlab = "X-axis label",
    glab = c("Group 1", "Group 2"),
    # other arguments from https://wviechtb.github.io/metafor/reference/forest.default.html
)
```

![Example forest plot](/Grouped Example.png)
