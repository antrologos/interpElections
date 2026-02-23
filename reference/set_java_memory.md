# Set Java heap memory for r5r

Configures the maximum heap size for the Java Virtual Machine used by
r5r. Large travel-time matrices can require several gigabytes of JVM
heap.

## Usage

``` r
set_java_memory(size, persist = interactive())
```

## Arguments

- size:

  Character. Memory size with unit suffix, e.g. `"4g"` for 4 GB,
  `"512m"` for 512 MB, `"8g"` for 8 GB.

- persist:

  Logical. Also write to `~/.Renviron` so the setting persists across R
  sessions. Default: TRUE in interactive sessions.

## Value

Invisibly, the previous value of `getOption("java.parameters")`.

## Details

This sets `options(java.parameters = "-Xmx{size}")`. It **must** be
called before `r5r` (or `rJava`) is loaded — once the JVM starts, heap
size cannot be changed without restarting R.

A rule of thumb: allocate \\\ge 2\\ GB per million OD pairs you expect
in your travel-time matrix. For a municipality with 5,000 census tracts
and 200 polling locations, that's 1 million pairs — 2-4 GB is usually
enough. Larger cities (e.g. Sao Paulo) may need 8-16 GB.

## See also

[`check_r5r()`](https://antrologos.github.io/interpElections/reference/check_r5r.md)
to see current memory settings.

## Examples

``` r
if (FALSE) { # \dontrun{
set_java_memory("4g")
set_java_memory("8g", persist = TRUE)
} # }
```
