# Check r5r and Java 21 setup

Runs a diagnostic check for the r5r dependency chain: whether the `r5r`
package is installed and whether a suitable Java/JDK (version 21) is
available on the system.

## Usage

``` r
check_r5r()
```

## Value

Invisibly, a list with components:

- r5r_installed:

  Logical.

- java_found:

  Logical.

- java_version:

  Integer major version, or `NA`.

- java_sufficient:

  Logical. TRUE if version is exactly 21.

- java_memory:

  Character or NULL. Configured JVM max heap (e.g. `"4g"`).

- system_ram:

  Character or NULL. Total system RAM (e.g. `"16 GB"`).

- ready:

  Logical. TRUE if all checks pass.

## See also

[`setup_java()`](https://antrologos.github.io/interpElections/reference/setup_java.md)
to install Java 21,
[`set_java_memory()`](https://antrologos.github.io/interpElections/reference/set_java_memory.md)
to configure JVM heap size.

## Examples

``` r
if (FALSE) { # \dontrun{
check_r5r()
} # }
```
