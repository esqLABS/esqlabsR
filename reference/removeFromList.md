# Remove an entry from a list

Removes all occurrences of the entry from the list. If the entry is not
in the list nothing is removed.

## Usage

``` r
removeFromList(entry, listArg)
```

## Arguments

- entry:

  The entry to be removed

- listArg:

  The list from which the entry will be removed

## Value

The list without the entry. If the input is a vector, it is converted to
a list.

## Examples

``` r
myList <- list("one", "two", "one", "three")
myList <- removeFromList("one", myList)
print(myList)
#> [[1]]
#> [1] "two"
#> 
#> [[2]]
#> [1] "three"
#> 
```
