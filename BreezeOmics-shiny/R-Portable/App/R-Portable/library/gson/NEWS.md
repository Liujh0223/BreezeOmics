# gson 0.1.0

+ fixed `read.gson()` by setting the default values (if empty) to NULL (to make it compatible with 'clusterProfiler') (2023-03-03, Mon)
+ set `gsname="WikiPathways"` in `read.gmt.wp(output = "gson")` (2022-09-23, Fri)

# gson 0.0.9

+ `gsonList` function to construct `GSONList` object and corresponding `print` method implemented (2022-09-06, Tue)
 
# gson 0.0.8

+ remove `iconv` in `write.gson` as it may return NA (2022-08-20, Sat)

# gson 0.0.7

+ add `keytype` slot for `read.gson()` and `write.gson()`. (2022-08-5, Wed)
+ add `keytype` slot for GSON object. (2022-07-13, Wed)
