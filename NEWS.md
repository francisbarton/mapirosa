# mapirosa 0.1.6 (4 March 2024)

* Long-awaited update to `query_names_api()` that puts the httr2 query step into a sub-function (`names_query`) and uses `map` to vectorise over multiple place names.
* Also uses `purrr::safely()` to handle errors better.

# mapirosa 0.1.5 (27 September 2023)

* Yikes it has been a long time
* Rewritten some terra steps (set extent and crs) to adjust to changes in {terra}
* Removed the `chatty` UI option
* Fixed some @examples to pass the CMD check
* Un-@exported a function that didn't need to be user-facing
* Improved/fixed/wrote missing function param documentation

# mapirosa 0.1.1 (10 December 2021)

* Added area parameter handling to `names_api_query`
* R CMD check tests are passing with no notes

# mapirosa 0.1.0 (9 December 2021)

* Added `names_api_query` (OS Names API) as the first exported function from this package.

# mapirose 0.0.2 (25 November 2021)

* initial tests for `check_zoom` - part of the Maps API query system

# mapirosa 0.0.1 (19 November 2021)

* Initial work on `query_api` function, working with the OS Maps API
* Requires more work to convert coords to tiles and to produce a raster map of the tiles - therefore not yet completed or exported.
