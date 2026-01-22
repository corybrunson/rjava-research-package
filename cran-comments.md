## Checks

* local OS X installs, R 4.2.1 and 4.5.1
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`

### R CMD check results



### WinBuilder



Several possibly misspelled words are in fact spelled correctly: "Reeb", "ReebGraphPairing", and "rJava".

## Downstream dependencies

As this is an initial submission, there are no dependencies to check.
