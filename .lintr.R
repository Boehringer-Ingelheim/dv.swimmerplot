linters <- lintr::default_linters
linters <- lintr::modify_defaults(
  linters,
  cyclocomp_linter = NULL
)
