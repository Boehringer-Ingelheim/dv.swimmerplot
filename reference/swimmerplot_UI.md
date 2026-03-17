# Swimmer Plot Module UI

(For use outside of the DaVinci framework)\
Places the Swimmer Plot module UI at the call site of this function. A
matching call to \[swimmerplot_server()\] is necessary.\

## Usage

``` r
swimmerplot_UI(
  id,
  group_by_vars = NULL,
  sort_by_vars = NULL,
  jumping_enabled = FALSE
)
```

## Arguments

- id:

  \`\[character(1)\]\` Unique shiny ID. Must match the ID provided to
  \[swimmerplot_server()\].

- group_by_vars:

  \`\[character(n)\]\` Variables available for grouping subjects.

- sort_by_vars:

  \`\[character(n)\]\` Variables available for sorting subjects.

- jumping_enabled:

  \`\[logical(1)\]\` Whether clicking on a subject should enable
  navigation to detail view.

## Value

Shiny UI.

## See also

\[mod_swimmerplot()\] and \[swimmerplot_server()\]
