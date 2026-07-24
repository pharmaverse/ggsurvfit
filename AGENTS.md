# AGENTS.md

Guidance for AI coding agents working in the **ggsurvfit** repository. Human contributors
should also read [`.github/CONTRIBUTING.md`](.github/CONTRIBUTING.md), which this file
summarizes and does not replace.

## What this package is

**ggsurvfit** is an R package (a ggplot2 extension) for building publication-ready
time-to-event / survival figures. Its design principle: every add-on is a *real* ggplot2
layer, so package functions compose with ordinary ggplot2 code via `+`.

- **Kaplan–Meier:** `survfit2()` wraps `survival::survfit()` (keeping the call/environment so
  labels and formulas can be recovered), then `ggsurvfit()` draws it.
- **Competing risks:** `tidycmprsk::cuminc()` fits are drawn with `ggcuminc()`.
- **Modifiers:** `add_confidence_interval()`, `add_risktable()`, `add_censor_mark()`,
  `add_quantile()`, `add_pvalue()`, `add_legend_title()`, `scale_ggsurvfit()`, etc. Each is
  added to a plot with `+`.

## Architecture conventions

- **Add-on functions return layer objects; the real work happens in an `ggplot_add.*` S3
  method.** Pattern (see [`R/add_pvalue.R`](R/add_pvalue.R)): the exported constructor stashes
  arguments as `attr()`s on a small classed object; a `ggplot_add.<class>()` method (registered
  via `#' @export`) reads those attributes and modifies the plot when `+` is evaluated. Follow
  this pattern for any new `add_*()` function rather than mutating the plot eagerly.
- **Data flows through `tidy_survfit()` / `tidy_cuminc()`**, which normalize `survfit`/`cuminc`
  objects into tidy data frames with publication-friendly strata labels. Prefer reusing these
  over re-parsing model objects.
- **Reach into a built plot with `ggplot2::ggplot_build(p)`** when a function needs the plotted
  data (e.g. which outcome/strata are on screen). Suppress expected warnings with
  `suppressWarnings()` as the existing code does.
- Small internal helpers live in [`R/utils.R`](R/utils.R) and
  [`R/utils-add_risktable.R`](R/utils-add_risktable.R); internal helpers are named with a
  leading dot (e.g. `.extract_data_from_survfit()`) or are plainly un-exported. Check here
  before writing a new helper.

## Dependencies

- **Imports** (usable freely, no `::` needed once imported): broom, cli, dplyr, ggplot2 (>=
  4.0.0), glue, gtable, patchwork, rlang, survival, tidyr.
- **Suggests** (competing-risks + testing/tooling): tidycmprsk, testthat, vdiffr, scales,
  withr, knitr, rmarkdown, covr, spelling. **Always guard/qualify Suggests** — reference them
  as `tidycmprsk::cuminc()` and, in code paths that require them, call
  `rlang::check_installed("tidycmprsk")` (see [`R/tidy_cuminc.R`](R/tidy_cuminc.R)).
- The magrittr pipe `%>%` and the native `|>` both appear; match the surrounding file.

## Style and messaging

- Follow the tidyverse [style guide](https://style.tidyverse.org). You may run
  [`styler`](https://styler.r-lib.org), but **do not restyle code unrelated to your change.**
- **Errors/warnings/messages** use cli, imported bare as `cli_abort()`, `cli_warn()`,
  `cli_inform()` (see the `@importFrom cli` line in
  [`R/ggsurvfit-package.R`](R/ggsurvfit-package.R)). Convention: state the problem first, then
  an optional `"i"` hint:
  ```r
  cli_abort(c("!" = "There was an error.",
              "i" = "A helpful message to resolve it."))
  ```
- **Documentation is roxygen2 with markdown** (`Roxygen: list(markdown = TRUE)`). Edit the
  roxygen comments in `R/*.R`, never the generated `man/*.Rd` or `NAMESPACE` by hand — run
  `devtools::document()` to regenerate them.

## Testing

- testthat edition 3 (`Config/testthat/edition: 3`); tests live in `tests/testthat/` as
  `test-<topic>.R` mirroring the `R/` file names.
- **Test plot structure by building first:** `ggplot2::ggplot_build(p)` (or the package's
  `ggsurvfit_build()`), then assert on the returned data — don't eyeball the object. Assert the
  *correct* value, not merely that output is non-empty (a weak `grepl("p", ...)` check let bug
  #277 slip through).
- **Image regression tests use vdiffr** (`vdiffr::expect_doppelganger(...)`). These are skipped
  when vdiffr is absent and are typically gated behind `skip_on_ci()` / `skip_on_cran()`.
- New features and bug fixes **must** include a test. For bug fixes, write a test that fails on
  the old code and passes on the fix.

## Developer workflow (run from the package root)

```r
devtools::load_all()        # load current source
devtools::document()        # regenerate man/ and NAMESPACE after roxygen changes
devtools::test()            # run all tests  (or: testthat::test_file("tests/testthat/test-<x>.R"))
devtools::check()           # full R CMD check — should pass clean before a PR
devtools::install_dev_deps()# first-time setup
```

If `Rscript`/`R` is not on `PATH` (common on Windows), invoke it by full path, e.g.
`"C:\Program Files\R\bin\Rscript.bat"`.

## Change checklist (before opening a PR)

1. Code follows tidyverse style; only your lines are restyled.
2. roxygen updated and `devtools::document()` run (man/ + NAMESPACE regenerated).
3. Tests added/updated; `devtools::test()` green.
4. `devtools::check()` clean.
5. **User-facing changes get a `NEWS.md` bullet** at the top (just under the first header),
   in tidyverse [NEWS style](https://style.tidyverse.org/news.html), referencing the issue/PR
   number (e.g. `(#277)`).
6. PR title briefly describes the change; PR body contains `Fixes #<issue>`.
```
