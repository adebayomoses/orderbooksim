## Submission

This is a new submission.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

The local check also reported messages about tools missing on the local
machine (qpdf, HTML Tidy, an online time check); these are not package issues.

## Test environments

* Local: Windows 10, R 4.5.1 (`R CMD check --as-cran`, vignettes and PDF
  manual built)
* GitHub Actions: Windows (release), macOS (release), Ubuntu (devel, release,
  oldrel-1) [TODO: confirm all passed before submitting]
* win-builder: R-devel [TODO: confirm result from the e-mail before submitting]

## Notes for reviewers

* The package is pure R; there is no compiled code.
* Simulations use R's random number generator. When a `seed` is supplied,
  the user's global random state is restored afterwards with
  `withr::with_seed()`.
* The optional 'shiny' app (`run_lob_app()`) is in Suggests, and its example
  runs only in interactive sessions.
* Plot methods restore any graphical parameters they change.
* Examples, tests and vignettes do not write to the user's home file space
  and do not use the network.
