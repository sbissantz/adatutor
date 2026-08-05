# adatutor – Introducing Psychologists to Boosting with AdaBoost in R <img src='man/figures/adatutor.png' align="right" width="120"/>

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/sbissantz/adatutor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbissantz/adatutor/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/sbissantz/adatutor/branch/master/graph/badge.svg)](https://app.codecov.io/gh/sbissantz/adatutor?branch=master)
<!-- badges: end -->

### Description

The `adatutor` package supports our efforts to introduce boosting to
psychologists with little or no machine learning background. It provides the
necessary dataset and convenience functions used in our hands-on tutorial.

### Installation

The `adatutor` package is not (yet) available on CRAN — but you can install the
development version from GitHub.

```r
# with pak (recommended)
install.packages("pak")
pak::pak("sbissantz/adatutor")

# or with remotes
install.packages("remotes")
remotes::install_github("sbissantz/adatutor")
```

To read the vignettes offline, build them at install time. This needs
[Quarto](https://quarto.org) (>= 1.4) on your `PATH`:

```r
remotes::install_github("sbissantz/adatutor", build_vignettes = TRUE)
browseVignettes("adatutor")
```

### Contributing

Contributions, suggestions, and bug reports are welcome. Please
[open an issue](https://github.com/sbissantz/adatutor/issues) or submit a pull
request on GitHub.

### Generative AI Statement

We use generative AI to streamline and optimize this package.

Most importantly, we let it run regularly through the code base to catch bugs or
inconsistencies before they reach a release (with more or less success, to be
honest).

In addition, it helps us keep naming and coding conventions consistent, so
everything stays easy to follow and build on. Reliably, it tweaks our vignettes,
tunes the docs to match the code, and sharpens our error and warning messages (in
the hope of a better user experience). To our astonishment and horror, it always
finds typos, spots grammar issues, and detects awkward phrasing that slips past
us non-native speakers.

Beyond that, we use it extensively to improve the robustness of our package. For
instance, it assists us in extending our test suite and has successfully hunted
down multiple dirty little edge cases (we would probably have missed). More than
once, it has suggested structures that we now find easier to test and maintain.
Without complaining, it handles all GitHub Actions chores, helping the package
build on most systems and stay backward compatible.

We invest a great deal of time reviewing, modifying, discussing, and approving
the suggestions. That said, if something gets past us, feel free to
[let us know](https://github.com/sbissantz/adatutor/issues). In any case, we take
full responsibility for the final code and documentation. Our tools are
[Claude Code](https://claude.com/claude-code),
[GitHub Copilot](https://github.com/features/copilot), and
[DeepL Write](https://www.deepl.com/write).
