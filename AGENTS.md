# Guidance for Codex AI Agents

Welcome to **ggExpress**, an R package providing quick, opinionated wrappers around `ggpubr` for plotting. This file explains how to navigate the repository, run checks, and follow project conventions. It also orients newcomers to the codebase.

## Repository Structure
- `R/ggExpress.R`: single source file containing ~19 `q*` helper functions (e.g., `qhistogram`, `qbarplot`). Each function is documented with roxygen2 comments and exported in `NAMESPACE`.
- `man/`: auto-generated documentation from roxygen2.
- `Examples/`: minimal scripts demonstrating package usage.
- `Development/`: experimental or scratch scripts.
- Standard R package files: `DESCRIPTION`, `NAMESPACE`, `CITATION.cff`.

## Dependencies
This package relies on several packages, including the following @vertesy repos:
- [`Stringendo`](https://github.com/vertesy/Stringendo)
- [`ReadWriter`](https://github.com/vertesy/ReadWriter)
- [`CodeAndRoll2`](https://github.com/vertesy/CodeAndRoll2)
- [`MarkdownHelpers`](https://github.com/vertesy/MarkdownHelpers)
Other important CRAN dependencies: `ggpubr`, `cowplot`, `ggplot2`, and their transitive dependencies.

## Development Conventions
- Use **2 spaces** for indentation and keep lines under ~100 characters.
- Document functions with **roxygen2** comments; regenerate docs with `devtools::document()`.
- Prefix new plotting helpers with `q` to stay consistent (`qscatter`, `qviolin`, ...).
- Update `DESCRIPTION` and `NAMESPACE` when adding packages or exports.
- Commit directly to the main branch (no new branches).

## Testing & Checks
After modifying code or documentation:
1. Ensure required packages above are installed.
2. From the repository root run:
   ```bash
   R -q -e "devtools::document(); devtools::check()"
   ```
   This runs roxygen and the full `R CMD check`. Address all notes, warnings, and errors when possible.

## Getting Started
1. Read `README.md` for installation and example usage.
2. Explore functions in `R/ggExpress.R` and corresponding docs in `man/`.
3. Try out scripts in `Examples/` to see typical workflows.
4. For deeper understanding, review dependent packages like Stringendo and CodeAndRoll2.

Happy plotting!
