# Guidance for Agents

Version: 2026.08.31-00:01

## I: Generic (all @vertesy repos)

### 3. General rules 

1. Write condense and very cleary understandable code and text.
2. ALWAYS understand the larger goal and the full context first: read the whole file, grep all call sites, check the roxygen block.
3. Use short inline comments to explain the code: separate lines before a larger block for the higher-level intent, and/or short trailing comments after a line for that specific step.

- Every function starts with a COMPACT input-argument assertion for key inputs. Use combined stopifnot statements.

### 2. Code Review Rules

Make every finding easy to scan and understand.

- Use simple, direct English.
- Use short sentences and bullet points.
- Avoid compiler jargon, dense technical language, and noun stacking.
- Name files, functions, variables, and arguments explicitly.
- Never use vague references such as "the new formal" or "subsequent values".

For each finding, state:

- **Problem:** What is wrong or will break?
- **Trigger:** When does it happen?
- **Fix:** What should be changed?

Keep only findings that can be explained clearly and concisely.
Do not flag formatting, line length, or missing tests.

### 3. Pull Request Descriptions

Open each PR with a few bullets per major change: what was wrong, how it was fixed, and whether it changes the function's output or behavior.

- Scale the description to the change: a typo or comment-only fix needs one short line, not a paragraph.
- Keep the whole description under 250 words; reserve that ceiling for genuinely complex PRs. If it doesn't fit, split the PR instead of writing more.

### 4. Update the Source, Not Just the Documentation

Documentation is generated from upstream sources: `.Rd` files from roxygen annotations and `DESCRIPTION` from `Development/Dependencies.R` via `config.R`.

Package rebuilds overwrite these files, so always update the upstream source first, then regenerate the documentation.

## II: Repos of R function libraries

- New arguments go at the end, just before `...`. Never insert in the middle.
- Do not use tests.
- Never update the package version unless the user explicitly requests a version change.
- Do not raise code review findings that ask for a package version change.

## III: ggExpress specific

**ggExpress** — R package of `q*` plotting wrappers around `ggpubr`.
Depends on @vertesy `Stringendo`, `ReadWriter`, `CodeAndRoll2`, `MarkdownHelpers`.

**ggExpress** — R package of `q*` plotting wrappers around `ggpubr`.

- `R/ggExpress.R`: single source file, ~19 `q*` plotting functions (e.g.: `qbarplot`); and helpers.
- `Development/`:  Files for documenting and re-creating the package. `config.R` contains very important parameters that will be propagated to namespace, description, or `CITATION.cff`.  
  scripts, incl. `Dependencies.R`.
- Depends on @vertesy `Stringendo`, `CodeAndRoll2`, `ReadWriter`, `MarkdownHelpers`.
