# Configuration for the Package
DESCRIPTION <- list(
  package.name = "ggExpress",
  version = "0.9.0",
  title = "ggExpress is the fastest way to create, annotate and export plots in R",
  description = "ggExpress is a set of R functions that allows you to generate precise figures easily,
    and create clean markdown reports about what you just discovered with your analysis script. It is ggplot-based smaller brother of vertesy/MarkdownReports. It helps you to:
    1. Create scientifically accurate (annotated) figures with very short code, making use of variable-, row- and columnnames.
    2. Save figures automatically as vector graphic (.pdf), that you can use from presentation to posters anywhere.
    3. Incorporate your figures automatically in a markdown report file.
    4. Describe your figures & findings in the same report in a clear and nicely formatted way, parsed from your variables into english sentences.
    5. Share your report, by exporting your report to .pdf, .html or .docx, or via Github or a personal website.",

  depends = "Stringendo, MarkdownHelpers, ggplot2, ggpubr",
  imports = "cowplot, tibble, RColorBrewer, MarkdownReports, CodeAndRoll2, ggVennDiagram",

  author.given = "Abel",
  author.family = "Vertesy",
  author.email = "av@imba.oeaw.ac.at",
  license = "GPL-3 + file LICENSE",
  github.user = "vertesy"
)
