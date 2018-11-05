(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("caption" "font=footnotesize") ("subcaption" "font=footnotesize") ("graphicx" "pdftex") ("babel" "american")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-environments-local "alltt")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "times"
    "amsthm"
    "amsmath"
    "amssymb"
    "listings"
    "caption"
    "subcaption"
    "graphicx"
    "wrapfig"
    "epstopdf"
    "babel"
    "url"
    "color"
    "xspace"
    "float"
    "tabularx"
    "multirow"
    "alltt"
    "multicol"
    "blindtext"
    "scrextend"
    "geometry"
    "hyperref"
    "algorithmic"
    "algorithm")
   (TeX-add-symbols
    '("unit" 1)
    '("cardinality" 2)
    '("elements" 3)
    '("set" 1)
    "argmin")
   (LaTeX-add-labels
    "sec:Issue"
    "sec:Data"
    "sec:methods"
    "sec:analysis"
    "appendix"
    "code"
    "fig:weatherstation"
    "fig:fatalitymap"
    "fig:impactpanel"
    "fig:impactdivision"
    "fig:timedayind"
    "fig:divisionhour"
    "fig:roadcondweather")
   (LaTeX-add-amsthm-newtheorems
    "definition")
   (LaTeX-add-color-definecolors
    "dkgreen"
    "gray"
    "mauve"))
 :latex)

