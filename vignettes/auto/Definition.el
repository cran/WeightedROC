(TeX-add-style-hook "Definition"
 (lambda ()
    (LaTeX-add-labels
     "eq:t_tau"
     "eq:weighted_FP"
     "eq:weighted_FN"
     "eq:weighted_FPR"
     "eq:weighted_TPR")
    (TeX-add-symbols
     "RR")
    (TeX-run-style-hooks
     "amssymb"
     "amsmath"
     "natbib"
     "graphicx"
     "hyperref"
     "verbatim"
     "fullpage"
     "cm"
     "latex2e"
     "art10"
     "article")))

