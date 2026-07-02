# survey-weighted logistic (svyglm) output is stable

    Code
      tula(m)
    Output
      Survey: Family: quasibinomial / Link: logit
                                                                 Number of obs = 32
                                                                 Population N  = 32
                                                                 Num. strata   =  1
                                                                 Num. PSUs     = 32
                                                                 Design df     = 31
      ─────────────────────────────────────────────────────────────────────────────
                              Linearized                                           
      am          │      Coef  Std. Err.          t     P>|t|  [95% Conf  Interval]
      ─────────────────────────────────────────────────────────────────────────────
      cyl         │     1.322      .6615      1.998     .0552    -.03125      2.675
      wt          │    -7.864      2.042     -3.851     .0006     -12.04     -3.688
      (Intercept) │     15.75       5.71      2.758     .0100      4.072      27.43
      ─────────────────────────────────────────────────────────────────────────────

