# Cox proportional hazards output is stable

    Code
      tula(m)
    Output
      Cox regression / Ties: efron
      No. of subjects =      228                              Number of obs =      228
      No. of failures =      165                              AIC           = 1489.696
      Time at risk    =    69593                              Concordance   =   0.6029
      Log likelihood  = -742.848                                                      
      ────────────────────────────────────────────────────────────────────────────────
      survival::Su~) │Haz. Ratio       DMSE          z     P>|z|  [95% Conf  Interval]
      ────────────────────────────────────────────────────────────────────────────────
      age            │     1.017    .009382      1.848     .0646       .999      1.036
      sex            │     .5986      .1002     -3.065     .0022      .4311      .8311
      ────────────────────────────────────────────────────────────────────────────────

# Cox model with exp = TRUE (hazard ratios) is stable

    Code
      tula(m, exp = TRUE)
    Output
      Cox regression / Ties: efron
      No. of subjects =      228                                        Number of obs =      228
      No. of failures =      165                                        AIC           = 1489.696
      Time at risk    =    69593                                        Concordance   =   0.6029
      Log likelihood  = -742.848                                                                
      ──────────────────────────────────────────────────────────────────────────────────────────
      survival::Surv(time, s~) │Haz. Ratio       DMSE          z     P>|z|  [95% Conf  Interval]
      ──────────────────────────────────────────────────────────────────────────────────────────
      age                      │     1.017    .009382      1.848     .0646       .999      1.036
      sex                      │     .5986      .1002     -3.065     .0022      .4311      .8311
      ──────────────────────────────────────────────────────────────────────────────────────────

# conditional logistic (clogit) output is stable

    Code
      tula(m)
    Output
      Conditional (fixed-effects) logistic regression
      LR chi2(2)     =     53.15                             Number of obs    = 248
      Prob > chi2    = 2.869e-12                             Number of groups =  83
      Log likelihood =   -64.202                                                   
      McFadden R-sq  =    0.2928                                                   
      ─────────────────────────────────────────────────────────────────────────────
      case        │      Coef  Std. Err.          z     P>|z|  [95% Conf  Interval]
      ─────────────────────────────────────────────────────────────────────────────
      spontaneous │     1.986      .3524      5.635    <.0001      1.295      2.677
      induced     │     1.409      .3607      3.906    <.0001       .702      2.116
      ─────────────────────────────────────────────────────────────────────────────

# survival regression (survreg / tobit) output is stable

    Code
      tula(m)
    Output
      Survival regression / Distribution: Weibull
      AIC            = 185.523                                Number of obs  =      26
      BIC            = 190.556                                Uncensored     =      12
      Log likelihood = -88.762                                Right-censored =      14
                                                              Pseudo R-sq    = 0.09384
      ────────────────────────────────────────────────────────────────────────────────
      survival::Su~) │      Coef  Std. Err.          z     P>|z|  [95% Conf  Interval]
      ────────────────────────────────────────────────────────────────────────────────
      age            │    -.0791     .01979     -3.997    <.0001     -.1179    -.04031
      rx             │     .5673      .3403      1.667     .0955    -.09975      1.234
      (Intercept)    │     10.46      1.443      7.252    <.0001      7.635      13.29
      ────────────────────────────────────────────────────────────────────────────────
      /sigma         │     .5506      .1295                           .3473      .8732
      ────────────────────────────────────────────────────────────────────────────────

