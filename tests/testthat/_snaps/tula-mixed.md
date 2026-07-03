# linear mixed model (lmer) output is stable

    Code
      tula(m)
    Output
      Mixed-effects REML regression
      Log likelihood = -871.814                                    Number of obs     = 180
      AIC            = 1755.628                                    N groups: Subject =  18
      BIC            = 1774.786                                                           
      ────────────────────────────────────────────────────────────────────────────────────
      Reaction           │      Coef  Std. Err.          z     P>|z|  [95% Conf  Interval]
      ────────────────────────────────────────────────────────────────────────────────────
      Days               │     10.47      1.546      6.771    <.0001      7.438       13.5
      (Intercept)        │     251.4      6.825      36.84    <.0001        238      264.8
      ────────────────────────────────────────────────────────────────────────────────────
      Random-effects Parameters
      ────────────────────────────────────────────────────────────────────────────────────
      Subject: 18 groups │                                                                
        sd(_cons)        │     24.74                                                      
        sd(Days)         │     5.922                                                      
        corr(_cons,Days) │    .06555                                                      
      sd(Residual)       │     25.59                                                      
      ────────────────────────────────────────────────────────────────────────────────────

# generalized linear mixed model (glmer, exp) output is stable

    Code
      tula(m, exp = TRUE)
    Output
      Mixed-effects GLM / Family: binomial / Link: logit
      Log likelihood = -92.027                                               Number of obs  = 56
      AIC            = 194.053                                               N groups: herd = 15
      BIC            = 204.180                                                                  
      ──────────────────────────────────────────────────────────────────────────────────────────
      cbind(incidence, size ~) │Odds Ratio       DMSE          z     P>|z|  [95% Conf  Interval]
      ──────────────────────────────────────────────────────────────────────────────────────────
      period                   │                                                                
        2                      │     .3709      .1124     -3.272     .0011      .2047      .6718
        3                      │     .3236      .1045     -3.495     .0005      .1719      .6093
        4                      │      .206     .08695     -3.743     .0002     .09009      .4712
      (Intercept)              │      .247     .05711     -6.048    <.0001       .157      .3886
      ──────────────────────────────────────────────────────────────────────────────────────────
      Random-effects Parameters
      ──────────────────────────────────────────────────────────────────────────────────────────
      herd: 15 groups          │                                                                
        sd(_cons)              │     .6421                                                      
      ──────────────────────────────────────────────────────────────────────────────────────────

# crossed random effects render both grouping factors

    Code
      tula(m)
    Output
      Mixed-effects REML regression
      Log likelihood = -165.430             Number of obs    = 144
      AIC            =  338.861             N groups: plate  =  24
      BIC            =  350.740             N groups: sample =   6
      ────────────────────────────────────────────────────────────
      diameter         │      Coef  Std. Err.          z     P>|z|
      ────────────────────────────────────────────────────────────
      (Intercept)      │     22.97      .8086      28.41    <.0001
      ────────────────────────────────────────────────────────────
      Random-effects Parameters
      ────────────────────────────────────────────────────────────
      plate: 24 groups │                                          
        sd(_cons)      │     .8467                                
      sample: 6 groups │                                          
        sd(_cons)      │     1.932                                
      sd(Residual)     │     .5499                                
      ────────────────────────────────────────────────────────────

