# data-frame codebook output is stable

    Code
      tula(d, codebook = TRUE)
    Output
      ────────────────────────────────────────────────────────────────────────────────
      mpg
      ────────────────────────────────────────────────────────────────────────────────
      
                   Type: Numeric (float)
      
                  Range: [10.4,33.9]
          Unique values: 25                                            Missing .: 0/32
      
                   Mean: 20.0906
              Std. dev.: 6.02695
      
            Percentiles:        10%       25%       50%       75%       90%
                              14.34    15.425      19.2      22.8     30.09
      
      ────────────────────────────────────────────────────────────────────────────────
      cyl
      ────────────────────────────────────────────────────────────────────────────────
      
                   Type: Numeric (float)
      
                  Range: [4,8]                                                Units: 2
          Unique values: 3                                             Missing .: 0/32
      
             Tabulation: Freq.  Value
                            11      4
                             7      6
                            14      8
      
      ────────────────────────────────────────────────────────────────────────────────
      am
      ────────────────────────────────────────────────────────────────────────────────
      
                   Type: Numeric (float)
      
                  Range: [0,1]                                                Units: 1
          Unique values: 2                                             Missing .: 0/32
      
             Tabulation: Freq.  Value
                            19      0
                            13      1
      

# factor-vector codebook output is stable

    Code
      tula(factor(mtcars$cyl), codebook = TRUE)
    Output
      ────────────────────────────────────────────────────────────────────────────────
      factor(mtcars$cyl)
      ────────────────────────────────────────────────────────────────────────────────
      
                   Type: Factor
      
          Unique values: 3                                             Missing .: 0/32
      
             Tabulation: Freq.  Level
                            11  4
                             7  6
                            14  8
      

# character-vector codebook (string mode) output is stable

    Code
      tula(x, codebook = TRUE)
    Output
      ────────────────────────────────────────────────────────────────────────────────
      x
      ────────────────────────────────────────────────────────────────────────────────
      
                   Type: String (str6)
      
          Unique values: 3                                            Missing "": 0/20
      
               Examples: "apple"
                         "banana"
                         "cherry"
      

# haven-labelled codebook output is stable

    Code
      tula(d, codebook = TRUE)
    Output
      ────────────────────────────────────────────────────────────────────────────────
      y
      ────────────────────────────────────────────────────────────────────────────────
      
                   Type: Numeric (float)
                  Label: y
      
                  Range: [0,1]                                                Units: 1
          Unique values: 2                                             Missing .: 0/32
      
             Tabulation: Freq.  Numeric  Label
                            19        0  auto
                            13        1  manual
      

# character-backed haven-labelled codebook output is stable

    Code
      tula(d, codebook = TRUE)
    Output
      ────────────────────────────────────────────────────────────────────────────────
      sex
      ────────────────────────────────────────────────────────────────────────────────
      
                   Type: Labelled string
                  Label: sex
      
          Unique values: 2                                              Missing .: 0/5
      
             Tabulation: Freq.  Numeric  Label
                             2        F  Female
                             3        M  Male
      

# haven codebook keeps numeric (not lexical) code ordering

    Code
      tula(d, codebook = TRUE)
    Output
      ────────────────────────────────────────────────────────────────────────────────
      g
      ────────────────────────────────────────────────────────────────────────────────
      
                   Type: Numeric (float)
                  Label: g
      
                  Range: [1,10]                                               Units: 1
          Unique values: 3                                              Missing .: 0/6
      
             Tabulation: Freq.  Numeric  Label
                             2        1  low
                             2        2  mid
                             2       10  high
      

