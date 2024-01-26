# Arithmetic law of variation ####
pop_variation_arit<- function(P0, ar, t) {
  # P0  numeric:  initial population
  # ar  numeric:  arithmetic rate of variation
  # t   numeric:  time span 
  
  if(!(is.numeric(P0) & 
       is.numeric(ar) & 
       is.numeric(t))) stop("'P0', 'ar' and 't' must be 'numeric'" )
  
  Pt = P0 * (1 + ar * t)
  return(Pt)
}

arit_rate<- function(P0, Pt, t) {
  # P0  numeric:  initial population
  # Pt  numeric:  final population
  # t   numeric:  time span 
  
  if(!(is.numeric(P0) & 
       is.numeric(Pt) & 
       is.numeric(t))) stop("'P0', 'Pt' and 't' must be 'numeric'" )
  
  ar<- (Pt - P0) / (P0 * t)
  return(ar)
}

# Geometric law of variation ####

pop_variation_geom<- function(P0, gr, t) {
  # P0  numeric:  initial population
  # gr  numeric:  geometric rate of variation
  # t   numeric:  time span 
  
  if(!(is.numeric(P0) & 
       is.numeric(gr) & 
       is.numeric(t))) stop("'P0', 'gr' and 't' must be 'numeric'" )
  
  Pt = P0 * (1 + gr)^t
  return(Pt)
}

geom_rate<- function(P0, Pt, t) {
  # P0  numeric:  initial population
  # Pt  numeric:  final population
  # t   numeric:  time span 
  
  if(!(is.numeric(P0) & 
       is.numeric(Pt) & 
       is.numeric(t))) stop("'P0', 'Pt' and 't' must be 'numeric'" )
  
  gr<- 10^(t^-1 * log(Pt / P0, base = 10)) - 1
  #gr<- exp(t^-1 * log(Pt / P0)) - 1
  return(gr)
}


# Exponential law of variation ####

pop_variation_exp<- function(P0, r, t) {
  # P0  numeric:  initial population
  # r   numeric:  exponential rate of variation
  # t   numeric:  time span 
  
  if(!(is.numeric(P0) & 
       is.numeric(r) & 
       is.numeric(t))) stop("'P0', 'r' and 't' must be 'numeric'" )
  
  Pt = P0 * exp(r * t)
  return(Pt)
}

exp_rate<- function(P0, Pt, t) {
  # P0  numeric:  initial population
  # Pt  numeric:  final population
  # t   numeric:  time span 
  
  if(!(is.numeric(P0) & 
       is.numeric(Pt) & 
       is.numeric(t))) stop("'P0', 'Pt' and 't' must be 'numeric'" )
  
  r<- t^-1 * log(Pt / P0)
  return(r)
}

# test ####
pop19711024<- 54136547
pop19811025<- 56556911
t<- 10.0027

arit_rate(pop19711024, pop19811025, t)
geom_rate(pop19711024, pop19811025, t)
exp_rate(pop19711024, pop19811025, t)

# start<- lubridate::ymd("19711024")
# end<- lubridate::ymd("19811025")
# 
# lubridate::int_diff(c(start, end)) |> lubridate::as.period(unit = "years")
