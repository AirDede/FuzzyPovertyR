##################
#--- Examples ---#
##################

#######################
#--- fuzzy monetary ---#
########################

HCR <- .154

fm_construct(income = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID)

fm_construct(income = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040)
fm_construct(income = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040, alpha = 2)

fm_var(income = eusilc$eqIncome, weight = eusilc$db090, ID = NULL, HCR = .16, alpha = fuzzy_fm$alpha, type = 'bootstrap', R = 500, M = 1000) # troppo basso! per me è la precisione di uniroot e non la varianza oppure errore nel codice
fm_var(income = eusilc$eqIncome, weight = eusilc$db090, breakdown = eusilc$db040, ID = NULL, HCR = .16, alpha = fuzzy_fm$alpha, type = 'bootstrap', R = 500, M = 1000) # troppo basso! per me è la precisione di uniroot e non la varianza oppure errore nel codice

fm_var(income = eusilc$eqIncome, weight = eusilc$db090, ID = NULL, HCR = .16, alpha = fuzzy_fm$alpha, type = 'Jackknife', stratum = eusilc$stratum, psu = eusilc$PSU) # troppo basso! per me è la precisione di uniroot e non la varianza oppure errore nel codice
fm_var(income = eusilc$eqIncome, weight = eusilc$db090, breakdown = eusilc$db040, ID = NULL, HCR = .16, alpha = fuzzy_fm$alpha, type = 'Jackknife', stratum = eusilc$stratum, psu = eusilc$PSU) # troppo basso! per me è la precisione di uniroot e non la varianza oppure errore nel codice


# vorrei che i nomi delle funzioni o i nomi degli oggetti avessero un filo logico (ma attenzione a sovrascrivere gli oggetti)
data(eusilc)
step2 = fs_transform(test, weight = data$DB090, ID = data$ID); head(step2) # plots?

dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5) # f.g. assegnazione in gruppi
steps3_5 = fs_weight(dimensions, step2 = step2, rho = NULL); head(steps3_5) # for reporting

alpha = fs_equate(steps3_5 = steps3_5, weight = data$DB090, HCR = .16, interval = c(1,10))
# alpha = 2 se vogliono saltare alfa

fs_results = fs_construct(steps3_5 = steps3_5, weight = data$DB090, alpha = alpha, breakdown = NULL)
fs_results = fs_construct(steps3_5 = steps3_5, weight = data$DB090, alpha = alpha, breakdown = breakdown)


fs_var(data = test, weight = weight, ID = NULL, dimensions = dimensions, breakdown = NULL, HCR = .16, alpha = alpha, rho = NULL, type = 'bootstrap', M = NULL, R = 2, verbose = T)
fs_var(data = test, weight = weight, ID = NULL, dimensions = dimensions, breakdown = breakdown, HCR = .16, alpha = alpha, rho = NULL, type = 'bootstrap', M = NULL, R = 2, verbose = T)

fs_var(data = test, weight = weight, ID = NULL, dimensions = dimensions, breakdown = NULL, HCR = .16, alpha = alpha, rho = NULL, type = 'jackknife', stratum = stratum, psu = psu, verbose = T, f = .01)
fs_var(data = test, weight = weight, ID = NULL, dimensions = dimensions, breakdown = breakdown, HCR = .16, alpha = alpha, rho = NULL, type = 'jackknife', stratum = stratum, psu = psu, verbose = T, f = .01)


# NA nel campionamento se item ha tutta stessa modalità, fatto in modo che alla fine un na non venga considerato nella funzione weighted.mean (..., na.rm = T)

#

