# METADATA
# This analysis script uses data from the 'Care City Cohort'
# It joins data from Adult Social Care, other council services, and NHS/CCG data
# The analysis aims to describe the association between hospital discharge and domiciliary care
# Author: Dan Lewer
# Most recent version: 28 Oct 2021

# :::::::::::::::::::::::::::::::::::
# Load packages and bespoke functions
# ...................................

options(scipen = 999) # prevents printing of scientific formats in outputs

library(data.table) # for data reading and processing
library(lubridate) # for date formatting
library(stringi) # for string processing (functions starting with 'stri_')
library(RColorBrewer) # provides colours for plots

vpt <- function(x, t, form = F, digs = 2, ...) { # vectorized poisson confidence intervals (https://gist.github.com/danlewer/636f392d0a86b3e7b8f75579e352cba2)
  f <- function(xl, tl) {
    if (is.na(xl) | is.na(tl)) return(c(0, 0, 0))
    y <- poisson.test(xl, tl, ...)
    c(xl/tl, y$conf.int[1:2])
  }
  res <- mapply(f, xl = x, tl = t)
  return(if (form) {
    res <- format(round(res, digs), digits = digs, nsmall = digs)
    res <- apply(res, 2, function(x) paste0(x[1], '(', x[2], '-', x[3], ')'))
    res <- gsub(' ', '', res)
    gsub('\\(', ' (', res)
  } else {
    res
  })
}

vrr <- function (c1, c2, t1, t2, form = T, digs = 2) { # regression-based confidence intervals for rate ratio
  f <- function (c1, c2, t1, t2) {
    m <- glm(c(c1, c2) ~ c('a', 'b') + offset(log(c(t1, t2))), family = 'poisson')
    m <- `dimnames<-`(exp(cbind(coef(m), confint(m))), list(c('intercept', 'rate_ratio'), c('est', 'lower', 'upper')))
    m[2,]
  }
  r <- mapply(f, c1 = c1, c2 = c2, t1 = t1, t2 = t2)
  if (form == T) {
    r <- format(round(r, digs), nsmall = digs, digits = digs)
    r <- paste0(r[1,], '(', r[2,], '-', r[3,], ')')
    gsub('\\(', ' (', gsub(' ', '', r))
  } else {
    t(r)
  }
}

propCI <- function(X, N, ..., form = T, digs = 0, pc = T) { # vectorized binomial confidence interval
  r <- t(rbind(X/N, mapply(function(x, n) prop.test(x, n, ...)$conf.int[1:2], x = X, n = N)))
  colnames(r) <- c('prop', 'lower', 'upper')
  if (form == T) {
    if (pc == T) {r <- r * 100}
    r <- format(round(r, digs), nsmall = digs, digits = digs)
    r <- paste0(r[,1], '(', r[,2], '-', r[,3], ')')
    gsub('\\(', ' (', gsub(' ', '', r))
  } else {
    r
  }
}

last_status <- function(a, vals = unique(a), excl = NA, fill = NA) { # last status in a vector (https://gist.github.com/danlewer/8466ecc5dea046c288bcec6b64ea353d)
  if (!is.na(excl)) {vals <- setdiff(vals, excl)}
  i <- a %in% vals
  j <- a[i][cumsum(i)]
  c(rep(fill, length(a) - length(j)), j)
}

# :::::::::::::::::::::::::::
# Cohort entry and exit dates
# ...........................

start_date <- as.integer(as.Date("2018-04-04", origin = '1970-01-01'))
end_date <- as.integer(as.Date("2021-01-31", origin = '1970-01-01'))

# :::::::::
# Read data
# .........

# adult social care data
# ......................

setwd("S:/Innovations/BD Research Project/dlewer/dom_care")

asc <- fread("dom_care_open_cases.csv", select = c('Unique_PatientID', 'DIM_SERVICE_TYPE_ID_DESC', 'DESCRIPTION', 'START_DTTM', 'END_DTTM', 'dod'), col.names = c('id', 'type', 'support', 'start', 'end', 'dod'))
date_cols <- c('start', 'end', 'dod')
asc[, (date_cols) := lapply(.SD, dmy), .SDcols = date_cols]

# identify dom care packages

asc[, dc := grepl('homecare', type, ignore.case = T)]
asc[, dc := dc | type %in% c('Crisis Intervention', 'Crisis Intervention (Double Handed)')]
asc[dc == T, .N, type] # check types of package identified as dom care

# council data
# ............

council2018 <- fread("S:/Innovations/BD Research Project/Source files by setting/Individual (Council)/clean csv/RM2018person.csv",select = c('pt_id', 'household_id', 'age_group'), col.names = c('id', 'hh', 'age_gp'))

# add number per household

council2018 <- council2018[, .(hhm = .N), hh][council2018, on = 'hh'] # household members

# ccg data
# ........

setwd("S:/Innovations/BD Research Project/Source files by setting/Simon- revised 18-19 and 19-20 datasets_29Dec20")
morbs <- c('Blindless_flag', 'Cancer_flag', 'Coronary_heart_disease_flag', 'CKD_flag', 'Chronic_Liver_Disease_flag', 'COPD_flag', 'Dementia_flag', 'Depression_flag', 'Diabetes_flag', 'Heart_Failure_flag', 'Learning_disability_flag', "Parkinson's_disease_flag", 'Peripheral_vascular_disease_flag', 'Stroke_flag')
ccg1920 <- fread("Individual_FY19-20_revised_v2_Non_PID.csv", colClasses = 'character', select = c('UniquePatientID', 'Gender', 'YearOfBirth', 'MMYYYYDeath', 'date_registered_end', morbs), col.names = c('id', 'sex', 'yob', 'dod', 'end_reg', morbs))
ccg1819 <- fread("Individual_FY18-19_revised_v2_Non_PID.csv", colClasses = 'character', select = c('UniquePatientID', 'Gender', 'YearOfBirth', 'MMYYYYDeath', 'date_registered_end', morbs), col.names = c('id', 'sex', 'yob', 'dod', 'end_reg',morbs))
ccg1920[, yr := '19_20']
ccg1819[, yr := '18_19']
ccg <- rbind(ccg1819, ccg1920)
ccg[, (morbs) := lapply(.SD, as.integer), .SDcols = morbs]

# number of morbidities (from list defined in 'morbs')

ccg[, nm := rowSums(ccg[, morbs, with = F])]
ccg[, nm3 := fifelse(nm > 2, '3+', as.character(nm))]
ccg[, nm3 := factor(nm3, c(0, 1, 2, '3+'))]

# /// show that dom care clients are mostly in in CCG data
table(asc[dc == T, id] %chin% ccg$id)

# format dates

ccg[, dod := stri_pad(dod, width = 6, pad = '0')]
ccg$dod[ccg$dod == '000000'] <- NA_character_
ccg[, dod2 := make_date(day = 1, month = as.integer(stri_sub(dod, 0, 2)), year = as.integer(stri_sub(dod, 3)))]
ccg[, end_reg2 := ymd(end_reg)]
ccg[, yr_start := fifelse(yr == '18_19', as.Date('2018-04-01', origin = '1970-01-01'), as.Date('2019-04-01', origin = '1970-01-01'))]
ccg[, dod2 := as.integer(dod2)]
ccg[, end_reg2 := as.integer(end_reg2)]
ccg[, end := pmin(dod2, end_reg2, end_date, na.rm = T)]

# consolidate 2 years' data

d <- ccg[, .(entry = min(yr_start), exit = max(end), yrdob = min(yob)), id]
d[, entry := as.integer(entry)]
ccg[, x := rowid(id)]
d_sex <- ccg[x == 1, c('id', 'sex')]
d_morb <- ccg[, lapply(.SD, sum), by = id, .SDcols = morbs]
d_morb[, (morbs) := lapply(.SD, function (x) x > 0), .SDcols = morbs]
d <- d_morb[d, on = 'id']
d <- d_sex[d, on = 'id']

# make age variable, with randomly assigned day of birth

d[, dob := make_date(year = yrdob)]
set.seed(717)
d[, dob := dob + sample(seq_len(365), .N, replace = T)]
d[, dob := as.integer(dob)]
d[, ageEntry := (entry - dob)/365.25]
hist(d$ageEntry, breaks = 100, main = NA, xlab = 'Age at entry', col = 'seagreen')
age_lims <- c(19, seq(30, 80, 10))
d[, ageGroup := findInterval(ageEntry, age_lims)]
d[, ageGroup := factor(ageGroup, seq_along(age_lims), age_lims)]

# hospital data
# .............

setwd("S:/Innovations/BD Research Project/Source files by setting/ServiceUseHosp/Clean csv")
hosp_read <- function (...) fread(..., select = c('pt_id', 'start_date_hospital_provider_spell', 'los', 'spell_primary_diagnosis'), col.names = c('id', 'admidate', 'los', 'icd10'))
nel_1819 <- hosp_read("HospNEL1819.csv")
nel_1920 <- hosp_read("HospNEL1920.csv")
el_1819 <- hosp_read("HospEL1819.csv")
el_1920 <- hosp_read("HospEL1920.csv")

# add field showing whether elective or non-elective

nel_1819[, e := 'nel']
nel_1920[, e := 'nel']
el_1819[, e := 'el']
el_1920[, e := 'el']

# create single dataset and format

hospital <- rbind(nel_1819, nel_1920, el_1819, el_1920)
rm(nel_1819, nel_1920, el_1819, el_1920)
hospital[, admidate := as.integer(ymd(admidate))]
hospital[, day := admidate + los]
hospital <- unique(hospital[, c('id', 'day', 'e', 'icd10', 'los')])
hospital[, hid := .I]

setwd("S:/Innovations/BD Research Project/dlewer/dom_care")

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# limit to individuals age 19-109 with known sex in CCG and council data
# ......................................................................

participants <- intersect(d[ageEntry >= 19 & ageEntry < 110 & sex %in% c('Male', 'Female'), id], council2018$id)
d <- d[id %chin% participants]
asc <- asc[id %chin% participants]
hospital <- hospital[id %chin% participants]

# :::::::::::::::::::::::::
# dom care during follow-up
# .........................

asc <- d[, c('id', 'entry', 'exit')][asc, on = 'id']
asc[, c('start', 'end') := lapply(.SD, as.integer), .SDcols = c('start', 'end')]

d[, dc := id %chin% asc[dc == T & start <= exit & end >= entry, unique(id)]]
d[, dc := factor(dc, c(T, F), c('yes', 'no'))]

# ::::::::::::::::::::::::::::::::::::::
# rate stratified by hospital discharges
# ......................................

# create daily data for domiciliary care packages
# ...............................................

asc[, eI := pmin(end_date, end, na.rm = T)]
# /// show that all small number of packages are missing start dates
sapply(asc[dc == T], function(x) sum(is.na(x)))
asc <- asc[!is.na(start)]
asc[, pid := .I] # package id

dc_long <- asc[dc == T, .(day = seq(start, eI)), c('id', 'pid')]
# /// show that package dates from 2013
as.Date(range(dc_long$day), origin = '1970-01-01')

# implement package start and end dates
# .....................................

days_of_domcare <- asc[dc == T, .(day = seq.int(from = start, to = eI)), c('id', 'pid')]
days_of_domcare <- unique(days_of_domcare[, c('id', 'day')])
days_of_domcare <- days_of_domcare[order(id, day)]

# create continuous timelines with "pauses" bridged

gap <- 30L # maximum number of days between packages that are merged

days_of_domcare[, dif := shift(day), id]
days_of_domcare[, dif := day - dif]
# /// check not all 1 day:
days_of_domcare[, .N, dif][order(dif)]

# fill in gaps of length 'gap' or less

fill <- days_of_domcare[dif > 1 & dif <= gap]
fill[, tmp_id := .I]
fill_days <- fill[, .(day = seq.int(from = day - dif + 1, to = day - 1)), c('id', 'tmp_id')]
fill_days[, tmp_id := NULL]
days_of_domcare <- rbind(days_of_domcare, fill_days, fill = T)
days_of_domcare <- days_of_domcare[order(id, day)]

# add date of new package or ending package

days_of_domcare[, dif := shift(day), id]
days_of_domcare[, dif := day - dif]
# /// show there are lots of 1's then jumps to 31, as expected
days_of_domcare[, .N, dif][order(dif)]
days_of_domcare[, new_package := dif > 1 | is.na(dif)]
days_of_domcare[, package_end := shift(new_package, -1), id]
days_of_domcare$package_end[is.na(days_of_domcare$package_end)] <- T

# daily dataset
# .............

date_range <- sapply(c('2018-04-01', '2020-03-31'), as.Date, origin = '1970-01-01')
starting_number <- nrow(days_of_domcare[day == date_range[1]])

whole_cohort_daily <- d[, .(day = seq.int(from = entry, to = exit)), id]

# dom care group

Sys.sleep(1); gc() # prevents computer from crashing
domcare_cohort_daily <- whole_cohort_daily[id %chin% asc[dc == T, unique(id)]]
Sys.sleep(1); gc()
domcare_cohort_daily <- days_of_domcare[domcare_cohort_daily, on = c('id', 'day')]
domcare_cohort_daily[, dif := NULL]
domcare_cohort_daily[, new_package := fifelse(is.na(new_package), F, new_package)]
domcare_cohort_daily[, package_end := fifelse(is.na(package_end), F, package_end)]
Sys.sleep(1); gc()

# add discharge dates to client timelines

domcare_cohort_daily <- hospital[domcare_cohort_daily, on = c('id', 'day')]

# discharge in 7 days before new package

weekAfterHosp <- domcare_cohort_daily[!is.na(hid)]
weekAfterHosp[, hid := .I]
weekAfterHosp <- weekAfterHosp[, .(day = seq.int(from = day, to = day + los + 7L)), c('id', 'hid')]
weekAfterHosp <- unique(weekAfterHosp)
weekAfterHosp[, weekAfter := 1]

domcare_cohort_daily <- weekAfterHosp[domcare_cohort_daily, on = c('id', 'day')]
domcare_cohort_daily[, weekAfter := ifelse(is.na(weekAfter), F, T)]

# /// show that about 25% of new packages have a hospital discharge in the week before
domcare_cohort_daily[new_package == T & day <= end_date, table(weekAfter)]

domcare_cohort_daily <- d[, c('id', 'ageGroup')][domcare_cohort_daily, on = 'id']
weekAfterRate <- domcare_cohort_daily[day <= end_date, .(years = .N/365.25, new_packages = sum(new_package)), c('weekAfter', 'ageGroup')]

# never had a DC package

d[, fu := exit - entry + 1]
no_dc_hosp <- hospital[id %chin% d[dc == 'no', id]]
no_dc_hosp <- d[, c('id', 'ageGroup')][no_dc_hosp, on = 'id']
no_dc_hosp[, dis := day + los]
no_dc_hosp[, weekAfter := dis + 7L]
no_dc_hosp_long <- no_dc_hosp[, .(day = seq.int(from = dis, to = weekAfter)), c('id', 'hid', 'ageGroup')]
no_dc_hosp_long <- unique(no_dc_hosp_long[, c('id', 'day', 'ageGroup')])

no_dc_weeks <- no_dc_hosp_long[, .(years_hosp_no_dc = .N/365.25), ageGroup][d[dc == 'no', .(total_years_no_dc = sum(fu)/365.25), ageGroup], on = 'ageGroup']
no_dc_weeks[, total_years_no_dc := total_years_no_dc - years_hosp_no_dc]
no_dc_weeks <- melt(no_dc_weeks, id.vars = 'ageGroup', variable.name = 'weekAfter', value.name = 'years')
no_dc_weeks[, weekAfter := weekAfter == 'years_hosp_no_dc']
no_dc_weeks[, new_packages := 0L]
no_dc_weeks[, source := 'no_dc']
weekAfterRate[, source := 'dc']
weekAfterRate <- rbind(weekAfterRate, no_dc_weeks)

weekAfterRateAge <- weekAfterRate[, .(years = sum(years), new_packages = sum(new_packages)), c('weekAfter', 'ageGroup')]
weekAfterRateAge <- weekAfterRateAge[order(ageGroup, weekAfter)]
weekAfterRateAge <- rbind(weekAfterRateAge, weekAfterRateAge[, .(years = sum(years), new_packages = sum(new_packages)), weekAfter], fill = T)

weekAfterRateAge[, r := vpt(new_packages, years / 100, form = T)]
weekAfterRateAge <- dcast(weekAfterRateAge, ageGroup ~ weekAfter, value.var = c('years', 'new_packages', 'r'))
weekAfterRateAge[, rr := vrr(c1 = new_packages_FALSE, c2 = new_packages_TRUE, t1 = years_FALSE, t2 = years_TRUE, digs = 2)]
weekAfterRateAge[, pc_hosp := propCI(X = new_packages_TRUE, N = new_packages_TRUE + new_packages_FALSE, pc = T, digs = 2)]

fwrite(weekAfterRateAge, 'table2_28oct2021.csv')

#  :::::::::::::::::::::::::::::::::::::::::::
#  analysis of dom care by discharge diagnosis
#  ...........................................

# create daily data

hos2 <- hospital[day <= end_date]
hos2 <- d[, c('id', 'ageEntry', 'sex')][hos2, on = 'id']
hos2 <- ccg[yr == '18_19', c('id', 'nm3', 'nm')][hos2, on = 'id']
hos2 <- hos2[ageEntry >= 19]
hospital_daily <- unique(hos2[, .(day = seq.int(from = day, to = day + los + 7L)), c('id', 'hid')])

# add new dom care packages

new_packages <- domcare_cohort_daily[new_package == T, c('id', 'day', 'new_package')]
hospital_daily <- new_packages[hospital_daily, on = c('id', 'day')]
hos2[, ndc := hid %in% hospital_daily[new_package == T, hid]] # new dom care package

# summarise primary cause of admission

gr <- read.csv(url("https://raw.githubusercontent.com/danlewer/group_tree/main/icd10_3l.csv"), stringsAsFactors = F)
setDT(gr)

hos2[, icd3 := stri_sub(icd10, 0, 3)]
hos2 <- gr[hos2, on = 'icd3']
age_lims <- c(19, seq(30, 80, 10))
hos2[, age_gp := findInterval(ageEntry, age_lims)]
hos2[, age_gp := factor(age_gp, seq_along(age_lims), age_lims)]

# summarise length of stay

los_cat <- c(0, 1, 5, 10, 20)
hos2[, losc := findInterval(los, los_cat)]
hos2[, losc := factor(losc, seq_along(los_cat), los_cat)]

# identify frailty-related admissions based on primary diagnosis

frail_icd3 <- c('F32', 'F33', 'F38', 'F41', 'F43', 'F44',
                'F05',
                'F00',
                'F01', 'F02', 'F03', 'R41',
                'Z74', 'Z75',
                'R55', 'S32', 'S33', 'S42', 'S43', 'S62', 'S72', 'S73', 'W00', 'W01', 'W02', 'W03', 'W04', 'W05', 'W06', 'W07', 'W08', 'W09', paste0('W', 10:19),
                'R15', 'R32', 
                'R26', 'R74',
                'L89',
                'R54')
hos2[, frail := icd3 %in% frail_icd3]

# describe admissions
# ...................

da <- function (x) {
  a <- hos2[, table(get(x), ndc)]
  pc <- t(t(a) / colSums(a))
  pc <- round(pc * 100, 1)
  a <- formatC(a, big.mark = ',')
  r <- paste0(a, '(', pc, ')')
  r <- gsub('\\(', ' (', gsub(' ', '', r))
  cbind(var = c(x, rep('', nrow(a)-1)), lev = row.names(a), matrix(r, ncol = 2))
}

dac <- function (x, digs = 1) {
  a <- aggregate(get(x) ~ ndc, hos2, quantile, probs = c(0.25, .5, 0.75))
  a <- a$`get(x)`
  a <- format(round(a, digs), nsmall = digs, digits = digs)
  a <- paste0(a[,2], '(', a[,1], '-', a[,3], ')')
  a <- gsub('\\(', ' (', gsub(' ', '', a))
  cbind(var = x, lev = 'med(IQR)', matrix(a, ncol = 2))
}

hos2[, total := 1L]

table3 <- list(
  da('total'),
  da('age_gp'),
  dac('ageEntry'),
  da('sex'),
  da('nm3'),
  dac('nm', digs = 0),
  da('losc'),
  dac('los'),
  da('e'),
  da('frail')
)
table3 <- do.call('rbind', table3)
write.csv(table3, 'table3_28oct2021.csv')

# ::::::::::::::::::::::::::::::::::::
# expected number of dom care packages
# ....................................

ref <- hos2[, .(ref_pc = sum(ndc) / .N), age_gp]
hos2 <- ref[hos2, on = 'age_gp']
hos2[, ref_pc2 := sum(ndc) / .N]

dom_care_by_hosp_diagnosis <- hos2[, .(n = .N, dc = sum(ndc), ex = sum(ref_pc)), c('l2', 'icd3')][order(dc, decreasing = T)]
dom_care_by_hosp_diagnosis[, ratio := vpt(dc, ex, form = T, digs = 2)]
dom_care_by_hosp_diagnosis[, ex := round(ex, 2)]

# add non-age-adjusted expected number ('crude')

tmp <- hos2[, .(dc = sum(ndc), ex2 = sum(ref_pc2)), c('l2', 'icd3')][order(dc, decreasing = T)]
tmp[, ratio2 := vpt(dc, ex2, form = T, digs = 2)]
tmp[, ex2 := round(ex2, 2)]

dom_care_by_hosp_diagnosis <- tmp[dom_care_by_hosp_diagnosis, on = c('l2', 'icd3', 'dc')]
setnames(dom_care_by_hosp_diagnosis, c('ex2', 'ratio2', 'ex', 'ratio'), c('expected_crude', 'ratio_crude_ci', 'expected_ageadj', 'ratio_ageadj_ci'))
dom_care_by_hosp_diagnosis <- cbind(dom_care_by_hosp_diagnosis, `colnames<-`(t(vpt(dom_care_by_hosp_diagnosis$dc, dom_care_by_hosp_diagnosis$expected_crude, form = F)), c('ratio_crude', 'lower_crude', 'upper_crude')))
dom_care_by_hosp_diagnosis <- cbind(dom_care_by_hosp_diagnosis, `colnames<-`(t(vpt(dom_care_by_hosp_diagnosis$dc, dom_care_by_hosp_diagnosis$expected_ageadj, form = F)), c('ratio_ageadj', 'lower_ageadj', 'upper_ageadj')))
dom_care_by_hosp_diagnosis <- dom_care_by_hosp_diagnosis[, c('icd3', 'l2', 'n', 'dc', 'expected_crude', 'ratio_crude', 'lower_crude', 'upper_crude', 'ratio_crude_ci', 'expected_ageadj', 'ratio_ageadj', 'lower_ageadj', 'upper_ageadj', 'ratio_ageadj_ci')]

fwrite(dom_care_by_hosp_diagnosis[dc > 4], 'table4_28oct2021.csv')

#  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#  descriptive table with hospital-associated com care packages 
#  ............................................................

d[, dc := id %chin% domcare_cohort_daily[new_package == T, unique(id)]]
d[, hadc := id %chin% hos2[ndc == T, id]]
d[, total := 1L]

sum_cat_dc <- function(x) {
  a <- d[, .(n = .N, dc = sum(dc), hadc = sum(hadc)), get(x)]
  names(a) <- c('x', 'n', 'dc', 'hadc')
  a[, dc_pc := format(round(dc / sum(dc) * 100, 1), nsmall = 1, digits = 1)]
  a[, hadc_pc := format(round(hadc / sum(hadc) * 100, 1), nsmall = 1, digits = 1)]
  a[, n_pc := format(round(n / sum(n) * 100, 1), nsmall = 1, digits = 1)]
  a[, c('n', 'dc', 'hadc') := lapply(.SD, prettyNum, big.mark = ','), .SDcols = c('n', 'dc', 'hadc')]
  a[, dc := paste0(dc, ' (', dc_pc, ')')]
  a[, hadc := paste0(hadc, ' (', hadc_pc, ')')]
  a[, n := paste0(n, ' (', n_pc, ')')]
  a <- a[order(x)]
  cbind(variable = x, a[, c('x', 'n', 'dc', 'hadc')])
}

catv <- lapply(c('total', 'sex', 'ageGroup', morbs), sum_cat_dc)
catv <- rbindlist(catv)
catv <- catv[!(grepl('_flag', variable) & x == F)]
fwrite(catv, 'table1_28sept2021.csv')

d[, quantile(ageEntry)]
d[dc == T, quantile(ageEntry)]
d[hadc == T, quantile(ageEntry)]
