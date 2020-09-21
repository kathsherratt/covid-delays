library(magrittr)

source("get_linelist.R")

linelist_complete <- get_linelist() # currently a covidregionaldata PR: https://github.com/epiforecasts/covidregionaldata/pull/80

# group by country

linelist_delays <- dplyr::filter(linelist_complete, 
                                 !is.na(date_onset) & !is.na(country)) %>%
  dplyr::mutate(delay_adm_to_death = ifelse(death == TRUE,
                                            date_death_or_discharge - date_admission_hospital, NA),
                delay_onset_report = ifelse(delay_onset_report  < 0, NA, delay_onset_report),
                delay_onset_admission = ifelse(delay_onset_admission < 0, NA, delay_onset_admission),
                delay_onset_death = ifelse(delay_onset_death < 0, NA, delay_onset_death),
                delay_adm_to_death = ifelse(delay_adm_to_death < 0, NA, delay_adm_to_death),
                continent = countrycode::countrycode(sourcevar = country,
                                                     origin = "country.name",
                                                     destination = "continent"))

# Global averages
delays_global <- list("death_mean" = mean(linelist_delays$delay_onset_death, na.rm=T),
                          "death_sd" = sd(linelist_delays$delay_onset_death, na.rm=T),
                          "admission_mean" = mean(linelist_delays$delay_onset_admission, na.rm=T),
                          "admission_sd" = sd(linelist_delays$delay_onset_admission, na.rm=T),
                          "report_mean" = mean(linelist_delays$delay_onset_report, na.rm=T),
                      "adm_death_mean" = mean(linelist_delays$delay_adm_to_death, na.rm=T),
                      "adm_death_sd" = sd(linelist_delays$delay_adm_to_death, na.rm=T),
                          "report_sd" = sd(linelist_delays$delay_onset_report, na.rm=T))
                          
delays_continent <- linelist_delays %>%
  dplyr::group_by(continent) %>%
  dplyr::summarise("death_mean" = mean(delay_onset_death, na.rm=T),
                   "death_sd" = sd(delay_onset_death, na.rm=T),
                   "admission_mean" = mean(delay_onset_admission, na.rm=T),
                   "admission_sd" = sd(delay_onset_admission, na.rm=T),
                   "report_mean" = mean(delay_onset_report, na.rm=T),
                   "report_sd" = sd(delay_onset_report, na.rm=T),
                   "adm_death_mean" = mean(delay_adm_to_death, na.rm=T),
                   "adm_death_sd" = sd(delay_adm_to_death, na.rm=T),
                   n = dplyr::n(),
                   .groups = "drop") %>%
  dplyr::filter(n > 99)

delays_country <- linelist_delays %>%
  dplyr::group_by(country) %>%
  dplyr::summarise("death_mean" = mean(delay_onset_death, na.rm=T),
                   "death_sd" = sd(delay_onset_death, na.rm=T),
                   "admission_mean" = mean(delay_onset_admission, na.rm=T),
                   "admission_sd" = sd(delay_onset_admission, na.rm=T),
                   "report_mean" = mean(delay_onset_report, na.rm=T),
                   "report_sd" = sd(delay_onset_report, na.rm=T),
                   "adm_death_mean" = mean(delay_adm_to_death, na.rm=T),
                   "adm_death_sd" = sd(delay_adm_to_death, na.rm=T),
                   n = dplyr::n(),
                   .groups = "drop") %>%
  dplyr::filter(n > 99)



