library(dplyr)

# get info for comparison and QCing site IDs
previous <- read.table('inst/extdata/RatingCurves_JudHarvey_161227.tsv', header=TRUE, sep='\t')
mb <- mda.streams::get_meta(c('basic', 'dvqcoefs'))
sites <- mda.streams::list_sites()

# first batch of coefficients from Jud
library(readxl)
key1 <- readLines('inst/extdata/harvey_170128/160518_powstreams_sites_metab.txt') # the site list I sent to Jud
input1 <- read_excel('inst/extdata/harvey_170128/RatingCurves_MetabolismPowellCenter_JWH_revised.xlsx') # the first [new] response from Jud
input1$site_num <- sapply(as.character(input1$StationID), function(sn) {
  smatch <- substring(grep(paste0(sn, '$'), sites, value=TRUE), 6)
  if(length(smatch) > 1) warning(paste0('multiple matches for ', sn, ': ', paste0(smatch, collapse=', ')))
  smatch <- if(length(smatch) == 0) NA else smatch[1]
})
# ?!?!?!? 31 sites present that weren't given. 21 of these are present in the
# full sites list...no idea about the other 10.
cantfind1 <- filter(input1, StationID != -9999 & is.na(site_num)) %>% .$StationID
canfindwhere <- sapply(setNames(nm=as.character(input1$StationID[input1$StationID != -9999])), function(cf) {
  found <- grep(paste0(cf, '$'), key1)
  if(length(found) == 1) 'key1' else {
    found <- grep(paste0(cf, '$'), sites)
    if(length(found) == 1) 'all sites' else NA
  }
})
table(canfindwhere, useNA='always')

# second batch of coefficients from Jud
key2 <- read.csv('inst/extdata/harvey_170128/sites_missing_dvqcoefs MANUAL CHECK.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
key2b <- readLines('inst/extdata/harvey_170128/sites_missing_dvqcoefs_list.txt')
input2 <- read.csv('inst/extdata/harvey_170128/RatingCurve coefficients_Teds sites using Best nhd_comid_final.csv', header=TRUE, stringsAsFactors=FALSE)
input2$site_num <- as.character(key2b[!is.na(key2$BEST_COMID)])
cantfind2 <- filter(input2, StationID != -9999 & is.na(site_num)) %>% .$StationID

# combine the two batches of coefficients
library(unitted)
dvqcoefs <- bind_rows(input1, rename(input2, Q_ma_m3_div_s=Qbf_ma_m3_div_s)) %>%
  filter(StationID != -9999) %>%
  rename(jud_a=a_coeff, jud_b=b_coeff, jud_c=c_coeff, jud_f=f_coeff) %>%
  mutate(jud_a=ifelse(jud_a == -9999, NA, jud_a),
         jud_b=ifelse(jud_b == -9999, NA, jud_b),
         jud_c=ifelse(jud_c == -9999, NA, jud_c),
         jud_f=ifelse(jud_f == -9999, NA, jud_f)) %>%
  u(c(StationID=NA, NHDcompid=NA, Q_ma_m3_div_s='m^3 s^-1', w_ma_m='m', d_ma_m='m',
      jud_a='m', jud_b=NA, jud_c='m', jud_f=NA, site_num=NA))

# confirm that we still have those totally extra 10 sites, then remove them
stillcantfind <- v(dvqcoefs[['StationID']])[which(is.na(v(dvqcoefs[['site_num']])))]
all.equal(cantfind1, stillcantfind)
dvqcoefs <- filter(dvqcoefs, !is.na(site_num)) %>%
  select(site_num, nhdplus_id=NHDcomid, jud_b, jud_f, jud_a, jud_c)
# stillcantfind: 1018035 3082500 3260015 3260050 3260100 3262001 3292550 5357206 6386500 6917630

# compare to previous version (as on SB)
newcoefsites <- dvqcoefs$site_num[which(!is.na(dvqcoefs$jud_c))]
oldcoefsites <- mb$site_num[which(!is.na(mb$dvqcoefs.c))]
(newsites <- setdiff(newcoefsites, oldcoefsites)) # 130 of them
(lostsites <- setdiff(oldcoefsites, newcoefsites)) # none
dvqcomp <- full_join(mutate(v(dvqcoefs), site_num_num=as.numeric(site_num)),
                     mutate(previous, 
                            jud_a=ifelse(jud_a == -9999, NA, jud_a),
                            jud_b=ifelse(jud_b == -9999, NA, jud_b),
                            jud_c=ifelse(jud_c == -9999, NA, jud_c),
                            jud_f=ifelse(jud_f == -9999, NA, jud_f)),
                     by=c('site_num_num'='site_num'))
library(ggplot2)
library(cowplot)
plot_grid(
  ggplot(dvqcomp, aes(x=jud_a.y, y=jud_a.x)) + geom_abline() + geom_point() + xlab('Old a') + ylab('New a'),
  ggplot(dvqcomp, aes(x=jud_b.y, y=jud_b.x)) + geom_abline() + geom_point() + xlab('Old b') + ylab('New b'),
  ggplot(dvqcomp, aes(x=jud_c.y, y=jud_c.x)) + geom_abline() + geom_point() + xlab('Old c') + ylab('New c'),
  ggplot(dvqcomp, aes(x=jud_f.y, y=jud_f.x)) + geom_abline() + geom_point() + xlab('Old f') + ylab('New f'))

write.table(v(dvqcoefs), 'inst/extdata/harvey_170128/RatingCurves_JudHarvey_170128.tsv', sep='\t', row.names=FALSE)
write.table(v(dvqcoefs), 'inst/extdata/RatingCurves_JudHarvey.tsv', sep='\t', row.names=FALSE)
