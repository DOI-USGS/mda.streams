# saveRDS test script from http://rpubs.com/hadley/saveRDS

test_saveRDS_manual <- function() {
  library(dplyr)
  library(tibble)
  library(microbenchmark)
  
  ## functions
  roundtrip <- function(dat, con_fun, reps=10, ...) {
    test <- tempfile()
    con <- con_fun(test, ...)
    on.exit(close(con))
    
    save <- summary(microbenchmark(saveRDS(dat, con), times=reps), unit='ms')$mean
    load <- summary(microbenchmark(x <- readRDS(test)), unit='ms')$mean
    size <- file.info(test)$size / (1024) ^ 2
    
    file.remove(test)
    tibble(save, load, size)
  }
  save_load_timing <- function(dat, reps=10, ...) {
    bind_rows(
      data.frame(type='raw', level=0, roundtrip(dat, file), stringsAsFactors=FALSE),
      data.frame(type='gz', level=1, roundtrip(dat, gzfile, reps=reps, compression = 1), stringsAsFactors=FALSE),
      data.frame(type='gz', level=6, roundtrip(dat, gzfile, reps=reps, compression = 6), stringsAsFactors=FALSE),
      data.frame(type='gz', level=9, roundtrip(dat, gzfile, reps=reps, compression = 9), stringsAsFactors=FALSE),
      data.frame(type='bz', level=1, roundtrip(dat, bzfile, reps=reps, compression = 1), stringsAsFactors=FALSE),
      data.frame(type='bz', level=6, roundtrip(dat, bzfile, reps=reps, compression = 6), stringsAsFactors=FALSE),
      data.frame(type='bz', level=9, roundtrip(dat, bzfile, reps=reps, compression = 9), stringsAsFactors=FALSE),
      data.frame(type='xz', level=1, roundtrip(dat, xzfile, reps=reps, compression = 1), stringsAsFactors=FALSE),
      data.frame(type='xz', level=6, roundtrip(dat, xzfile, reps=reps, compression = 6), stringsAsFactors=FALSE),
      data.frame(type='xz', level=9, roundtrip(dat, xzfile, reps=reps, compression = 9), stringsAsFactors=FALSE)
    ) %>% 
      mutate(
        total=save+load, 
        typelevel=paste0(type, level),
        timesize=4*(load/max(load)) + 2*(size/max(size)) + 1*(save/max(save))) %>% # metric weights load time above size above save time
      arrange(timesize) %>%
      mutate(typelevel=ordered(typelevel,typelevel))
  }
  plot_save_load_timing <- function(times) {
    ggplot(times, aes(x=typelevel, group=1)) + 
      geom_line(aes(y=timesize), color='purple', size=2) + 
      geom_line(aes(y=total*max(timesize)/max(total)), color='red') + 
      geom_line(aes(y=size*max(timesize)/max(size)), color='blue')
  }
  test_save_load_recovery <- function(df) {
    # specific test for gz6
    test <- tempfile()
    con <- gzfile(test, compression=6)
    on.exit(close(con))
    saveRDS(df, file=con)
    df2 <- readRDS(test)
    expect_equal(df, df2)
    expect_equal(get_fit(df), get_fit(df2))
    expect_equal(get_data(df), get_data(df2))
    list(original=df, reloaded=df2)
  }
  
  # mda.streams data test
  library(mda.streams)
  dat1 <- get_ts('doobs_nwis', 'nwis_08062500')
  dat2 <- get_ts('sitetime_calcLon', 'nwis_01654000')
  dat3 <- get_ts(c('dischdaily_calcDMean','er_estBest','doamp_calcDAmp'), 'nwis_07263296')
  
  sl1 <- save_load_timing(dat1, reps=5)
  sl2 <- save_load_timing(dat2, reps=5)
  sl3 <- save_load_timing(dat3, reps=5)
  
  # > sl1
  # Source: local data frame [10 x 8]
  # 
  #     type level        save       load      size      total typelevel timesize
  #    (chr) (dbl)       (dbl)      (dbl)     (dbl)      (dbl)    (fctr)    (dbl)
  # 1     gz     1   48.158323  19.740499 0.7653599   67.89882       gz1 1.143106
  # 2     gz     6  238.926692  19.830199 0.8620081  258.75689       gz6 1.262351
  # 3     gz     9 2929.961255  18.798167 0.8573570 2948.75942       gz9 2.140506
  # 4    raw     0    8.590187   9.084234 3.8016768   17.67442      raw0 2.336116
  # 5     xz     1  221.396080  62.755205 0.4095116  284.15128       xz1 2.592687
  # 6     xz     6  815.813783  60.899201 0.3982887  876.71298       xz6 2.721585
  # 7     xz     9  846.573774  62.543268 0.3982887  909.11704       xz9 2.792383
  # 8     bz     1  399.983579  80.742574 0.4715405  480.72615       bz1 3.345998
  # 9     bz     6  486.986184 101.418807 0.6027946  588.40499       bz6 4.203089
  # 10    bz     9  512.466602 109.059519 0.5891695  621.52612       bz9 4.484858
  # > sl2
  # Source: local data frame [10 x 8]
  # 
  #     type level      save      load       size      total typelevel timesize
  #    (chr) (dbl)     (dbl)     (dbl)      (dbl)      (dbl)    (fctr)    (dbl)
  # 1     gz     1   3.44088 1.1785474 0.05821514   4.619427       gz1 1.310645
  # 2     gz     6  18.60831 1.0524404 0.07236099  19.660755       gz6 1.465403
  # 3    raw     0   1.07508 0.3861819 0.17895031   1.461262      raw0 2.216045
  # 4     gz     9 230.90109 0.9951914 0.07295895 231.896281       gz9 2.360159
  # 5     xz     1  16.65863 5.0151763 0.03260422  21.673804       xz1 3.181752
  # 6     xz     9  67.99456 4.7342939 0.03287125  72.728855       xz9 3.253315
  # 7     xz     6  66.31027 5.0717108 0.03287125  71.381981       xz6 3.430716
  # 8     bz     1  16.54748 6.2035989 0.03845215  22.751081       bz1 3.897149
  # 9     bz     9  17.92943 6.7044844 0.03873825  24.633913       bz9 4.180506
  # 10    bz     6  16.04497 7.3075258 0.03873825  23.352495       bz6 4.502438
  # > sl3
  # Source: local data frame [10 x 8]
  # 
  #     type level       save      load       size     total typelevel timesize
  #    (chr) (dbl)      (dbl)     (dbl)      (dbl)     (dbl)    (fctr)    (dbl)
  # 1     gz     1  3.0573434 0.7037264 0.06089783  3.761070       gz1 1.958087
  # 2     gz     6  4.4814828 0.6484578 0.06143284  5.129941       gz6 1.969640
  # 3    raw     0  0.8493294 0.2703229 0.08471489  1.119652      raw0 2.192119
  # 4     gz     9 16.4542774 0.6708427 0.06138515 17.125120       gz9 2.269001
  # 5     xz     1 15.7109240 4.8623052 0.05676651 20.573229       xz1 4.806208
  # 6     xz     9 38.1641802 4.6907631 0.05625153 42.854943       xz9 5.221858
  # 7     bz     1 13.9289436 5.8033907 0.06252384 19.732334       bz1 5.497663
  # 8     xz     6 41.8250706 5.3759436 0.05625153 47.201014       xz6 5.744876
  # 9     bz     9 14.1852932 6.2934393 0.06252384 20.478733       bz9 5.815258
  # 10    bz     6 31.2639484 5.9386473 0.06252384 37.202596       bz6 5.998094
  
  # Someday, when feather is more stable, try it. But they're not yet guaranteeing
  # consistent file formats release to release.
  # Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
  # Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")
  # devtools::install_github("wesm/feather/R")
  # library(feather)
}
