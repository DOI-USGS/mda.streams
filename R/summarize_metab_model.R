#' Summarize a single metabolism model
#' 
#' Returns a data.frame of metrics, one row per model_name
#' 
#' @param model_name the name of the metab_model file
#' @inheritParams download_item_files
#' @param out a list of one or more outputs to include in the summary dataframe,
#'   in addition to the model_name and parsed name columns.
#' @import dplyr
#' @import streamMetabolizer
#' @export
summarize_metab_model <- function(
  model_name, on_local_exists='replace',
  out=c('num_dates','num_complete','KvR_kendall_cor',
        'GPP_pct_neg','ER_pct_pos','K600_pct_neg',
        'GPP_q0', 'GPP_q10', 'GPP_q50', 'GPP_q90', 'GPP_q100',
        'ER_q0', 'ER_q10', 'ER_q50', 'ER_q90', 'ER_q100',
        'K600_q0', 'K600_q10', 'K600_q50', 'K600_q90', 'K600_q100',
        'DO_RMSE_q0', 'DO_RMSE_q10', 'DO_RMSE_q50', 'DO_RMSE_q90', 'DO_RMSE_q100')) {
  
  # download the model objects
  mms <- lapply(setNames(model_name,model_name), function(mname) {
    get_metab_model(mname, on_local_exists = on_local_exists)
  })
  
  # parse the name into some starter information about each model
  model_info <- parse_metab_model_name(model_name) %>% 
    add_rownames(var="model_name") %>% 
    as.data.frame(stringsAsFactors=FALSE)
  model_info_units <- rep(NA, ncol(model_info)) %>% setNames(names(model_info))
    
  # define helper functions
  frac_sign <- function(var, sign=c('+','-')) {
    function(metab, ...) { 
      vals <- metab[!is.na(metab[[var]]), var] 
      100 * length(which(if(sign=='+') vals>0 else vals<0)) / length(vals) 
    }
  }
  met_quant <- function(var, q) {
    function(metab, ...) { quantile(metab[[var]], probs=q, na.rm=TRUE) }
  }
  do_quant <- function(q) {
    function(doRMSEdaily, ...) { quantile(doRMSEdaily$RMSE, probs=q, na.rm=TRUE) }
  }
  # define the functions that might be applied to each model
  summary_funs <- list(
    num_dates = function(metab, ...) { nrow(metab) },
    num_complete = function(mod, metab, ...) { 
      estnames <- if(class(mod) == 'metab_night') c('ER','K600') else c('GPP','ER','K600')
      length(which(complete.cases(metab[estnames]))) 
    },
    KvR_kendall_cor = function(metab, ...) {
      metab_complete <- metab[!is.na(metab$ER) & !is.na(metab$K600),]
      cor(metab_complete$ER, metab_complete$K600, method='kendall')
    },
    GPP_pct_neg = frac_sign('GPP','-'),
    ER_pct_pos = frac_sign('ER','+'),
    K600_pct_neg = frac_sign('K600','-'),
    GPP_q0 = met_quant('GPP',0.0),
    GPP_q10 = met_quant('GPP',0.1),
    GPP_q50 = met_quant('GPP',0.5),
    GPP_q90 = met_quant('GPP',0.9),
    GPP_q100 = met_quant('GPP',1.0),
    ER_q0 = met_quant('ER',0.0),
    ER_q10 = met_quant('ER',0.1),
    ER_q50 = met_quant('ER',0.5),
    ER_q90 = met_quant('ER',0.9),
    ER_q100 = met_quant('ER',1.0),
    K600_q0 = met_quant('K600',0.0),
    K600_q10 = met_quant('K600',0.1),
    K600_q50 = met_quant('K600',0.5),
    K600_q90 = met_quant('K600',0.9),
    K600_q100 = met_quant('K600',1.0),
    DO_RMSE_q0 = do_quant(0.0),
    DO_RMSE_q10 = do_quant(0.1),
    DO_RMSE_q50 = do_quant(0.5),
    DO_RMSE_q90 = do_quant(0.9),
    DO_RMSE_q100 = do_quant(1.0)
  )
  # select only those functions that have been requested; get units
  out_summary <- out[out %in% names(summary_funs)]
  summary_funs <- summary_funs[out_summary]
  units_gpp <- units_er <- unique(get_var_src_codes(metab_var=='GPP', out='units'))
  units_K600 <- unique(get_var_src_codes(metab_var=='K600', out='units'))
  units_DO <- unique(get_var_src_codes(metab_var=='DO.obs', out='units'))
  summary_units <- c(
    num_dates='dates', num_complete='dates',
    KvR_kendall_cor=NA, GPP_pct_neg='%', ER_pct_pos='%', K600_pct_neg='%',
    GPP_q0=units_gpp, GPP_q10=units_gpp, GPP_q50=units_gpp, GPP_q90=units_gpp, GPP_q100=units_gpp,
    ER_q0=units_er, ER_q10=units_er, ER_q50=units_er, ER_q90=units_er, ER_q100=units_er,
    K600_q0=units_K600, K600_q10=units_K600, K600_q50=units_K600, K600_q90=units_K600, K600_q100=units_K600,
    DO_RMSE_q0=units_DO, DO_RMSE_q10=units_DO, DO_RMSE_q50=units_DO, DO_RMSE_q90=units_DO, DO_RMSE_q100=units_DO
  )[out_summary]
  
  # actually apply the selected functions
  mm_summary <- lapply(seq_len(nrow(model_info)), function(model_row) {
    mm <- mms[[model_row]]
    metab <- predict_metab(mm)
    dopreds <- predict_DO(mm)
    doRMSEdaily <- dopreds %>% group_by(local.date) %>% summarize(RMSE=sqrt(mean((DO.obs-DO.mod)^2))) %>% as.data.frame
    as.data.frame(c(
      as.list(model_info[model_row,]),
      lapply(summary_funs, function(fun) { fun(mod=mm, metab=metab, doRMSEdaily=doRMSEdaily) })
    ), stringsAsFactors=FALSE)
  }) %>%
    bind_rows() %>% 
    as.data.frame() %>%
    u(c(model_info_units, summary_units))
  
  mm_summary
}