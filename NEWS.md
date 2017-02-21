# 0.9.20

* accommodate NAs in `parse_metab_model_path`, `_name`, and `_title`

# 0.9.15

* added `par_calcLatSw` to `build_calc_ts_needs`

# 0.9.14

* added `par_calcLatSw` option to `stage_calc_ts` to compute the merger of
modeled and observed light

# 0.9.12+ to 0.9.13

* `config_to_data` now converts instantaneous discharge (`disch`) from cfs, as 
on ScienceBase, to cms, as required by streamMetabolizer

* `list_sites`, `list_datasets`, and `list_tses` now accept a 
`with_ts_uploaded_after` argument

* minor improvements to `get_ts`, `build_calc_ts_needs` (again), and 
`stage_calc_ts` in support of the ongoing data re-pull in the `stream_metab_usa`
project

# 0.9.12

* updates throughout to accommodate changes to package dependencies (`dplyr` and
`tibble`)

* new: `login_sb` now looks for username and password in a stream_metab profile 
file if that file exists (outside the project directory)

* `stage_calc_ts` now tracks and reports on input data provenance for truly 
calculated (`src == calc[XYZ]`) variables

* minor improvements to `build_calc_ts_needs`, `get_site_coords`, 
`summarize_ts_files`, `stage_nwis_ts`, `stage_ldas_ts`, `stage_nwis_sitelist` in
support of the ongoing data re-pull in the `stream_metab_usa` project

* new: `cluster_bundle_packages` builds a zip file of bundled package and 
dependency sources for use in cluster computing

# 0.9.7

* new: `make_metab_config` is like `stage_metab_config` but more suited to 
interactive use (less typing required)

* new: `get_metab_prep` is a shortcut to `config_to_data(stage_metab_config())`,
with less typing required

* new: `get_metab_data` is a shortcut to `get_metab_prep(out='data', 
simplify_out=TRUE)`

* faster: `choose_data_source` takes shortcuts in locating a metabolism model 
when possible

# 0.9.6.3

* `locate_ts` and `locate_site` are now much more efficient in special cases of 
looking up many items at a time

* `view_google_map` can now open several tabs at a time

* `summarize_metab_mode`l works with the newest streamMetabolizer models

* `parse_ts_path` now accommodates archived ts files even when rownames are 
requested

* re-modernized `modernize_metab_model`

* smarter default filename for `get_config`

# 0.9.6.2

* include CI bounds in metab `estBest` timeseries files

# 0.9.6.1

* date column name update in `modernize_metab_model`

* now looks to ScienceBase for the table of `var_src` codes, units, etc.

* minor documentation updates

# 0.9.6

* new: `try_calc_ts`, which combines `stage_calc_ts` and `post_ts` in a 
fault-tolerant loop that only tries the sites that have the prerequisite data 
and haven't already been posted in the desired time window and format

* new: `delete_sb_files`, which deletes a single file from a single item (in an 
optionally vectorized way). permits corrections of mis-postings even in 
multi-file items such as `metab_model`s and `ts`es

* new: `summarize_ts_files`, which quickly returns a table of ts files, their 
upload dates, and other inventory information

* faster: `repair_ts`, which now runs `locate_ts` not twice but once whenever 
possible

* bugfixes and robustness improvements to `stage_calc_ts` and `ts_has_file`

# 0.9.5

* `config_to_metab_repeat` now works better with Bayesian models and arbitrary 
model 'strategy' fields

* simulation pipeline (data -> DO simulation model -> metabolism fitting model) 
is up to date with recent package changes

* updates for querying ScienceBase with sbtools 0.18.0

* more internal functions seek .Rds timeseries data; external functions 
(`get_ts`, etc.) still seek .tsv by default for backward compatibility

# 0.9.4

* moving toward system-wide use of binary (.Rds) timeseries files rather than 
text (.tsv) - allows greater precision and censoring information in stored data

* now using gO2 and mgO2 for units of oxygen variables

* `stage_nldas_ts` has been updated in response to changes in how NLDAS data are
served

* updates for querying ScienceBase with sbtools 0.16.0

* `stage_calc_ts` for `stage_styx_site` are back up to date

# 0.9.3

* Now automatically checks for available updates when you attach the package

# 0.9.2

* complex requests to `get_ts()` are about 7 times faster now if accompanied by 
update to streamMetabolizer v0.9.3

# 0.9.1

* `read_ts`, `write_ts`, `post_ts`, and `download_ts` now support either .tsv or
.Rds versions of data (Jordan Read)

* `get_ts` is now much smarter about requests for multiple timeseries whose 
temporal resolutions and/or extents are different (e.g., merging daily and 
15-minute data) (Lindsay Carr)

# 0.9.0

## Changes

* file downloads are faster and more internally streamlined (0.8.17)

* `get_metab_model` now posts a modernized model to SB when practical (0.8.16)

* make `get_sites` private (0.8.15)

# 0.8.14

## Status

This package is not ready for use by many, but it does currently have functions 
for:

* downloading and preparing (staging) data from outside websites

* posting staged data to ScienceBase as tab-separated (.tsv) tables with data 
units

* modifying or deleting data on ScienceBase

* synthesizing data for model testing, and posting such data to ScienceBase

* querying and modifying the table of variables and their units, sources, and 
definitions

* querying, locating, downloading, and merging data from ScienceBase

* finding site information including metadata and maps

* creating metabolism model configuration files

* using metabolism model configuration files to build input datasets from 
ScienceBase

* running metabolism models based on configuration files - once per row or many 
times per row - locally or on a cluster

* posting model config files and results to ScienceBase

* retrieving posted models and modernizing to meet current streamMetabolizer 
formatting standards

* summarizing the availability of input data

* summarizing the quality of fitted models (but some of this logic might move to
streamMetabolizer)