data_validator <- function(filename,sheet_choice, progress_updater = NULL) {
  
  today <- Sys.Date()
  # Helper function to update progress if the updater is provided
  update_progress <- function(detail = NULL) {
    if (is.function(progress_updater)) {
      progress_updater(detail = detail)
    }
  }
  
  ## Read in data and do some very basic processing
  update_progress(detail = "Reading data...")
  df <- read_xlsx(filename,
                  sheet = sheet_choice,
                  col_types = "text",
                  na=c("NA","UNK","88888","888888","99999","999999","N/A","Unk"))|> #Unknowns
    arrange(alai_up_uid)|>
    mutate(alai_up_uid=suppressWarnings(as.numeric(alai_up_uid)))|>
    filter(!is.na(alai_up_uid))
  
  
  # Data preprocessing
  update_progress(detail = "Preprocessing data...")
  df <- df|>
    # convert date columns do correct format
    mutate(across(contains("date"),
                  \(x) case_when(
                    is.POSIXct(x) ~ as.Date(x,format = "%Y-%m-%d"),
                    is.Date(x) ~ as.Date(x,format = "%Y-%m-%d"),
                    str_detect(x,"/") ~ as.Date(x,format = "%m/%d/%Y"),
                    str_detect(x,"-") ~ as.Date(x,format = "%Y-%m-%d"),
                    .default = as.Date(as.numeric(x),origin = "1899-12-30") #Excel date origin
                  ))) |>
    # convert some variables to numeric
    mutate(
      across(contains('cd4')&!contains('date'),as.numeric),
      across(contains('bmi'),as.numeric),
      age=as.numeric(age)
    )
  
  # Allow these two columns to be missing entirely, but create them as NA if they
  update_progress(detail = "Handling missing columns...")
  missing_cols <- c("gender_id", "immigration_status_undoc")
  existing_missing <- missing_cols[!missing_cols %in% names(df)]
  
  if (length(existing_missing) > 0) {
    for (col in existing_missing) {
      df[[col]] <- NA_character_
    }
  }
  
  #incorporate a missing data check for key variables
  update_progress(detail = "Checking missing demographics...")
  missing_demographics <- df |>
    select(vital_status_alive,
           zip_code,
           age,
           ethnicity_hispanic,
           contains("race")&!contains("other"),
           sex_birth,
           contains("risk"),
           insurance_status,
           housing_status,
           employment_status,
           poverty_level,
           immigration_status_undoc,
           language,
           incarceration_history,
           contains("active")) |>
    summarize(across(everything(),
                     .fns = list(n_missing = \(x) sum(is.na(x))),
                     .names = "{.col}.{.fn}")) |>
    pivot_longer(cols = everything(),
                 names_to = c("column",".value"),
                 names_pattern = "(.*).(n_missing)")
  
  # Initialize the report
  update_progress(detail = "Running initial validations...")
  report <- data.validator::data_validation_report() # Add validation to the report
  validate(data = df,
           description = "Validation Test") |>
    validate_if(is_uniq(alai_up_uid), description = "ID is unique") |>
    validate_if(!is.na(alai_up_uid) & alai_up_uid != "", description = "ID is not empty") |>
    validate_cols(predicate = in_set(c("0","1")),
                  vital_status_alive,
                  description = "invalid value for vital_status_alive, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("0","1")),
                  ethnicity_hispanic,
                  description = "invalid value for ethnicity_hispanic, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("0","1")),
                  contains('race')&!contains('specify')&!contains("Changes"),
                  description = "invalid value for race variable, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("1","2","3","4","5")),
                  any_of("gender_id"),
                  description = "gender_id") |>
    validate_cols(predicate = in_set(c("1","2","3")),
                  sex_birth,
                  description = "invalid value for sex_birth, should be 1, 2, or 3") |>
    validate_cols(predicate = in_set(c("0","1")),
                  contains('active'),
                  description = "invalid value for active year, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("0","1")),
                  contains('risk'),
                  description = "invalid value for risk factor, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("1","2","3","4","5","6","7","8")),
                  insurance_status,
                  description = "invalid value for insurance_status, should be 1, 2, 3, 4, 5, 6, 7, or  8") |>
    validate_cols(predicate = in_set(c("1","2","3")),
                  housing_status,
                  description = "invalid value for housing_status, should be 1, 2, or 3") |>
    validate_cols(predicate = in_set(c("1","2","3")),
                  employment_status,
                  description = "invalid value for mployment_status, should be 1, 2, or 3") |>
    validate_cols(predicate = in_set(c("1","2","3","4")),
                  poverty_level,
                  description = "invalid value for poverty_level, should be 1, 2, 3, or 4") |>
    validate_cols(predicate = in_set(c("0","1")),
                  any_of("immigration_status_undoc"),
                  description = "invalid value for immigration_status_undoc, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("1","2","3","4","5","6","7","8","20")),
                  language,
                  description = "invalid value for language, should be 1, 2, 3, 4, 5, 6, 7, 8, or 20") |>
    validate_cols(predicate = in_set(c("0","1","2","3")),
                  incarceration_history,
                  description = "invalid value for incarceration_history, should be 0, 1, 2, or 3") |>
    validate_cols(predicate = in_set(c("1","2","3","4","5","6")),
                  contains('vl')&contains('result'),
                  description = "invalid valud for VL result, should be 1, 2, 3, 4, 5, 6") |>
    validate_cols(predicate = in_set(c("0","1")),
                  contains("ever"),
                  description = "invalid value for ever screened or counseled, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("1","2","3")),
                  contains("counsel")&contains("outcome"),
                  description = "invalid value for counsel outcome, should be 1, 2, or 3") |>
    validate_cols(predicate = in_set(c("0","1")),
                  contains("screen")&contains("outcome"),
                  description = "invalid value for screening outcome, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("0","1")),
                  contains('rx')&!contains('date'),
                  description = "invalid value for rx, should be 0 or 1") |>
    validate_cols(predicate = in_set(c("1","2","3","4","5","6")),
                  contains('_dose'),
                  description = "invalid value for Injection [x] dose, should be 1, 2, 3, 4, 5, or 6") |>
    validate_cols(predicate = in_set(c("0","1")),
                  icab_rpv_discontinued,
                  description = "invalid value for icab_rpv_discontinued, should be 0 or 1") |>
    add_results(report = report)
  
  
  #not eligible reasons
  update_progress(detail = "Checking 'not eligible' reasons...")
  not_elig_df <- df |>
    select(alai_up_uid, contains("not_elig")&contains("reason")&!contains("oth")) |>
    mutate(across(contains("not_elig")&contains("reason")&!contains("oth"),
                  \(x) str_split(x,","))) |>
    unnest_wider(col = !alai_up_uid,names_sep = "_") |>
    mutate(across(!alai_up_uid,
                  \(x) case_when(
                    str_trim(x) %in% c("1","2","3","4","5","6","7","8","9","10","20") ~ "OK",
                    is.na(x) ~ "OK",
                    .default = str_extract(cur_column(),"(?<=_)[\\d+]+(?=_)")
                  )))
  
  validate(data = not_elig_df,
           description = "Not eligible reasons check") |>
    validate_cols(predicate = in_set(c("OK")),
                  !alai_up_uid,
                  description = "invalid not eligible reason, should be 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, or 20") |>
    add_results(report = report)
  
  # Disinterest reasons
  update_progress(detail = "Checking 'disinterest' reasons...")
  disinterest_df <- df |>
    select(alai_up_uid, contains("disinterest")&contains("reason")&!contains("oth")) |>
    mutate(across(contains("disinterest")&contains("reason")&!contains("oth"),
                  \(x) str_split(x,","))) |>
    unnest_wider(col = !alai_up_uid, names_sep = "_") |>
    mutate(across(!alai_up_uid,
                  \(x) case_when(
                    str_trim(x) %in% c("1","2","3","4","5","6","20") ~ "OK",
                    is.na(x) ~ "OK",
                    .default = str_extract(cur_column(),"(?<=_)[\\d+]+(?=_)")
                  )))
  
  validate(data = disinterest_df,
           description = "Disinterest reasons check") |>
    validate_cols(predicate = in_set(c("OK")),
                  !alai_up_uid,
                  description = "invalid disinterest reason, should be 1, 2, 3, 4, 5, 6, or 20") |>
    add_results(report = report)
  
  
  # Discontinued reasons
  update_progress(detail = "Checking 'discontinued' reasons...")
  discontinued_df <- df |>
    select(alai_up_uid, contains("discontinued")&contains("reason")&!contains("oth")) |>
    mutate(across(contains("discontinued")&contains("reason")&!contains("oth"),
                  \(x) str_split(x,","))) |>
    unnest_wider(col = !alai_up_uid, names_sep = "_") |>
    mutate(across(!alai_up_uid,
                  \(x) case_when(
                    str_trim(x) %in% c("1","2","3","4","5","6","7","8","9","10","20") ~ "OK",
                    is.na(x) ~ "OK",
                    .default = str_extract(cur_column(),"(?<=_)[\\d+]+(?=_)")
                  )))
  
  validate(data = discontinued_df,
           description = "Discontinued reasons check") |>
    validate_cols(predicate = in_set(c("OK")),
                  !alai_up_uid,
                  description = "invalid discontinued reason, should be 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, or 20") |>
    add_results(report = report)
  
  # Viral load data
  update_progress(detail = "Processing viral load data...")
  vl_long <- df|>
    select(alai_up_uid,contains("hiv_vl")&!contains("dx")&!contains("pre_icab"))|>
    pivot_longer(cols=contains("hiv_vl"),
                 names_to=c("index",".value"),
                 names_pattern = "hiv_vl_(.*)_(date|result)",
    )|>
    mutate(date=as.Date(date),
           index = as.numeric(index))|>
    group_by(alai_up_uid)|>
    arrange(index)|>
    mutate(vl_sequenced=case_when(
      date>lag(date)~1,
      date<lag(date)~0,
    ))|>
    mutate(
      vl_violation=case_when(
        vl_sequenced==0~paste(alai_up_uid,':',lag(date),date,lead(date)),
        .default='OK'
      ),
      missing_vl_result = case_when(
        is.na(result) & !is.na(date) ~ paste(alai_up_uid,": VL",index,",",date),
        .default = "OK"
      ),
      date_in_future = case_when(
        date > today ~ paste(alai_up_uid,": VL",index,",",date),
        .default = "OK"
      )
    )
  
  vl_summary <- vl_long |>
    group_by(alai_up_uid) |>
    summarize(
      any_vl_violation = if_else(any(vl_violation != "OK"),
                                 paste(paste(index[vl_violation != "OK"],collapse = ", ")),
                                 "0"),
      any_missing_vl_result = if_else(any(missing_vl_result != "OK"),
                                      paste(paste(index[missing_vl_result != "OK"],collapse = ", ")),
                                      "0"),
      any_vl_in_future = if_else(any(date_in_future != "OK"),
                                 paste(paste(index[date_in_future != "OK"],collapse = ", ")),
                                 "0"))
  
  update_progress(detail = "Validating viral load data...")
  validate(data = vl_summary, description = "VL issues") |>
    validate_cols(predicate = in_set(c("0")),
                  any_vl_violation,
                  description = "VL not in sequential order. Please re-order VL dates and results from oldest to newest") |>
    validate_cols(predicate = in_set(c("0")),
                  any_missing_vl_result,
                  description = "Viral load result missing. Please enter VL result or delete VL date") |>
    validate_cols(predicate = in_set(c("0")),
                  any_vl_in_future,
                  description = "Viral load date is in the future") |>
    add_results(report = report)
  
  # Injection data
  update_progress(detail = "Processing injection data...")
  shot_long <- df|>
    select(alai_up_uid,contains("icab_rpv_shot") & (contains("date") | contains("dose")),
           contains("pre_icab"))|>
    pivot_longer(cols=contains("icab_rpv_shot")& (contains("date") | contains("dose")),
                 names_to=c("index",".value"),
                 names_pattern = "icab_rpv_shot(\\d+)_(date|dose)",
    )|>
    mutate(index = as.numeric(index),
           date=as.Date(date))|>
    group_by(alai_up_uid)|>
    arrange(alai_up_uid,index)|>
    mutate(shot_sequenced=case_when(
      date>lag(date)~1,
      date<=lag(date)~0,
    ))|>
    mutate(
      ever_on_cab = if_else(any(!is.na(date)),1,0),
      shot_violation=case_when(
        ever_on_cab == 1 & shot_sequenced==0~paste(alai_up_uid,':',lag(date),date,lead(date)),
        .default='OK'
      ),
      missing_dose = case_when(
        ever_on_cab == 1 & is.na(dose)& !is.na(date) ~ paste(alai_up_uid,": shot",index,",",date),
        .default = "OK"
      ),
      missing_first_dose = case_when(
        ever_on_cab == 1 & any(index == 1 & is.na(date) & is.na(dose)) ~ paste(alai_up_uid),
        .default = "OK"
      ),
      missing_pre_icab_vl = case_when(
        ever_on_cab == 1 & any(is.na(hiv_vl_pre_icab_result)) ~ paste(alai_up_uid),
        .default = "OK"
      ),
      interval = as.numeric(difftime(date,lag(date),units = "days")),
      late = case_when(dose %in% c(4,5,6) ~ FALSE,
                       lag(index) == 1 & lag(dose) != 6 ~ interval > 31+7,
                       lag(dose) %in% c(1,3,4,5) ~ interval > 31+7,
                       lag(dose) %in% c(2,6) ~ interval > 62+7,
                       is.na(lag(dose)) ~ interval > 62 + 7,
                       .default = FALSE),
      early = case_when(dose %in% c(4,5,6) ~ FALSE,
                        lag(index) == 1 & lag(dose) != 6 ~ interval < 31-7,
                        lag(dose) %in% c(1,3,4,5) ~ interval < 31-7,
                        lag(dose) %in% c(2,6) ~ interval < 62-7,
                        is.na(lag(dose)) ~ interval < 31 - 7,
                        .default = FALSE),
      date_in_future = case_when(
        date > today ~ paste(alai_up_uid,": shot",index,",",date),
        .default = "OK"
      ),
      early_date = case_when(
        date < as.Date("2021-01-22") ~ paste(alai_up_uid,": shot",index,",",date),
        .default = "OK"
      ),
      alai_up_uid=as.numeric(alai_up_uid)
    )
  
  
  shot_summary <- shot_long |>
    group_by(alai_up_uid) |>
    summarize(
      any_shot_violation = if_else(any(shot_violation != "OK"),
                                   paste(paste(index[shot_violation != "OK"],collapse = ", ")),
                                   "0"),
      any_missing_dose = if_else(any(missing_dose != "OK"),
                                 paste(paste(index[missing_dose != "OK"],collapse = ", ")),
                                 "0"),
      any_missing_first_dose = if_else(any(missing_first_dose != "OK"),
                                       "1","0"),
      missing_pre_icab_vl = if_else(any(missing_pre_icab_vl != "OK"),
                                    paste(unique(alai_up_uid)),
                                    "0"),
      any_late_dose = if_else(any(late == TRUE),
                              paste(paste(index[late == TRUE & !is.na(late)],collapse = ", ")),
                              "0"),
      any_early_dose = if_else(any(early == TRUE),
                               paste(paste(index[early == TRUE & !is.na(early)],collapse = ", ")),
                               "0"),
      any_shot_in_future = if_else(any(date_in_future != "OK"),
                                   paste(paste(index[date_in_future != "OK"],collapse = ", ")),
                                   "0"),
      any_early_date = if_else(any(early_date != "OK"),
                               paste(paste(index[early_date != "OK"],collapse = ", ")),
                               "0")
    )
  
  update_progress(detail = "Validating injection data...")
  validate(data = shot_summary, description = "Shot issues") |>
    validate_cols(predicate = in_set(c("0")),
                  any_shot_violation,
                  description = "iCAB/RPV shot dates are not in sequential order. Please re-order shot dates and related data (dose, needle length, BMI) from oldest to newest") |>
    validate_cols(predicate = in_set(c("0")),
                  any_missing_dose,
                  description = "iCAB/RPV dose is missing. Please enter missing iCAB/RPV dose. If no shot was delivered, then delete shot date.") |>
    validate_cols(predicate = in_set(c("0")),
                  any_missing_first_dose,
                  description = "Patient has information for later iCAB/RPV shot doses (e.g., shot2 or shot3), but is missing the date of first dose. Please enter date for first dose or shift shot information to remove this gap. If first shot was delivered at a different clinic, please use the special response options for dose to indicate this. Don’t leave shot1 information blank.") |>
    validate_cols(predicate = in_set(c("0")),
                  missing_pre_icab_vl,
                  description = "Pre-iCAB/RPV VL is missing. Please enter the client's most recent viral load test on or before first iCAB/RPV injection.") |>
    validate_cols(predicate = in_set(c("0")),
                  any_late_dose,
                  description = "iCAB/RPV dose is late. Check to ensure dose is actually late. Check to ensure that the correct dose is listed (bi-monthly vs. monthly dose). Check to ensure previous shot data is not missing.") |>
    validate_cols(predicate = in_set(c("0")),
                  any_early_dose,
                  description = "iCAB/RPV dose is early. Check to ensure dose is actually early. Check to ensure that the correct dose is listed (monthly vs. bi-monthly dose).") |>
    validate_cols(predicate = in_set(c("0")),
                  any_shot_in_future,
                  description = "Warning: iCAB/RPV shot date is in future. Check to ensure that the shot date is correct.") |>
    validate_cols(predicate = in_set(c("0")),
                  any_early_date,
                  description = "Warning: iCAB/RPV shot date is before January 22, 2021 (before iCAB/RPV was FDA approved)  Check to ensure that the shot date is correct.") |>
    add_results(report = report)
  
  
  
  ## iCAB-related events
  update_progress(detail = "Processing iCAB-related events...")
  icab_events_long <- df|>
    filter(!is.na(alai_up_uid))|>
    select(alai_up_uid,contains("counsel"),contains("screen"),contains("rx"),
           icab_rpv_shot1_date,icab_rpv_shot1_dose,
           icab_rpv_discontinued_date,icab_rpv_discontinued)|>
    mutate(across(everything()&!alai_up_uid,as.character))|>
    pivot_longer(
      cols = (contains("counsel")|contains("screen"))&!contains("ever")&!contains("reason"),
      names_to = c("event", "index",".value"),
      names_pattern = "icab_rpv_(.+)_(\\d+)_(outcome|date)",
      values_drop_na = FALSE)
  
  # get just the events before the first injection
  icab_events_before_shot1 <- icab_events_long |>
    filter(
      is.na(icab_rpv_shot1_date) | date <= icab_rpv_shot1_date | is.na(date)
    ) |>
    #no counseling/screening after initiation is kept
    #but those events can still happen between the time of prescription and initiation
    mutate(
      prescribe_after_counsel = case_when(
        icab_rpv_rx == 1 &
          event == "counsel" &
          date <= icab_rpv_rx_date &
          !is.na(date) &
          !is.na(icab_rpv_rx_date) ~ 1,
        icab_rpv_rx == 1 &
          event == "counsel" &
          date > icab_rpv_rx_date &
          !is.na(date) &
          !is.na(icab_rpv_rx_date) ~ 0,
        (icab_rpv_rx == 0 | is.na(icab_rpv_rx)) & is.na(icab_rpv_rx_date) ~ 1,
        .default = NA
      ),
      prescribe_after_screen = case_when(
        icab_rpv_rx == 1 &
          event == "screen" &
          date <= icab_rpv_rx_date &
          !is.na(date) &
          !is.na(icab_rpv_rx_date) ~ 1,
        icab_rpv_rx == 1 &
          event == "screen" &
          date > icab_rpv_rx_date &
          !is.na(date) &
          !is.na(icab_rpv_rx_date) ~ 0,
        (icab_rpv_rx == 0 | is.na(icab_rpv_rx)) & is.na(icab_rpv_rx_date) ~ 1,
        .default = NA
      ),
      rx_interested = case_when(
        icab_rpv_rx == 1 & event == "counsel" & outcome == 3 ~ 1,
        icab_rpv_rx == 1 & event == "counsel" & outcome != 3 ~ 0,
        .default = NA
      ),
      rx_eligible = case_when(
        icab_rpv_rx == 1 & event == "screen" & outcome == 1 ~ 1,
        icab_rpv_rx == 1 & event == "screen" & outcome != 1 ~ 0,
        .default = NA
      )
    )
  
  
  #group by alai_up_uid AND arrange for first and last events before shot1
  icab_events <- icab_events_before_shot1|>
    group_by(alai_up_uid)|>
    arrange(date)|>
    summarize(
      ever_counselled_VALID=case_when(
        all(icab_rpv_counsel_ever==1)&any(event=="counsel" & (!is.na(date) | !is.na(outcome)))~1,
        all(icab_rpv_counsel_ever==0)&!any(event=="counsel" & !is.na(date) &!is.na(outcome))~1,
        all(is.na(icab_rpv_counsel_ever))&!any(event=="counsel"& !is.na(date) &!is.na(outcome))~1,
        .default=unique(alai_up_uid)
      ),
      ever_screened_VALID=case_when(
        all(icab_rpv_screen_ever==1)&any(event=="screen" & (!is.na(date) | !is.na(outcome)))~1,
        all(icab_rpv_screen_ever==0)&!any(event=="screen" & !is.na(date) &!is.na(outcome))~1,
        all(is.na(icab_rpv_screen_ever))&!any(event=="screen" & !is.na(date) &!is.na(outcome))~1,
        .default=unique(alai_up_uid)
      ),
      prescribe_NOT_interested=case_when(
        !is.na(last(rx_interested))&
          last(rx_interested)==0~max(as.numeric(index[rx_interested == 0]),na.rm = T),
        is.na(last(rx_interested))~0,
        .default=0),
      prescribe_NOT_eligible=case_when(
        !is.na(last(rx_eligible))&
          last(rx_eligible)==0~max(as.numeric(index[rx_eligible == 0]),na.rm = T),
        is.na(last(rx_eligible))~0,
        .default=0),
      prescribe_BEFORE_screen=if_else(any(!is.na(prescribe_after_screen))&
                                        all(prescribe_after_screen==0),
                                      max(as.numeric(index[prescribe_after_screen==0]),na.rm = T),0),
      prescribe_BEFORE_counsel=if_else(any(!is.na(prescribe_after_counsel))&
                                         all(prescribe_after_counsel==0),
                                       max(as.numeric(index[prescribe_after_counsel==0]),na.rm = T),0),
      # do we care about first screening valid?
      first_counsel_valid = case_when(
        !any(event=="counsel" & !is.na(date) &!is.na(outcome)) ~ 1,
        any(event=="counsel" & index==1 & (!is.na(date) |!is.na(outcome))) ~ 1,
        .default = unique(alai_up_uid)
      ),
      first_screen_valid=case_when(
        !any(event=="screen" & !is.na(date) &!is.na(outcome)) ~ 1,
        any(event=="screen" & index==1 & (!is.na(date) |!is.na(outcome))) ~ 1,
        .default = unique(alai_up_uid)
      ),
      counsel_extreme_date = case_when(
        any(event == "counsel" & date > today & !is.na(date)) ~
          paste(paste(index[event == "counsel" & date > today & !is.na(date)],collapse = ", ")),
        any(event == "counsel" & date < as.Date("2021-01-22") & !is.na(date)) ~
          paste(paste(index[event == "counsel" & date < as.Date("2021-01-22") & !is.na(date)],collapse = ", ")),
        .default = "0"
      ),
      screen_extreme_date = case_when(
        any(event == "screen" & date > today & !is.na(date)) ~
          paste(paste(index[event == "screen" & date > today & !is.na(date)],collapse = ", ")),
        any(event == "screen" & date < as.Date("2021-01-22") & !is.na(date)) ~
          paste(paste(index[event == "screen" & date < as.Date("2021-01-22") & !is.na(date)],collapse = ", ")),
        .default = "0"
      ),
      rx_extreme_date = case_when(
        any(icab_rpv_rx_date > today & !is.na(icab_rpv_rx_date)) ~ unique(alai_up_uid),
        any(icab_rpv_rx_date < as.Date("2021-01-22") & !is.na(icab_rpv_rx_date)) ~ unique(alai_up_uid),
        .default = 0
      ),
      discontinued_extreme_date = case_when(
        any(icab_rpv_discontinued_date > today & !is.na(icab_rpv_discontinued_date)) ~ unique(alai_up_uid),
        any(icab_rpv_discontinued_date < as.Date("2021-01-22") & !is.na(icab_rpv_discontinued_date)) ~ unique(alai_up_uid),
        .default = 0
      )
    )
  
  ##summarization level
  icab_2 <- df|>
    mutate(
      discontinue_valid=case_when(
        icab_rpv_discontinued==1 & !is.na(icab_rpv_discontinued_date)& !is.na(icab_rpv_rx_date)&
          icab_rpv_discontinued_date>=icab_rpv_rx_date~1,
        icab_rpv_discontinued==1 & is.na(icab_rpv_discontinued_date)~1,
        icab_rpv_discontinued==0 & (!is.na(icab_rpv_rx_date) | !is.na(icab_rpv_shot1_date) |!is.na(icab_rpv_shot2_date)) ~1,
        is.na(icab_rpv_discontinued)~1,
        .default = alai_up_uid
      ),
      initiate_after_rx=case_when(
        icab_rpv_rx==1 & !is.na(icab_rpv_shot1_date) &
          icab_rpv_shot1_date>=icab_rpv_rx_date~1,
        icab_rpv_rx==1 & is.na(icab_rpv_shot1_date) ~1, #prescribed but not yet started
        icab_rpv_rx==1 & is.na(icab_rpv_rx_date) & !is.na(icab_rpv_shot1_date) ~1,# missing rx date but has dose OK
        (icab_rpv_rx==0|is.na(icab_rpv_rx))  & is.na(icab_rpv_rx_date) ~1,
        .default = alai_up_uid
      ),
    )|>
    select(alai_up_uid,discontinue_valid,initiate_after_rx)
  
  icab_events_whole=merge(icab_events,icab_2,by="alai_up_uid") |>
    distinct()
  
  update_progress(detail = "Validating iCAB logic...")
  validate(data = icab_events_whole, description = "iCAB logic check") |>
    validate_cols(predicate = in_set(c(1)),
                  ever_counselled_VALID,
                  description = "Counsel_ever is not correctly documented. If “icab_rpv_counsel_ever=1”, then there should be a counseling date and outcome. If “icab_rpv_counsel_ever=0, then there should be no counseling date or outcome.") |>
    validate_cols(predicate = in_set(c(1)),
                  ever_screened_VALID,
                  description = "Screen_ever is not correctly documented. If “icab_rpv_screen_ever=1”, then there should be a screening date and outcome. If “icab_rpv_screen_ever=0, then there should be no screen date or outcome.") |>
    validate_cols(predicate = in_set(c(1)),
                  first_counsel_valid,
                  description = "Missing iCAB/RPV first counseling date. Patient has information for later counseling dates (e.g., counseling 2), but is missing the date or outcome of the first counseling event. Please enter information for first counseling event, or shift counseling information to remove this gap.") |>
    validate_cols(predicate = in_set(c(1)),
                  first_screen_valid,
                  description = "Missing iCAB/RPV first screening date. Patient has information for later screening dates (e.g., screening 2), but is missing the date or outcome of the first screening event. Please enter information for first screening event, or shift screening information to remove this gap.") |>
    validate_cols(predicate = in_set(c(0)),
                  prescribe_BEFORE_counsel,
                  description = "iCAB/RPV prescription date is before the counseling date. Please check to ensure dates are correct.") |>
    validate_cols(predicate = in_set(c(0)),
                  prescribe_BEFORE_screen,
                  description = "iCAB/RPV prescription date is before the screening date. Please check to ensure dates are correct.") |>
    validate_cols(predicate = in_set(c(1)),
                  initiate_after_rx,
                  description = "Date of iCAB/RPV initiation (shot 1 date) is before iCAB/RPV prescription date. Please check to ensure dates are correct.") |>
    validate_cols(predicate = in_set(c(0)),
                  prescribe_NOT_eligible,
                  description = "Patient has been prescribed iCAB/RPV but screening outcome is 0 (not eligible) or missing. Please check to ensure all data are complete and correct. Possible allowable reason: Patient may have been viremic and not eligible but still prescribed iCAB/RPV.") |>
    validate_cols(predicate = in_set(c(0)),
                  prescribe_NOT_interested,
                  description = "Patient has been prescribed iCAB/RPV but last counsel outcome is 1, 2, or missing, meaning patient is not interested or not sure. Please check to ensure all data are complete and correct. Is there a missing counseling event when patient decided they were interested?") |>
    validate_cols(predicate = in_set(c(1)),
                  discontinue_valid,
                  description = "iCAB/RPV discontinued date is before first iCAB/RPV shot date. Please check to ensure dates are correct.") |>
    validate_cols(predicate = in_set(c("0")),
                  counsel_extreme_date,
                  description = "Warning: iCAB/RPV counsel date is before January 22, 2021 (before iCAB/RPV was FDA approved) or after today’s date. Check to ensure that the counsel date is correct.") |>
    validate_cols(predicate = in_set(c("0")),
                  screen_extreme_date,
                  description = "Warning: iCAB/RPV screening date is before January 22, 2021 (before iCAB/RPV was FDA approved) or after today’s date. Check to ensure that the screening date is correct.") |>
    validate_cols(predicate = in_set(c(0)),
                  rx_extreme_date,
                  description = "Warning: iCAB/RPV prescription date is before January 22, 2021 (before iCAB/RPV was FDA approved) or after today’s date. Check to ensure that the prescription date is correct.") |>
    validate_cols(predicate = in_set(c(0)),
                  discontinued_extreme_date,
                  description = "Warning: iCAB/RPV discontinuation date is before January 22, 2021 (before iCAB/RPV was FDA approved) or after today’s date. Check to ensure that the discontinuation date is correct.") |>
    add_results(report = report)
  
  # create the final report, edit excel instructions
  update_progress(detail = "Generating final report...")
  final_report <- get_results(report, unnest = T) |>
    filter(type == "error") |>
    left_join(df |>
                mutate(index = row_number()) |>
                select(index,alai_up_uid)) |>
    mutate(value = str_split(value,", ")) |>
    unnest(value) |>
    mutate(column_new = case_when(
      table_name == "df" & description == "ID is unique" ~ "alai_up_uid",
      table_name == "df"  ~  column,
      table_name == "not_elig_df" ~ str_c("icab_rpv_not_elig_",value,"_reason"),
      table_name == "disinterest_df" ~ str_c("icab_rpv_disinterest_reason_",value),
      table_name == "discontinued_df" ~ "icab_rpv_discontinued_reason",
      table_name == "vl_summary" & column == "any_vl_violation" ~ str_c("hiv_vl_",as.numeric(value)-1,"_date, hiv_vl_",value,"_date"),
      table_name == "vl_summary" & column == "any_missing_vl_result" ~ str_c("hiv_vl_",value,"_result"),
      table_name == "vl_summary" & column == "any_vl_in_future" ~ str_c("hiv_vl_",value,"_date"),
      table_name == "shot_summary" & column == "any_shot_violation" ~ str_c("icab_rpv_shot",as.numeric(value)-1,"_date, icab_rpv_shot",value,"_date"),
      table_name == "shot_summary" & column == "any_missing_dose" ~ str_c("icab_rpv_shot",value,"_dose"),
      table_name == "shot_summary" & column == "any_missing_first_dose" ~ str_c("icab_rpv_shot",value,"_date"),
      table_name == "shot_summary" & column == "missing_pre_icab_vl" ~ "hiv_vl_pre_icab_result",
      table_name == "shot_summary" & column == "any_late_dose" ~ str_c("icab_rpv_shot",as.numeric(value)-1,"_date, icab_rpv_shot",value,"_date"),
      table_name == "shot_summary" & column == "any_early_dose" ~ str_c("icab_rpv_shot",as.numeric(value)-1,"_date, icab_rpv_shot",value,"_date"),
      table_name == "shot_summary" & column == "any_shot_in_future" ~ str_c("icab_rpv_shot",value,"_date"),
      table_name == "shot_summary" & column == "any_early_date" ~ str_c("icab_rpv_shot",value,"_date"),
      table_name == "icab_events_whole" & column == "ever_counselled_VALID" ~ "icab_rpv_counsel_ever",
      table_name == "icab_events_whole" & column == "ever_screened_VALID" ~ "icab_rpv_screen_ever",
      table_name == "icab_events_whole" & column == "prescribe_NOT_interested" ~ str_c("icab_rpv_rx, icab_rpv_counsel_",value,"_outcome"),
      table_name == "icab_events_whole" & column == "prescribe_NOT_eligible" ~ str_c("icab_rpv_rx, icab_rpv_screen_",value,"_outcome"),
      table_name == "icab_events_whole" & column == "prescribe_BEFORE_screen" ~ str_c("icab_rpv_rx_date, icab_rpv_screen_",value,"_outcome"),
      table_name == "icab_events_whole" & column == "prescribe_BEFORE_counsel" ~ str_c("icab_rpv_rx_date, icab_rpv_counsel_",value,"_date"),
      table_name == "icab_events_whole" & column == "first_counsel_valid" ~ "icab_rpv_counsel_1_date, icab_rpv_counsel_1_outcome",
      table_name == "icab_events_whole" & column == "first_screen_valid" ~ "icab_rpv_screen_1_date, icab_rpv_screen_1_outcome",
      table_name == "icab_events_whole" & column == "counsel_extreme_date" ~ str_c("icab_rpv_counsel_",value,"_date"),
      table_name == "icab_events_whole" & column == "screen_extreme_date" ~ str_c("icab_rpv_screen_",value,"_date"),
      table_name == "icab_events_whole" & column == "rx_extreme_date" ~ "icab_rpv_rx_date",
      table_name == "icab_events_whole" & column == "discontinued_extreme_date" ~ "icab_rpv_discontinued_date",
      table_name == "icab_events_whole" & column == "discontinue_valid" ~ "icab_rpv_discontinued",
      table_name == "icab_events_whole" & column == "initiate_after_rx" ~ "icab_rpv_rx_date, icab_rpv_shot1_date"
    )) |>
    select(alai_up_uid,description,column_new) |>
    separate(column_new,sep = ", ",
             into = c("col1","col2"))
  
  final_report$val1 <- NA_character_
  final_report$val2 <- NA_character_
  
  update_progress(detail = "Fetching values for report...")
  for (i in 1:nrow(final_report)){
    final_report$val1[i] = as.character(pull(df[df$alai_up_uid == final_report$alai_up_uid[i],
                                                final_report$col1[i]]))
    if (!is.na(final_report$col2[i])){
      final_report$val2[i] = as.character(pull(df[df$alai_up_uid == final_report$alai_up_uid[i],
                                                  final_report$col2[i]]))
    } else {
      final_report$val2[i] <- NA_character_
    }
    
  }
  
  final_report <- final_report |>
    select(alai_up_uid, description, col1, val1, col2, val2) |>
    rename(column1 = col1,
           value1 = val1,
           column2 = col2,
           value2 = val2)
  
  update_progress(detail = "Done.")
  return(list(final_report = final_report,
              missing_demographics = missing_demographics))
  
}

# write_xlsx(list(final_report = final_report,
#                 missing_demographics = missing_demographics),
#            path = paste0(dirname(filename), "/data_validation_report_",today,".xlsx"))

# print("Done")
