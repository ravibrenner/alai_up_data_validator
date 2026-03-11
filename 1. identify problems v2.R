if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 
p_load(tidyverse,readxl,data.validator,assertr,skimr,svDialogs,writexl)

# File selection
filename <- dlg_open(title = "Select your Excel file")$res

if (filename == "") {
  stop("No file selected.")
}

# sheet selection
sheet_names <- excel_sheets(filename)

sheet_choice <- dlg_list(sheet_names, title = "Choose a sheet")$res

if (sheet_choice == "") {
  stop("No sheet selected.")
}

today <- Sys.Date()

## Read in data and do some very basic processing
df <- read_xlsx(filename,
                sheet = sheet_choice,
                col_types = "text", 
                na=c("NA","UNK","88888","888888","99999","999999","N/A","Unk"))|> #Unknowns
  arrange(alai_up_uid)|> 
  mutate(alai_up_uid=suppressWarnings(as.numeric(alai_up_uid)))|>
  filter(!is.na(alai_up_uid)) 
  

# Data preprocessing
df <- df|>
  # convert date columns do correct format
  mutate(across(contains("date"),
                \(x) case_when(
                  is.POSIXct(x) ~ as.Date(x,format = "%Y-%m-%d"),
                  is.Date(x) ~ as.Date(x,format = "%Y-%m-%d"),
                  str_detect(x,"/") ~ as.Date(x,format = "%m/%d/%Y"),
                  .default = as.Date(as.numeric(x),origin = "1899-12-30")
                ))) |>
  # convert some variables to numeric
  mutate(
    across(contains('cd4')&!contains('date'),as.numeric),
    across(contains('bmi'),as.numeric),
    age=as.numeric(age)
  ) |>
  # get correct birth year
  mutate(
    birth_year=case_when(
      birth_year > 2025 ~ 2024 - as.numeric(age),
      .default = as.numeric(birth_year)
    )
  )

#incorporate a missing data check for key variables
missing_demographics <- df |>
  select(vital_status_alive,
         ethnicity_hispanic,
         contains("race")&!contains("other"),
         sex_birth,
         insurance_status,
         housing_status,
         contains("active")) |>
  summarize(across(everything(),
                   .fns = list(n_missing = \(x) sum(is.na(x))),
                   .names = "{.col}.{.fn}")) |>
  pivot_longer(cols = everything(),
               names_to = c("column",".value"),
               names_pattern = "(.*).(n_missing)")


# Initialize the report
report <- data_validation_report() # Add validation to the report 
validate(data = df,
         description = "Validation Test") %>%
  validate_if(is_uniq(alai_up_uid), description = "ID is unique") %>%
  validate_if(!is.na(alai_up_uid) & alai_up_uid != "", description = "ID is not empty") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                vital_status_alive,
                description = "vital_status_alive") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                ethnicity_hispanic, 
                description = "ethnicity_hispanic") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains('race')&!contains('specify')&!contains("Changes"),
                description = "race") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5")),
                any_of("gender_id"),
                description = "gender_id") %>%
  validate_cols(predicate = in_set(c("1","2","3")), 
                sex_birth, 
                description = "sex_birth") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains('active'), 
                description = "active") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains('risk'),
                description = "risk") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5","6","7","8")), 
                insurance_status, 
                description = "insurance_status") %>%
  validate_cols(predicate = in_set(c("1","2","3")), 
                housing_status, 
                description = "housing_status") %>%
  validate_cols(predicate = in_set(c("1","2","3")), 
                employment_status, 
                description = "employment_status") %>%
  validate_cols(predicate = in_set(c("1","2","3","4")), 
                poverty_level, 
                description = "poverty_level") %>%
#  validate_cols(predicate = in_set(c("0","1")), 
#                immigration_status_undoc, 
#                description = "immigration_status_undoc") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5","6","7","8","20")), 
                language, 
                description = "language") %>%
  validate_cols(predicate = in_set(c("0","1","2","3")), 
                incarceration_history, 
                description = "incarceration_history") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains('hx')&!contains('date'),
                description = "hx") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains('mhservices'),
                description = "mhservices") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                hiv_status_pos, 
                description = "hiv_status_pos") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5","6")), 
                contains('vl')&contains('result'), 
                description = "vl results") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                current_on_art, 
                description = "current_on_art") %>%
  validate_cols(predicate = in_set(c("0","1","2","20")), 
                contains("art")&contains("type"), 
                description = "art_regimen_[x]_type") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5","6","7","8","9","20")), 
                contains("art")&!contains("type")&!contains("other")&!contains("date")&!contains("current"), 
                description = "art_regimen_[x]") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains("_mutations"), 
                description = "cab_rpv_mutations") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains("ever"), 
                description = "ever screened or counseled") %>%
  validate_cols(predicate = in_set(c("1","2","3")), 
                contains("counsel")&contains("outcome"), 
                description = "counsel outcome") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5","6","20")), 
                contains("disinterest")&!contains("oth"), 
                description = "disinterest reason") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains("screen")&contains("outcome"), 
                description = "screening outcome") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains('rx')&!contains('date'),
                description = "rx") %>%
  validate_cols(predicate = in_set(c("0","1","NA")), 
                icab_rpv_insurance_denial, 
                description = "icab_rpv_insurance_denial") %>%
  validate_cols(predicate = in_set(c("0","1","NA")), 
                contains("prior_auth"), 
                description = "prior_auth") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5","6","7","8","20")), 
                icab_rpv_payor, 
                description = "icab_rpv_payor") %>%
  validate_cols(predicate = in_set(c("1","2","3")), 
                icab_rpv_insurance_benefit_type, 
                description = "icab_rpv_insurance_benefit_type") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                contains('oral')&!contains("date"),
                description = "oral bridge or lead in") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5","6","7")), 
                contains('_dose'),
                description = "Injection [x] dose") %>%
  validate_cols(predicate = in_set(c("1","2")), 
                contains('needle_length'),
                description = "shot[x]_needle_length") %>%
  validate_cols(predicate = in_set(c("0","1")), 
                icab_rpv_discontinued,
                description = "icab_rpv_discontinued") %>%
  validate_cols(predicate = in_set(c("1","2","3","4","5","6","7","8","9","10","20")), 
                icab_rpv_discontinued_reason, 
                description = "icab_rpv_discontinued_reason") %>%
  add_results(report = report)


#not eligible reasons
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

# Viral load data
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

validate(data = vl_summary, description = "VL issues") |>
  validate_cols(predicate = in_set(c("0")),
                any_vl_violation,
                description = "non-sequential VL dates") |>
  validate_cols(predicate = in_set(c("0")),
                any_missing_vl_result,
                description = "missing VL result") |>
  validate_cols(predicate = in_set(c("0")),
                any_vl_in_future,
                description = "VL in future") |>
  add_results(report = report)

# Injection data
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
    late = case_when(dose %in% c(3,4,5,6) ~ FALSE,
                     lag(index) == 1 & lag(dose) != 6 ~ interval > 31+7,
                     lag(dose) %in% c(1,3,4,5) ~ interval > 31+7,
                     lag(dose) %in% c(2,6) ~ interval > 62+7,
                     is.na(lag(dose)) ~ interval > 62 + 7,
                     .default = FALSE),
    early = case_when(dose %in% c(3,4,5,6) ~ FALSE,
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
      date < as.Date("2022-01-01") ~ paste(alai_up_uid,": shot",index,",",date),
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

validate(data = shot_summary, description = "Shot issues") |>
  validate_cols(predicate = in_set(c("0")),
                any_shot_violation,
                description = "non-sequential shot dates") |>
  validate_cols(predicate = in_set(c("0")),
                any_missing_dose,
                description = "missing dose code") |>
  validate_cols(predicate = in_set(c("0")),
                any_missing_first_dose,
                description = "missing first dose date") |>
  validate_cols(predicate = in_set(c("0")),
                missing_pre_icab_vl,
                description = "missing pre-icab VL") |>
  validate_cols(predicate = in_set(c("0")),
                any_late_dose,
                description = "Check to ensure dose is actually late") |>
  validate_cols(predicate = in_set(c("0")),
                any_early_dose,
                description = "Check to ensure dose is actually early") |>
  validate_cols(predicate = in_set(c("0")),
                any_shot_in_future,
                description = "warning: shot in future") |>
  validate_cols(predicate = in_set(c("0")),
                any_early_date,
                description = "warning: check early dose date") |>
  add_results(report = report)



## iCAB-related events
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
icab_events_before_shot1 <- icab_events_long|>
  filter( is.na(icab_rpv_shot1_date) |date<=icab_rpv_shot1_date |is.na(date)
  )|> 
  #no counseling/screening after initiation is kept
  #but those events can still happen between the time of prescription and initiation
  mutate(
    prescribe_after_counsel=case_when(
      icab_rpv_rx==1 & event=="counsel" & date<=icab_rpv_rx_date &
        !is.na(date) & !is.na(icab_rpv_rx_date) ~ 1,
      icab_rpv_rx==1 & event=="counsel" & date>icab_rpv_rx_date &
        !is.na(date) & !is.na(icab_rpv_rx_date) ~ 0,
      (icab_rpv_rx==0 | is.na(icab_rpv_rx)) & is.na(icab_rpv_rx_date) ~ 1,
      .default = NA
    ),
    prescribe_after_screen=case_when(
      icab_rpv_rx==1 & event=="screen" & date<=icab_rpv_rx_date &
        !is.na(date) & !is.na(icab_rpv_rx_date) ~ 1,
      icab_rpv_rx==1 & event=="screen" & date>icab_rpv_rx_date &
        !is.na(date) & !is.na(icab_rpv_rx_date) ~ 0,
      (icab_rpv_rx==0 | is.na(icab_rpv_rx)) & is.na(icab_rpv_rx_date) ~ 1,
      .default = NA
    ),
    rx_interested=case_when(
      icab_rpv_rx==1 & event=="counsel" & outcome==3 ~1,
      icab_rpv_rx==1 & event=="counsel" & outcome!=3 ~0,
      .default = NA
    ),
    rx_eligible=case_when(
      icab_rpv_rx==1 & event=="screen" & outcome==1 ~1,
      icab_rpv_rx==1 & event=="screen" & outcome!=1 ~0,
      .default = NA
    ))


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
      any(event == "counsel" & date < as.Date("2022-01-01") & !is.na(date)) ~ 
        paste(paste(index[event == "counsel" & date < as.Date("2022-01-01") & !is.na(date)],collapse = ", ")),
      .default = "0"
    ),
    screen_extreme_date = case_when(
      any(event == "screen" & date > today & !is.na(date)) ~  
        paste(paste(index[event == "screen" & date > today & !is.na(date)],collapse = ", ")),
      any(event == "screen" & date < as.Date("2022-01-01") & !is.na(date)) ~ 
        paste(paste(index[event == "screen" & date < as.Date("2022-01-01") & !is.na(date)],collapse = ", ")),
      .default = "0"
    ),
    rx_extreme_date = case_when(
      any(icab_rpv_rx_date > today & !is.na(icab_rpv_rx_date)) ~ unique(alai_up_uid),
      any(icab_rpv_rx_date < as.Date("2022-01-01") & !is.na(icab_rpv_rx_date)) ~ unique(alai_up_uid),
      .default = 0
    ),
    discontinued_extreme_date = case_when(
      any(icab_rpv_discontinued_date > today & !is.na(icab_rpv_discontinued_date)) ~ unique(alai_up_uid),
      any(icab_rpv_discontinued_date < as.Date("2022-01-01") & !is.na(icab_rpv_discontinued_date)) ~ unique(alai_up_uid),
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
      icab_rpv_discontinued==0 & (!is.na(icab_rpv_rx_date) | !is.na(icab_rpv_shot1_date)) ~1,
      is.na(icab_rpv_discontinued)~1,
      .default = alai_up_uid
    ),
    initiate_after_rx=case_when(
      icab_rpv_rx==1 & !is.na(icab_rpv_shot1_date) &
        icab_rpv_shot1_date>=icab_rpv_rx_date~1,
      icab_rpv_rx==1 & is.na(icab_rpv_shot1_date) ~1, #prescribed but not yet started
      icab_rpv_rx==1 & is.na(icab_rpv_rx_date) & !is.na(icab_rpv_shot1_date) ~1,# missing rx date but has dose OK
      (icab_rpv_rx==0|is.na(icab_rpv_rx))  & is.na(icab_rpv_rx_date) &
        is.na(icab_rpv_shot1_date) ~1,
      .default = alai_up_uid
    ),
  )|>
  select(alai_up_uid,discontinue_valid,initiate_after_rx)

icab_events_whole=merge(icab_events,icab_2,by="alai_up_uid") |>
  distinct()

validate(data = icab_events_whole, description = "iCAB logic check") |>
  validate_cols(predicate = in_set(c(1)),
                ever_counselled_VALID,
                description = "counsel_ever is NOT correctly documented") |>
  validate_cols(predicate = in_set(c(1)),
                ever_screened_VALID,
                description = "screen_ever is NOT correctly documented") |>
  validate_cols(predicate = in_set(c(1)),
                first_counsel_valid,
                description = "The first counseling is NOT correctly documented in counsel_1 blocks") |>
  validate_cols(predicate = in_set(c(1)),
                first_screen_valid,
                description = "The first screening is NOT correctly documented in screen_1 blocks") |>
  validate_cols(predicate = in_set(c(0)),
                prescribe_BEFORE_counsel,
                description = "Prescription date is before the counseling date.") |>
  validate_cols(predicate = in_set(c(0)),
                prescribe_BEFORE_screen,
                description = "Prescription date is before the screening date.") |>
  validate_cols(predicate = in_set(c(1)),
                initiate_after_rx,
                description = "Patient initiated CAB before being prescribed.") |>
  validate_cols(predicate = in_set(c(0)),
                prescribe_NOT_eligible,
                description = "Prescribed when patient is not eligible.") |>
  validate_cols(predicate = in_set(c(0)),
                prescribe_NOT_interested,
                description = "Prescribed when patient is not interested.") |>
  validate_cols(predicate = in_set(c(1)),
                discontinue_valid,
                description = "Patient discontinued before initiating.") |>
  validate_cols(predicate = in_set(c("0")),
                counsel_extreme_date,
                description = "Very early or late counsel date") |>
  validate_cols(predicate = in_set(c("0")),
                screen_extreme_date,
                description = "Very early or late screen date") |>
  validate_cols(predicate = in_set(c(0)),
                rx_extreme_date,
                description = "Very early or late rx date") |>
  validate_cols(predicate = in_set(c(0)),
                discontinued_extreme_date,
                description = "Very early or late discontinued date") |>
  add_results(report = report)


# create the final report, edit excel instructions
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


print("Done")

write_xlsx(list(final_report = final_report,
                missing_demographics = missing_demographics),
           path = paste0(dirname(filename), "/data_validation_report_",today,".xlsx"))

# Print the report
# print(report)
#get_results(report) |> filter(type == "error")
save_report(report,
            output_file = paste0("data_validation_report_",today,".html"),
            output_dir = dirname(filename))
browseURL(paste0(dirname(filename), "/data_validation_report_",today,"html"))





