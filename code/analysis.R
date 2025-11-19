#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
  }
  pacman::p_load(
    dplyr,
    fixest,
    glue,
    haven,
    purrr,
    stringr,
    survey,
    tibble
  )
})

options(survey.lonely.psu = "adjust")

root_dir <- getwd()
raw_dir <- file.path(root_dir, "data", "raw")
output_dir <- file.path(root_dir, "output", "tables")

if (!dir.exists(raw_dir)) {
  stop("Cannot find data/raw. Run this script from the repository root.")
}

read_wave5 <- function(filename) {
  read_dta(file.path(raw_dir, filename))
}

zap_num <- function(x) {
  as.numeric(haven::zap_labels(x))
}

question_map <- tribble(
  ~var, ~prefix, ~correct_codes,
  "w5_flint", "numeracy", 2,
  "w5_flval", "inflation", 2,
  "w5_flcomp1", "comp1", 1,
  "w5_flcomp2", "comp2", 1,
  "w5_flrisk", "risk", 2
)

indicator_cols <- c(
  "numeracy_correct",
  "numeracy_dk",
  "inflation_correct",
  "inflation_dk",
  "comp1_correct",
  "comp1_dk",
  "comp2_correct",
  "comp2_dk",
  "risk_correct",
  "risk_dk",
  "overall"
)

prepare_data <- function() {
  adult <- read_wave5("Adult_W5_Anon_V1.0.0.dta") %>%
    select(
      pid,
      w5_hhid,
      starts_with("w5_a_fl"),
      w5_a_asfin,
      w5_a_aspen,
      w5_a_em1prod_c
    ) %>%
    rename_with(~ str_replace(.x, "^w5_a_", "w5_"), starts_with("w5_a_"))

  link <- read_wave5("Link_File_W5_Anon_V1.0.0.dta") %>%
    select(pid, w5_hhid, cluster)

  individual <- read_wave5("indderived_W5_Anon_V1.0.0.dta") %>%
    select(
      w5_hhid,
      pid,
      w5_flyn,
      w5_flscore,
      w5_best_age_yrs,
      w5_best_race,
      w5_best_gen,
      w5_best_marstt,
      w5_best_edu,
      w5_empl_stat,
      w5_fwag
    )

  household <- read_wave5("hhderived_W5_Anon_V1.0.0.dta") %>%
    select(
      w5_hhid,
      w5_dc2001,
      w5_wgt,
      w5_hhincome,
      w5_net_worth
    )

  data <- adult %>%
    left_join(individual, by = c("pid", "w5_hhid")) %>%
    left_join(link, by = c("pid", "w5_hhid")) %>%
    left_join(household, by = "w5_hhid")

  labelled_vars <- c(
    question_map$var,
    "w5_flyn",
    "w5_flscore",
    "w5_best_gen",
    "w5_best_race",
    "w5_best_edu",
    "w5_best_marstt",
    "w5_empl_stat",
    "w5_em1prod_c",
    "w5_asfin",
    "w5_aspen"
  )

  data <- data %>%
    mutate(across(all_of(labelled_vars), zap_num))

  add_question_indicators <- function(df, var, prefix, correct_codes) {
    values <- df[[var]]
    valid <- !is.na(values) & values != -3
    df[[paste0(prefix, "_correct")]] <- ifelse(
      valid,
      ifelse(values %in% correct_codes, 1, 0),
      NA_real_
    )
    df[[paste0(prefix, "_dk")]] <- ifelse(
      valid,
      ifelse(values %in% c(-8, -9), 1, 0),
      NA_real_
    )
    df
  }

  for (i in seq_len(nrow(question_map))) {
    data <- add_question_indicators(
      data,
      question_map$var[i],
      question_map$prefix[i],
      question_map$correct_codes[[i]]
    )
  }

  recode_education <- function(x) {
    case_when(
      x == 25 ~ 0,
      x %in% 1:7 ~ 1,
      x %in% 8:11 ~ 2,
      x %in% c(14:18, 24, 27:32) ~ 3,
      x == 12 ~ 4,
      x %in% 19:23 ~ 5,
      TRUE ~ NA_real_
    )
  }

  education_code <- recode_education(data$w5_best_edu)
  education_labels <- c(
    "No schooling",
    "Primary",
    "High school",
    "Technical",
    "Matric",
    "Degree or diploma"
  )

  wealth_p90 <- quantile(data$w5_net_worth, probs = 0.9, na.rm = TRUE, type = 2)

  data <- data %>%
    mutate(
      w5_best_age_yrs = as.numeric(w5_best_age_yrs),
      w5_fwag = as.numeric(w5_fwag),
      age_cat = cut(
        w5_best_age_yrs,
        breaks = c(0, 35, 55, 65, Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("0 - 35", "35 - 55", "55 - 65", "65 +")
      ),
      gender = factor(case_when(
        w5_best_gen == 1 ~ "Male",
        w5_best_gen == 2 ~ "Female",
        TRUE ~ NA_character_
      ), levels = c("Male", "Female")),
      education = factor(
        education_labels[education_code + 1],
        levels = education_labels
      ),
      race = factor(case_when(
        w5_best_race == 1 ~ "African",
        w5_best_race == 2 ~ "Coloured",
        w5_best_race == 3 ~ "Asian/Indian",
        w5_best_race == 4 ~ "White",
        TRUE ~ NA_character_
      ), levels = c("African", "Coloured", "Asian/Indian", "White")),
      income_cat = factor(case_when(
        w5_hhincome >= 0 & w5_hhincome < 5000 ~ "R0 - R5 000",
        w5_hhincome >= 5000 & w5_hhincome < 15000 ~ "R5 000 - R15 000",
        w5_hhincome >= 15000 & w5_hhincome < 30000 ~ "R15 000 - R30 000",
        w5_hhincome >= 30000 ~ "R30 000 +",
        TRUE ~ NA_character_
      ), levels = c(
        "R0 - R5 000",
        "R5 000 - R15 000",
        "R15 000 - R30 000",
        "R30 000 +"
      )),
      wealth_cat = factor(case_when(
        !is.na(w5_net_worth) & w5_net_worth < wealth_p90 ~ "Bottom 90%",
        !is.na(w5_net_worth) & w5_net_worth >= wealth_p90 ~ "Top 10%",
        TRUE ~ NA_character_
      ), levels = c("Bottom 90%", "Top 10%"))
    )

  emp_sector <- case_when(
    data$w5_em1prod_c %in% c(0:7, 9) ~ "Non-financial sector",
    data$w5_em1prod_c == 8 ~ "Financial sector",
    TRUE ~ NA_character_
  )
  emp_sector[data$w5_empl_stat %in% c(0, 1, 2)] <- "Not employed"
  data$emp_sect <- factor(
    emp_sector,
    levels = c("Not employed", "Non-financial sector", "Financial sector")
  )

  data <- data %>%
    mutate(
      overall = case_when(
        w5_flyn == 1 ~ 1,
        w5_flyn == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      male = case_when(
        w5_best_gen == 1 ~ 1,
        w5_best_gen == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      female = case_when(
        w5_best_gen == 2 ~ 1,
        w5_best_gen == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      married = case_when(
        w5_best_marstt == 1 ~ 1,
        w5_best_marstt %in% c(2, 3, 4, 5) ~ 0,
        TRUE ~ NA_real_
      ),
      university = case_when(
        education_code %in% 0:4 ~ 0,
        education_code == 5 ~ 1,
        TRUE ~ NA_real_
      ),
      white = case_when(
        w5_best_race == 4 ~ 1,
        w5_best_race %in% c(1, 2, 3) ~ 0,
        TRUE ~ NA_real_
      ),
      finlit3 = case_when(
        w5_flyn == 1 ~ 1,
        w5_flyn == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      finlit4 = case_when(
        w5_flscore == 4 ~ 1,
        w5_flscore %in% 0:3 ~ 0,
        TRUE ~ NA_real_
      ),
      pension = case_when(
        w5_aspen == 1 ~ 1,
        w5_aspen %in% c(0, 2, -8, -9) ~ 0,
        TRUE ~ NA_real_
      ),
      mutual_fund = case_when(
        w5_asfin == 1 ~ 1,
        w5_asfin %in% c(0, 2, -8, -9) ~ 0,
        TRUE ~ NA_real_
      ),
      working_age_2000 = case_when(
        !is.na(w5_fwag) & w5_fwag >= 2000 &
          !is.na(w5_best_age_yrs) &
          w5_best_age_yrs >= 25 & w5_best_age_yrs < 60 ~ 1,
        !is.na(w5_hhid) ~ 0,
        TRUE ~ NA_real_
      )
    )

  analysis_data <- data %>%
    filter(
      !is.na(w5_wgt),
      w5_wgt > 0,
      !is.na(cluster),
      !is.na(w5_dc2001)
    ) %>%
    mutate(cluster = as.numeric(cluster))

  design <- svydesign(
    ids = ~cluster,
    strata = ~w5_dc2001,
    weights = ~w5_wgt,
    data = analysis_data,
    nest = TRUE
  )

  list(data = analysis_data, design = design)
}

escape_latex <- function(x) {
  x %>% str_replace_all("%", "\\\\%")
}

format_number <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits)
}

calc_demo_table <- function(design, var, header, panel_id) {
  stats <- svyby(
    as.formula(paste0("~", paste(indicator_cols, collapse = " + "))),
    as.formula(paste0("~", var)),
    design,
    svymean,
    na.rm = TRUE,
    keep.names = TRUE,
    vartype = NULL
  )

  var_levels <- levels(design$variables[[var]])
  if (is.null(var_levels)) {
    var_levels <- unique(stats[[var]])
  }

  as_tibble(stats) %>%
    rename(category = !!rlang::sym(var)) %>%
    mutate(
      category = as.character(category),
      level_order = match(category, var_levels),
      panel = panel_id,
      header = header
    ) %>%
    arrange(level_order) %>%
    select(-level_order) %>%
    mutate(across(all_of(indicator_cols), ~ .x * 100))
}

format_table2_category <- function(header, value) {
  ifelse(
    header == "Income",
    case_when(
      value == "R0 - R5 000" ~ "R0-",
      value == "R5 000 - R15 000" ~ "R5,000-",
      value == "R15 000 - R30 000" ~ "R15,000-",
      value == "R30 000 +" ~ "R30,000-",
      TRUE ~ value
    ),
    value
  )
}

format_panel <- function(df) {
  df %>%
    mutate(
      display_category = format_table2_category(header, category)
    ) %>%
    mutate(
      category_label = paste0("\\ \\ ", escape_latex(display_category))
    ) %>%
    mutate(across(all_of(indicator_cols), ~ format_number(.x, digits = 1)))
}

build_panel_lines <- function(panel_df) {
  headers <- unique(panel_df$header)
  lines <- vector("list", length(headers))
  for (i in seq_along(headers)) {
    current_header <- headers[i]
    section <- panel_df %>% filter(header == current_header)
    rows <- section %>%
      transmute(
        line = glue(
          "{category_label} & {numeracy_correct} & {numeracy_dk} & ",
          "{inflation_correct} & {inflation_dk} & ",
          "{comp1_correct} & {comp1_dk} & ",
          "{comp2_correct} & {comp2_dk} & ",
          "{risk_correct} & {risk_dk} & {overall} \\\\"
        )
      ) %>%
      pull(line)
    lines[[i]] <- c(
      glue("\\multicolumn{{12}}{{l}}{{\\textbf{{{current_header}}}}} \\\\"),
      rows
    )
  }
  unlist(lines, use.names = FALSE)
}

write_table02 <- function(data, design) {
  panel_config <- tribble(
    ~panel, ~header, ~var,
    1, "Age categories", "age_cat",
    1, "Gender", "gender",
    1, "Education", "education",
    1, "Apartheid-era racial categories", "race",
    2, "Income", "income_cat",
    2, "Wealth", "wealth_cat",
    2, "Employment", "emp_sect"
  )

  result_tables <- pmap_dfr(
    panel_config,
    function(panel, header, var) {
      calc_demo_table(design, var, header, panel)
    }
  )

  panel1 <- format_panel(filter(result_tables, panel == 1))
  panel2 <- format_panel(filter(result_tables, panel == 2))

  header_lines <- c(
    "\\afterpage{",
    "\\begin{landscape}",
    "\\begin{table}",
    "\\vspace*{-2cm}",
    "\\thisfloatpagestyle{empty} % To remove page number",
    "  \\centerfloat",
    "  \\small",
    "\\caption{The Distribution of Financial Literacy by Demographics}",
    "\\label{tab:dist}",
    "\\begin{tabular}{lccccccccccc}",
    "\\toprule",
    "{} & \\multicolumn{2}{c}{\\textbf{Numeracy}} & \\multicolumn{2}{c}{\\textbf{Inflation}} & \\multicolumn{2}{c}{\\textbf{Compounding 1}} & \\multicolumn{2}{c}{\\textbf{Compounding 2}} & \\multicolumn{2}{c}{\\textbf{Risk}} & \\textbf{Overall} \\\\",
    "\\textbf{Category} &  Correct & Don't know &   Correct & Don't know &       Correct & Don't know &       Correct & Don't know & Correct & Don't know &     >=3 \\\\",
    "\\midrule"
  )

  footer_lines <- c(
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )

  continued_header <- c(
    "",
    "",
    "\\begin{table}",
    "\\thisfloatpagestyle{empty} % To remove page number",
    "\\caption*{Table 2 continued: The Distribution of Financial Literacy by Demographics}",
    "\\centerfloat",
    "\\small",
    "\\begin{tabular}{lccccccccccc}",
    "\\toprule",
    "{} & \\multicolumn{2}{c}{\\textbf{Numeracy}} & \\multicolumn{2}{c}{\\textbf{Inflation}} & \\multicolumn{2}{c}{\\textbf{Compounding 1}} & \\multicolumn{2}{c}{\\textbf{Compounding 2}} & \\multicolumn{2}{c}{\\textbf{Risk}} & \\textbf{Overall} \\\\",
    "\\textbf{Category} &  Correct & Don't know &   Correct & Don't know &       Correct & Don't know &       Correct & Don't know & Correct & Don't know &     >=3 \\\\",
    "\\midrule"
  )

  continued_footer <- c(
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    "\\end{landscape}",
    "}"
  )

  output_lines <- c(
    header_lines,
    build_panel_lines(panel1),
    footer_lines,
    continued_header,
    build_panel_lines(panel2),
    continued_footer
  )

  writeLines(output_lines, file.path(output_dir, "table_02.tex"))
}

format_result <- function(value, se, digits = 2) {
  est <- format_number(value, digits = digits)
  err <- format_number(se, digits = digits)
  list(est = est, se = err)
}

write_table03 <- function(data, design) {
  tab_data <- subset(design, !is.na(finlit3) & !is.na(male))

  model1 <- svyglm(finlit3 ~ male, design = tab_data)
  coef1 <- coef(summary(model1))["male", "Estimate"]
  se1 <- coef(summary(model1))["male", "Std. Error"]

  fe_data <- tab_data$variables %>%
    filter(!is.na(finlit3), !is.na(male), !is.na(w5_hhid))

  model2 <- feols(
    finlit3 ~ male | w5_hhid,
    weights = ~w5_wgt,
    cluster = ~w5_dc2001,
    data = fe_data
  )

  coef2 <- coef(model2)["male"]
  se2 <- se(model2)["male"]

  res1 <- format_result(coef1 * 100, se1 * 100)
  res2 <- format_result(coef2 * 100, se2 * 100)
  n_obs <- format(fe_data %>% nrow(), big.mark = ",", scientific = FALSE)

  lines <- c(
    "\\begin{table}[htbp] ",
    "\\centering",
    "\\begin{threeparttable}",
    "\\caption{Financial literacy and gender}",
    "\\label{tab:gender}",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "Specification           & 1         & 2         \\\\",
    "\\midrule",
    glue("Male                    & {res1$est}    & {res2$est}   \\\\"),
    glue("                      & ({res1$se}) & ({res2$se}) \\\\"),
    "\\midrule",
    "Household Fixed Effects & No        & Yes       \\\\",
    glue("Observations            & {n_obs}    & {n_obs}    \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[para,flushleft] % Options for paragraph format, aligned left",
    "\\small % Make the note text smaller",
    "\\textit{Note:} Both specifications use weights and standard errors are clustered at the district council level. Standard errors are shown in brackets.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, file.path(output_dir, "table_03.tex"))
}

calc_asset_table <- function(design, group_var, header) {
  svyby(
    as.formula(paste0("~", paste(indicator_cols, collapse = " + "))),
    as.formula(paste0("~", group_var)),
    design,
    svymean,
    na.rm = TRUE,
    keep.names = TRUE,
    vartype = NULL
  ) %>%
    as_tibble() %>%
    rename(category = !!rlang::sym(group_var)) %>%
    mutate(
      category = ifelse(category == 1, "Yes", "No"),
      header = header
    ) %>%
    arrange(desc(category == "Yes")) %>%
    mutate(across(all_of(indicator_cols), ~ .x * 100))
}

write_table04 <- function(design) {
  working_design <- subset(design, working_age_2000 == 1)

  pension_tbl <- calc_asset_table(
    subset(working_design, !is.na(pension)),
    "pension",
    "Pension"
  )

  mutual_tbl <- calc_asset_table(
    subset(working_design, !is.na(mutual_fund)),
    "mutual_fund",
    "Mutual funds"
  )

  make_rows <- function(tbl) {
    tbl %>%
      mutate(
        category_label = paste0("\\ \\ ", category)
      ) %>%
      mutate(across(all_of(indicator_cols), ~ format_number(.x, digits = 2))) %>%
      transmute(
        line = glue(
          "{category_label}\t&\t{numeracy_correct}\t&\t{numeracy_dk}\t&\t{inflation_correct}\t&\t{inflation_dk}\t&\t{comp1_correct}\t&\t{comp1_dk}\t&\t{comp2_correct}\t&\t{comp2_dk}\t&\t{risk_correct}\t&\t{risk_dk}\t&\t{overall}\t\\\\"
        )
      ) %>%
      pull(line)
  }

  body_lines <- c(
    "\\multicolumn{12}{l}{\\textbf{Pension}} \\\\",
    make_rows(pension_tbl),
    "\\multicolumn{12}{l}{\\textbf{Mutual funds}} \\\\",
    make_rows(mutual_tbl)
  )

  lines <- c(
    "\\afterpage{",
    "\\begin{landscape}",
    "\\begin{table}",
    "\\vspace*{-2cm}",
    "\\thisfloatpagestyle{empty} % To remove page number",
    "  \\centerfloat",
    "  \\small",
    "\\caption{Financial literacy and holdings of pensions and mutual funds}",
    "\\label{tab:pensions}",
    "\\begin{tabular}{lccccccccccc}",
    "\\toprule",
    "{} & \\multicolumn{2}{c}{\\textbf{Numeracy}} & \\multicolumn{2}{c}{\\textbf{Inflation}} & \\multicolumn{2}{c}{\\textbf{Compounding 1}} & \\multicolumn{2}{c}{\\textbf{Compounding 2}} & \\multicolumn{2}{c}{\\textbf{Risk}} & \\textbf{Overall} \\\\",
    "\\textbf{Category} &  Correct & Don't know &   Correct & Don't know &       Correct & Don't know &       Correct & Don't know & Correct & Don't know &     >=3 \\\\",
    "\\midrule",
    body_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    "\\end{landscape}",
    "}"
  )

  writeLines(lines, file.path(output_dir, "table_04.tex"))
}

calc_ame_indicator <- function(model, coef_name) {
  design <- model$survey.design
  mm <- model.matrix(model)
  eta <- as.numeric(predict(model, type = "link"))
  beta <- coef(model)[coef_name]
  x <- mm[, coef_name]
  eta1 <- eta + (1 - x) * beta
  eta0 <- eta - x * beta
  diff <- plogis(eta1) - plogis(eta0)
  tmp <- update(design, ame = diff)
  est <- svymean(~ame, tmp)
  c(estimate = as.numeric(coef(est)), se = as.numeric(SE(est)))
}

calc_ame_continuous <- function(model, coef_name) {
  design <- model$survey.design
  beta <- coef(model)[coef_name]
  p_hat <- as.numeric(predict(model, type = "response"))
  diff <- beta * p_hat * (1 - p_hat)
  tmp <- update(design, ame = diff)
  est <- svymean(~ame, tmp)
  c(estimate = as.numeric(coef(est)), se = as.numeric(SE(est)))
}

extract_ame <- function(model, var_info) {
  map_dfr(var_info, function(info) {
    coef_name <- info$coef
    if (!(coef_name %in% names(coef(model)))) {
      return(tibble(variable = info$label, estimate = NA_real_, se = NA_real_))
    }
    stats <- if (info$type == "continuous") {
      calc_ame_continuous(model, coef_name)
    } else {
      calc_ame_indicator(model, coef_name)
    }
    tibble(
      variable = info$label,
      estimate = stats["estimate"] * 100,
      se = stats["se"] * 100
    )
  })
}

write_table05 <- function(design) {
  working_design <- subset(
    design,
    working_age_2000 == 1 &
      !is.na(pension) &
      !is.na(mutual_fund) &
      !is.na(finlit3) &
      !is.na(finlit4) &
      !is.na(w5_best_age_yrs) &
      !is.na(female) &
      !is.na(married) &
      !is.na(university) &
      !is.na(white) &
      !is.na(income_cat) &
      !is.na(wealth_cat)
  )

  reg_formula <- function(lit_var, dep_var) {
    as.formula(glue(
      "{dep_var} ~ {lit_var} + w5_best_age_yrs + female + married + university + white + income_cat + wealth_cat"
    ))
  }

  model1 <- svyglm(
    reg_formula("finlit3", "pension"),
    design = working_design,
    family = quasibinomial()
  )
  model2 <- svyglm(
    reg_formula("finlit4", "pension"),
    design = working_design,
    family = quasibinomial()
  )
  model3 <- svyglm(
    reg_formula("finlit3", "mutual_fund"),
    design = working_design,
    family = quasibinomial()
  )
  model4 <- svyglm(
    reg_formula("finlit4", "mutual_fund"),
    design = working_design,
    family = quasibinomial()
  )

  ame_vars <- list(
    list(coef = "finlit3", label = "Correct in at least three categories", type = "indicator"),
    list(coef = "finlit4", label = "Correct in all four categories", type = "indicator"),
    list(coef = "w5_best_age_yrs", label = "Age", type = "continuous"),
    list(coef = "female", label = "Female", type = "indicator"),
    list(coef = "married", label = "Married", type = "indicator"),
    list(coef = "university", label = "Degree or diploma from university", type = "indicator"),
    list(coef = "white", label = "Categorized as \"white\" under Apartheid", type = "indicator"),
    list(coef = "income_catR5 000 - R15 000", label = "R5 000 - R15 000", type = "indicator"),
    list(coef = "income_catR15 000 - R30 000", label = "R15 000 - R30 000", type = "indicator"),
    list(coef = "income_catR30 000 +", label = "R30 000 +", type = "indicator"),
    list(coef = "wealth_catTop 10%", label = "Top 10 ", type = "indicator")
  )

  extract_specs <- function(model) {
    extract_ame(model, ame_vars)
  }

  specs <- list(
    extract_specs(model1),
    extract_specs(model2),
    extract_specs(model3),
    extract_specs(model4)
  )

  combine_specs <- function(row_index, spec_index) {
    spec <- specs[[spec_index]]
    spec[row_index, c("estimate", "se")]
  }

  mean_pension <- svymean(~pension, working_design)
  mean_mutual <- svymean(~mutual_fund, working_design)

  mean_line <- glue(
    "Mean of dependent variable\t&\t\\multicolumn{{2}}{{c}}{{{format_number(coef(mean_pension) * 100, 2)}\\%}}\t\t&\t\\multicolumn{{2}}{{c}}{{{format_number(coef(mean_mutual) * 100, 2)}\\%}}\t\t\\\\"
  )
  mean_se_line <- glue(
    "\t&\t\\multicolumn{{2}}{{c}}{{({format_number(SE(mean_pension) * 100, 2)}\\%)}}\t\t&\t\\multicolumn{{2}}{{c}}{{({format_number(SE(mean_mutual) * 100, 2)}\\%)}}\t\t\\\\"
  )

  format_row <- function(idx, label, include_section = FALSE) {
    spec_values <- map(specs, ~ .x[idx, ])
    ests <- map_chr(spec_values, ~ ifelse(is.na(.x$estimate), "", format_number(.x$estimate, 1)))
    ses <- map_chr(spec_values, ~ ifelse(is.na(.x$se), "", paste0("(", format_number(.x$se, 1), ")")))
    row <- glue("{label}\t&\t{ests[[1]]}\t&\t{ests[[2]]}\t&\t{ests[[3]]}\t&\t{ests[[4]]}\t\\\\")
    se_row <- glue("\t&\t{ses[[1]]}\t&\t{ses[[2]]}\t&\t{ses[[3]]}\t&\t{ses[[4]]}\t\\\\")
    if (include_section) {
      c(glue("\\multicolumn{{5}}{{l}}{{\\textbf{{{label}}}}}\t\\\\"))
    } else {
      c(row, se_row)
    }
  }

  var_labels <- c(
    "Correct in at least three categories",
    "Correct in all four categories",
    "Age",
    "Female",
    "Married",
    "Degree or diploma from university",
    "Categorized as \"white\" under Apartheid",
    "R5 000 - R15 000",
    "R15 000 - R30 000",
    "R30 000 +",
    "Top 10 "
  )

  table_rows <- map(seq_along(var_labels), function(i) {
    format_row(i, var_labels[i])
  }) %>%
    unlist()

  n_obs <- format(nrow(working_design$variables), big.mark = ",", scientific = FALSE)

  lines <- c(
    "\\begin{table}[htbp] ",
    "\\centering",
    "\\begin{threeparttable}",
    "\\caption{Average marginal effects of financial literacy on pension and mutual fund holdings}",
    "\\label{tab:logit_pension}",
    "\\small",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Dependent variable\t&\t\\multicolumn{2}{c}{Pension}\t\t&\t\\multicolumn{2}{c}{Mutual funds}\t\t\t\\\\",
    mean_line,
    mean_se_line,
    "\\midrule",
    "Specification \t&\t1\t&\t2\t&\t3\t&\t4\t\\\\",
    "\\midrule",
    "\\multicolumn{5}{l}{\\textbf{Financial literacy measure}}\t\\\\",
    table_rows[1:4],
    "",
    "\\multicolumn{5}{l}{\\textbf{Socio-demographic controls}}\t\t\\\\",
    table_rows[5:length(table_rows)],
    "\\midrule",
    glue("Observations\t&\t{n_obs}\t&\t{n_obs}\t&\t{n_obs}\t&\t{n_obs}\t\\\\"),
    "  \\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[para,flushleft] % Options for paragraph format, aligned left",
    "  \\small % Make the note text smaller",
    "  \\textit{Note:} Calculated on subpopulation of working age (25-60 years) individuals who earn more than R2 000 per month from their primary occupation. We use weights and account for the complex survey design.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, file.path(output_dir, "table_05.tex"))
}

run_analysis <- function() {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  data_list <- prepare_data()
  data <- data_list$data
  design <- data_list$design

  write_table02(data, design)
  write_table03(data, design)
  write_table04(design)
  write_table05(design)
  message("Updated tables 2-5 in ", output_dir)
}

if (interactive()) {
  run_analysis()
}
