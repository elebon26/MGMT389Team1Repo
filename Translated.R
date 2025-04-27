# Uncomment and run these lines if you need to install any missing packages:
#install.packages(c("shiny", "shinythemes", "rvest", "dplyr", "stringr", "tidyverse", "stringdist", "tidyllm", "ggplot2", "fullcalendarWidget"))
# remotes::install_github("CannaData/fullcalendarWidget")

library(shiny)
library(shinythemes)
library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)
library(stringdist)
library(tidyllm)
library(fullcalendarWidget)
library(ggplot2)

Sys.setenv(OPENAI_API_KEY = "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")

# ===================== 1) LOAD TRANSLATION DATA =====================
translations <- read.csv("translation.csv", stringsAsFactors = FALSE)

# We'll let the user pick from these language columns:
available_langs <- c("en", "es", "zh", "tl", "vi")

# ===================== 2) SCHEDULE TIMES HELPER =====================
schedule_times <- function(freq) {
  if (freq <= 1) {
    return("8:00 AM")
  }
  interval <- 12 / (freq - 1)
  start <- as.POSIXct("2023-01-01 08:00", tz = "UTC")
  times <- start + (0:(freq - 1)) * interval * 3600
  format(times, "%I:%M %p")
}

# ===================== 3) DISEASE DATA & FUNCTIONS =====================
gloss <- read.csv("eye_terms_glossary.csv", stringsAsFactors = FALSE)

get_aao_disease_links <- function() {
  page <- read_html("https://www.aao.org/eye-health/a-z#l")
  links <- page %>% html_elements("a")
  hrefs <- links %>% html_attr("href")
  titles <- links %>% html_text(trim = TRUE)
  valid <- which(str_starts(hrefs, "/eye-health/diseases/"))
  tibble(
    condition = titles[valid],
    url = paste0("https://www.aao.org", hrefs[valid])
  )
}

aao_index <- get_aao_disease_links()

search_aao_condition <- function(keyword, index_df, n = 5) {
  keyword_clean <- tolower(keyword)
  query_tokens <- str_split(keyword_clean, "\\s+")[[1]]
  exact_match <- index_df %>% filter(tolower(condition) == keyword_clean)
  if (nrow(exact_match) > 0) {
    return(exact_match %>% select(condition, url))
  }
  scored_df <- index_df %>%
    mutate(
      condition_clean = tolower(condition),
      condition_tokens = str_split(condition_clean, "\\s+")
    ) %>%
    rowwise() %>%
    mutate(
      token_match_score = mean(
        purrr::map_dbl(query_tokens, function(q_tok) {
          min(stringdist(q_tok, unlist(condition_tokens), method = "jw"))
        })
      ),
      full_dist = stringdist(condition_clean, keyword_clean, method = "jw")
    ) %>%
    ungroup() %>%
    mutate(final_score = 0.7 * token_match_score + 0.3 * full_dist) %>%
    arrange(final_score)
  
  scored_df %>% select(condition, url) %>% slice(1:n)
}

get_disease_description <- function(disease_url) {
  page <- read_html(disease_url)
  paragraphs <- page %>% html_nodes("p") %>% html_text(trim = TRUE)
  if (length(paragraphs) < 5) {
    return(paste(paragraphs, collapse = " "))
  }
  paste(paragraphs[2:5], collapse = " ")
}

# ===================== 4) TREATMENT DATA & FUNCTIONS =====================
get_aao_treatment_links <- function() {
  page <- read_html("https://www.aao.org/eye-health/a-z#l")
  links <- page %>% html_elements("a")
  hrefs <- links %>% html_attr("href")
  titles <- links %>% html_text(trim = TRUE)
  valid <- which(str_starts(hrefs, "/eye-health/treatments/"))
  tibble(
    condition = titles[valid],
    url = paste0("https://www.aao.org", hrefs[valid])
  )
}

aao_treatment <- get_aao_treatment_links()

get_treatment_description <- function(treatment_url) {
  page <- read_html(treatment_url)
  paragraphs <- page %>% html_nodes("p") %>% html_text(trim = TRUE)
  if (length(paragraphs) < 5) {
    return(paste(paragraphs, collapse = " "))
  }
  paste(paragraphs[2:5], collapse = " ")
}

# ===================== 5) DRUG DATA & FUNCTIONS =====================
drop_guide_url <- "https://eyewiki.org/Comprehensive_Drop_Guide#Antibiotics"
page_drug <- read_html(drop_guide_url)
extracted_tables <- page_drug %>% html_table(fill = TRUE)

antibiotics_raw <- extracted_tables[[2]]
names(antibiotics_raw)[names(antibiotics_raw) == "NOTES & USES"] <- "USES"
names(antibiotics_raw)[names(antibiotics_raw) == "GTT/UNG"] <- "gtt/ung"
names(antibiotics_raw)[names(antibiotics_raw) == "TOP COLOR"] <- "COLOR"
antibiotics_df <- antibiotics_raw[, c("DRUG", "BRAND", "COLOR", "USES")]

steroid_raw <- extracted_tables[[3]]
steroid_raw$DRUG[steroid_raw$DRUG == "Prenisolone"] <- "Prednisolone"
steroid_df <- steroid_raw[, c("DRUG", "BRAND", "COLOR", "USES")]

combo_raw <- extracted_tables[[4]]
combo_steroid_antibiotic_df <- combo_raw[, c("DRUG", "BRAND", "COLOR", "USES")]

antimicrobial_raw <- extracted_tables[[5]]
antimicrobial_df <- antimicrobial_raw[, c("DRUG", "BRAND", "COLOR", "USES")]

pressure_regulator_raw <- extracted_tables[[6]]
presure_regulator_df <- pressure_regulator_raw[, c("DRUG", "BRAND", "COLOR", "USES")]

rm(drop_guide_url, page_drug, extracted_tables,
   antibiotics_raw, steroid_raw, combo_raw, antimicrobial_raw, pressure_regulator_raw)

df_list <- list(
  antibiotics_df              = antibiotics_df,
  steroid_df                  = steroid_df,
  combo_steroid_antibiotic_df = combo_steroid_antibiotic_df,
  antimicrobial_df            = antimicrobial_df,
  presure_regulator_df        = presure_regulator_df
)

search_drug_matches <- function(search_term, max_dist = 5, dist_buffer = 1) {
  search_term_lower <- tolower(search_term)
  all_matches <- list()
  for (source_name in names(df_list)) {
    current_df <- df_list[[source_name]]
    drug_dist  <- stringdist(search_term_lower, tolower(current_df$DRUG), method = "lv")
    brand_dist <- stringdist(search_term_lower, tolower(current_df$BRAND), method = "lv")
    match_idx  <- which(drug_dist <= max_dist | brand_dist <= max_dist)
    if (length(match_idx) > 0) {
      matched_rows <- current_df[match_idx, ]
      matched_rows$SOURCE <- source_name
      all_matches[[length(all_matches) + 1]] <- matched_rows
    }
  }
  combined <- bind_rows(all_matches)
  if (nrow(combined) == 0) return(NULL)
  combined <- combined %>%
    rowwise() %>%
    mutate(dist = min(
      stringdist(search_term_lower, tolower(DRUG), method = "lv"),
      stringdist(search_term_lower, tolower(BRAND), method = "lv")
    )) %>%
    ungroup()
  best_distance <- min(combined$dist)
  best_matches <- combined %>%
    filter(dist <= max_dist, dist <= (best_distance + dist_buffer)) %>%
    arrange(dist)
  best_matches
}

format_drug_description <- function(drug_row) {
  source_type <- gsub("_df", "", drug_row$SOURCE)
  source_type <- gsub("_", " and ", source_type)
  source_type <- str_to_title(source_type)
  color <- drug_row$COLOR
  uses  <- drug_row$USES
  color_text <- if (!is.na(color) && nchar(color) > 0) {
    paste0(" Its drop color is typically ", tolower(color), ".")
  } else ""
  uses_text <- if (!is.na(uses) && nchar(uses) > 0) {
    paste0(" It is used for: ", uses, ".")
  } else ""
  paste0(
    drug_row$DRUG, " is a ", tolower(source_type), " medication marketed under the name ",
    drug_row$BRAND, ".", color_text, uses_text
  )
}

# ===================== 6) SHINY APP =====================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # ============ A) Add language selector at the top ============
  fluidRow(
    column(width = 3,
           selectInput(
             "language",
             "Language:",
             choices = c("English" = "en", 
                         "Español" = "es",
                         "中文"     = "zh",
                         "Tagalog" = "tl",
                         "Tiếng Việt" = "vi"),
             selected = "en"
           )
           
    )
  ),
  # End language selector row
  
  tags$head(
    tags$style(HTML("
      .fc-today {
        background-color: #ffd2d2 !important;
      }
      .container-fluid {
        padding-left: 10px !important;
        padding-right: 10px !important;
      }
      .well {
        background-color: #fefefe; 
        border: 1px solid #ddd; 
        box-shadow: 1px 1px 3px rgba(0,0,0,0.2);
      }
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        let today = new Date();
        let yyyy = today.getFullYear();
        let mm = ('0' + (today.getMonth() + 1)).slice(-2);
        let dd = ('0' + today.getDate()).slice(-2);
        Shiny.setInputValue('selected_date', yyyy + '-' + mm + '-' + dd);
      });
    "))
  ),
  
  # ============ B) Title Panel as dynamic text ============
  titlePanel(textOutput("app_title")),
  
  # ============ C) Tabset Panel with dynamic tab labels ============
  tabsetPanel(
    id = "tabs",
    
    # 1) Summary tab
    tabPanel(title = textOutput("ophthalmic_history"),  # dynamic label
             fluidRow(
               column(width = 4,
                      wellPanel(
                        h4(textOutput("condition_entry_label")),
                        textInput("condition_input", label = textOutput("enter_condition_label"), value = ""),
                        radioButtons("eye_selection", label = textOutput("affected_eye_label"),
                                     choices = c("Right Eye", "Left Eye", "Both Eyes"),
                                     selected = "Both Eyes"),
                        actionButton("check_condition", "", class = "btn btn-primary"),
                        actionButton("remove_condition", "", class = "btn btn-danger"),
                        uiOutput("saved_conditions")
                      ),
                      wellPanel(
                        h4(textOutput("drug_entry_label")),
                        textInput("drug_input", label = textOutput("enter_drug_label"), value = ""),
                        numericInput("drug_frequency", label = textOutput("times_per_day_label"),
                                     value = 1, min = 1, step = 1),
                        actionButton("check_drug", "", class = "btn btn-primary"),
                        actionButton("remove_drug", "", class = "btn btn-danger"),
                        uiOutput("saved_drugs")
                      )
               ),
               
               column(width = 8,
                      fluidRow(
                        column(width = 12,
                               wellPanel(
                                 style = "min-height: 260px; overflow-y: auto; font-size: calc(14px + 0.2vw); margin-bottom: 30px;",
                                 h3(textOutput("patient_history_summary_label")),
                                 uiOutput("ai_summary_ui")
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               wellPanel(
                                 h4(textOutput("treatment_entry_label")),
                                 selectInput("treatment_select", label = textOutput("select_treatments_label"),
                                             choices = aao_treatment$condition, multiple = TRUE),
                                 actionButton("add_treatment", "", class = "btn btn-primary"),
                                 actionButton("remove_treatment", "", class = "btn btn-danger"),
                                 uiOutput("saved_treatments")
                               ),
                               wellPanel(
                                 h4(textOutput("doctor_notes_question_label")),
                                 textAreaInput("doctor_notes", label = NULL, value = "", 
                                               placeholder = ""),
                                 br(),
                                 actionButton("generate_output", "", class = "btn btn-success")
                               )
                        ),
                        column(width = 6,
                               wellPanel(
                                 style = "min-height: 250px; overflow-y: auto; font-size: calc(14px + 0.2vw);",
                                 h4(textOutput("fast_facts_label")),
                                 uiOutput("fast_facts")
                               )
                        )
                      )
               )
             )
    ),
    
    # 2) Calendar tab
    tabPanel(title = textOutput("calendar_tab"),  # dynamic label
             fluidRow(
               style = "margin-bottom: 0px;",
               column(
                 width = 6,
                 tags$div(
                   style = "border:1px solid #ccc; padding:10px; background-color:#f9f9f9;",
                   fullcalendarOutput("calendar", height = "450px")
                 )
               ),
               column(
                 width = 6,
                 fluidRow(
                   column(
                     width = 8,
                     h3(textOutput("date_label"), textOutput("current_date_label", inline = TRUE))
                   ),
                   column(
                     width = 4,
                     actionButton("reschedule_btn", "", class = "btn btn-warning")
                   )
                 ),
                 tags$div(
                   style = "border:1px solid #ccc; padding:10px; margin-bottom:10px; background-color:#fefefe;",
                   h4(textOutput("medication_schedule_label")),
                   verbatimTextOutput("med_schedule")
                 ),
                 tags$div(
                   style = "border:1px solid #ccc; padding:10px; margin-bottom:10px; background-color:#fefefe;",
                   h4(textOutput("symptom_log_label")),
                   numericInput("pain_log", label = textOutput("enter_symptom_severity_label"),
                                value = NA, min = 0, max = 10)
                 ),
                 actionButton("save_btn", "", class = "btn btn-success")
               )
             ),
             fluidRow(
               style = "margin-top: 0px;",
               column(
                 width = 6,
                 tags$div(
                   style = "border:1px solid #ccc; padding:10px; min-height:250px; background-color:#fefefe;",
                   h4(textOutput("symptom_log_patient_notes_label")),
                   uiOutput("notes_display")
                 )
               ),
               column(
                 width = 6,
                 tags$div(
                   style = "border:1px solid #ccc; padding:10px; min-height:250px; background-color:#fefefe;",
                   h4(textOutput("symptom_severity_log_7_days_label")),
                   plotOutput("pain_chart", height = "180px")
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # ========== 7) Translator function: picks the correct language column ==========
  tr <- function(key) {
    row_idx <- which(translations$key == key)
    if (length(row_idx) == 0) return(key)  # fallback if key not found
    # If user picks a language that doesn't exist, fallback to "en"
    lang_col <- if (input$language %in% colnames(translations)) input$language else "en"
    translations[[lang_col]][row_idx]
  }
  
  # ========== 8) Render dynamic text for all your labels & buttons ==========
  output$app_title <- renderText({ tr("app_title") })
  output$ophthalmic_history <- renderText({ tr("ophthalmic_history") })
  output$calendar_tab <- renderText({ tr("calendar_tab") })
  
  output$condition_entry_label <- renderText({ tr("condition_entry") })
  output$enter_condition_label <- renderText({ tr("enter_condition") })
  output$affected_eye_label <- renderText({ tr("affected_eye") })
  
  # Because radioButtons expects named choices, we can do something dynamic:
  observe({
    updateRadioButtons(session, "eye_selection",
                       label = tr("affected_eye"),
                       choices = c(tr("right_eye"), tr("left_eye"), tr("both_eyes")),
                       selected = tr("both_eyes"))
  })
  
  observe({
    updateActionButton(session, "check_condition", label = tr("check_condition"))
    updateActionButton(session, "remove_condition", label = tr("remove_condition"))
    
    updateActionButton(session, "check_drug", label = tr("check_drug"))
    updateActionButton(session, "remove_drug", label = tr("remove_drug"))
    
    updateActionButton(session, "add_treatment", label = tr("add_treatment"))
    updateActionButton(session, "remove_treatment", label = tr("remove_treatment"))
    
    updateActionButton(session, "generate_output", label = tr("generate_output"))
    updateActionButton(session, "save_btn", label = tr("save_btn"))
    updateActionButton(session, "reschedule_btn", label = tr("rescheduled_appointment"))
  })
  
  output$drug_entry_label <- renderText({ tr("drug_entry") })
  output$enter_drug_label <- renderText({ tr("enter_drug") })
  output$times_per_day_label <- renderText({ tr("times_per_day") })
  output$patient_history_summary_label <- renderText({ tr("patient_history_summary") })
  output$treatment_entry_label <- renderText({ tr("treatment_entry") })
  output$select_treatments_label <- renderText({ tr("select_treatments") })
  output$doctor_notes_question_label <- renderText({ tr("doctor_notes_question") })
  
  # The textAreaInput placeholder
  observe({
    updateTextAreaInput(session, "doctor_notes", 
                        label = NULL, 
                        placeholder = tr("enter_doctor_notes"))
  })
  
  output$fast_facts_label <- renderText({ tr("fast_facts") })
  
  output$date_label <- renderText({ tr("date_label") })
  output$medication_schedule_label <- renderText({ tr("medication_schedule") })
  output$symptom_log_label <- renderText({ tr("symptom_log") })
  output$enter_symptom_severity_label <- renderText({ tr("enter_symptom_severity") })
  output$symptom_log_patient_notes_label <- renderText({ tr("symptom_log_patient_notes") })
  output$symptom_severity_log_7_days_label <- renderText({ tr("symptom_severity_log_7_days") })
  
  # We'll also define a variable to store the dynamic text for "Save" if you want:
  observe({
    updateActionButton(session, "save_btn", label = tr("save"))
  })
  
  #### Summary Tab Reactive Values ####
  final_condition_list <- reactiveVal(list())
  final_drug_list <- reactiveVal(list())
  final_treatment_list <- reactiveVal(list())
  condition_fuzzy_options <- reactiveVal(NULL)
  drug_fuzzy_options <- reactiveVal(NULL)
  ai_summary_val <- reactiveVal("")
  
  # ------------------ Condition logic ------------------
  observeEvent(input$check_condition, {
    req(input$condition_input)
    cond_text <- input$condition_input
    exact_cond <- aao_index %>% filter(tolower(condition) == tolower(cond_text))
    if (nrow(exact_cond) > 0) {
      current <- final_condition_list()
      current[[length(current) + 1]] <- list(row = exact_cond[1, ], eye = input$eye_selection)
      final_condition_list(current)
      updateTextInput(session, "condition_input", value = "")
    } else {
      fuzzy_res <- search_aao_condition(cond_text, aao_index, n = 5)
      if (is.null(fuzzy_res) || nrow(fuzzy_res) == 0) {
        showNotification(paste(tr("no_matches_condition"), cond_text), type = "error")
      } else if (nrow(fuzzy_res) == 1) {
        current <- final_condition_list()
        current[[length(current) + 1]] <- list(row = fuzzy_res[1, ], eye = input$eye_selection)
        final_condition_list(current)
        updateTextInput(session, "condition_input", value = "")
      } else {
        condition_fuzzy_options(fuzzy_res)
        showModal(
          modalDialog(
            title = tr("select_your_condition"),
            selectInput("selected_condition_modal", tr("choose_one"), choices = fuzzy_res$condition),
            footer = tagList(
              actionButton("confirm_condition_modal", tr("confirm")),
              modalButton(tr("cancel"))
            )
          )
        )
      }
    }
  })
  
  observeEvent(input$confirm_condition_modal, {
    req(condition_fuzzy_options())
    fuzzy <- condition_fuzzy_options()
    chosen <- input$selected_condition_modal
    row_idx <- which(fuzzy$condition == chosen)
    if (length(row_idx) > 0) {
      current <- final_condition_list()
      current[[length(current) + 1]] <- list(row = fuzzy[row_idx, ], eye = input$eye_selection)
      final_condition_list(current)
    }
    removeModal()
    updateTextInput(session, "condition_input", value = "")
  })
  
  observeEvent(input$remove_condition, {
    current <- final_condition_list()
    if (length(current) > 0) {
      current <- current[-length(current)]
      final_condition_list(current)
    }
  })
  
  output$saved_conditions <- renderUI({
    cond_list <- final_condition_list()
    if (length(cond_list) == 0) return(NULL)
    tagList(
      h5(tr("saved_conditions")),
      lapply(seq_along(cond_list), function(i) {
        entry <- cond_list[[i]]
        p(paste(tr("condition_entry"), i, ":", entry$row$condition,
                "-", tr("affected_eye"), ":", entry$eye))
      })
    )
  })
  
  # ------------------ Drug logic ------------------
  observeEvent(input$check_drug, {
    req(input$drug_input)
    drug_text <- input$drug_input
    all_drugs <- bind_rows(df_list, .id = "SOURCE")
    exact_drug <- all_drugs %>% filter(tolower(DRUG) == tolower(drug_text) | tolower(BRAND) == tolower(drug_text))
    if (nrow(exact_drug) > 0) {
      current <- final_drug_list()
      current[[length(current) + 1]] <- list(row = exact_drug[1, ], frequency = input$drug_frequency)
      final_drug_list(current)
      updateTextInput(session, "drug_input", value = "")
      updateNumericInput(session, "drug_frequency", value = 1)
    } else {
      fuzzy_drug <- search_drug_matches(drug_text, max_dist = 5, dist_buffer = 1)
      if (is.null(fuzzy_drug) || nrow(fuzzy_drug) == 0) {
        showNotification(paste(tr("no_matches_drug"), drug_text), type = "error")
      } else if (nrow(fuzzy_drug) == 1) {
        current <- final_drug_list()
        current[[length(current) + 1]] <- list(row = fuzzy_drug[1, ], frequency = input$drug_frequency)
        final_drug_list(current)
        updateTextInput(session, "drug_input", value = "")
        updateNumericInput(session, "drug_frequency", value = 1)
      } else {
        drug_fuzzy_options(fuzzy_drug)
        showModal(
          modalDialog(
            title = tr("select_your_drug"),
            selectInput("selected_drug_modal", tr("choose_one"),
                        choices = apply(fuzzy_drug, 1, function(r) {
                          paste0(r["DRUG"], " (", r["BRAND"], ")")
                        })),
            footer = tagList(
              actionButton("confirm_drug_modal", tr("confirm")),
              modalButton(tr("cancel"))
            )
          )
        )
      }
    }
  })
  
  observeEvent(input$confirm_drug_modal, {
    req(drug_fuzzy_options())
    fuzzy <- drug_fuzzy_options()
    choices <- apply(fuzzy, 1, function(r) paste0(r["DRUG"], " (", r["BRAND"], ")"))
    chosen <- input$selected_drug_modal
    row_idx <- which(choices == chosen)
    if (length(row_idx) > 0) {
      current <- final_drug_list()
      current[[length(current) + 1]] <- list(row = fuzzy[row_idx, ], frequency = input$drug_frequency)
      final_drug_list(current)
    }
    removeModal()
    updateTextInput(session, "drug_input", value = "")
    updateNumericInput(session, "drug_frequency", value = 1)
  })
  
  observeEvent(input$remove_drug, {
    current <- final_drug_list()
    if (length(current) > 0) {
      current <- current[-length(current)]
      final_drug_list(current)
    }
  })
  
  output$saved_drugs <- renderUI({
    d_list <- final_drug_list()
    if (length(d_list) == 0) return(NULL)
    tagList(
      h5(tr("saved_drugs")),
      lapply(seq_along(d_list), function(i) {
        entry <- d_list[[i]]
        row <- entry$row
        freq <- entry$frequency
        p(paste(tr("drug_entry"), i, ":", row$DRUG,
                "(", row$BRAND, ")", "-", freq, tr("times_per_day")))
      })
    )
  })
  
  # ------------------ Treatment logic ------------------
  observeEvent(input$add_treatment, {
    req(input$treatment_select)
    current <- final_treatment_list()
    for (t_name in input$treatment_select) {
      row_idx <- which(aao_treatment$condition == t_name)
      if (length(row_idx) > 0) {
        current[[length(current) + 1]] <- aao_treatment[row_idx, ]
      }
    }
    final_treatment_list(current)
  })
  
  observeEvent(input$remove_treatment, {
    current <- final_treatment_list()
    if (length(current) > 0) {
      current <- current[-length(current)]
      final_treatment_list(current)
    }
  })
  
  output$saved_treatments <- renderUI({
    t_list <- final_treatment_list()
    if (length(t_list) == 0) return(NULL)
    tagList(
      h5(tr("saved_treatments")),
      lapply(seq_along(t_list), function(i) {
        row <- t_list[[i]]
        p(paste(tr("treatment_entry"), i, ":", row$condition))
      })
    )
  })
  
  # ------------------ Generate AI Summary ------------------
  ai_summary_val <- reactiveVal("")
  
  observeEvent(input$generate_output, {
    if (length(final_condition_list()) == 0 && length(final_drug_list()) == 0 &&
        length(final_treatment_list()) == 0 && input$doctor_notes == "") {
      showNotification("Please enter at least one condition, drug, treatment, or doctor note.", type = "error")
    } else {
      condition_summaries <- lapply(final_condition_list(), function(entry) {
        desc <- get_disease_description(entry$row$url)
        paste0(entry$row$condition, " (", entry$eye, "): ", desc)
      })
      drug_summaries <- lapply(final_drug_list(), function(entry) {
        format_drug_description(entry$row)
      })
      treatment_summaries <- lapply(final_treatment_list(), function(t_row) {
        get_treatment_description(t_row$url)
      })
      
      additional_notes <- if (input$doctor_notes != "") {
        paste0("Additional Doctor Notes: ", input$doctor_notes)
      } else {
        ""
      }
      
      # --- Only the prompt_text changed here ---
      prompt_text <- paste0(
        "Patient Data Summary:\n",
        "Language Selected: ",
        switch(
          input$language,
          "en" = "English",
          "es" = "Spanish",
          "zh" = "Chinese",
          "tl" = "Tagalog",
          "vi" = "Vietnamese",
          "English"
        ),
        "\n\nConditions:\n", 
        paste(condition_summaries, collapse = "\n\n"),
        
        "\n\nMedications:\n",
        paste(drug_summaries, collapse = "\n\n"),
        
        "\n\nTreatments:\n",
        paste(treatment_summaries, collapse = "\n\n"),
        
        "\n\nDoctor's Notes:\n",
        additional_notes,
        
        "\n\nYou are an ophthalmology expert speaking directly to a patient who has a high school education and limited scientific knowledge. Your role is to explain each disease, treatment, and medication in clear, respectful, and accessible language, much like a knowledgeable and patient teacher guiding someone through important health information. The tone should be calm, informative, and supportive, without being too casual or overly formal. Aim to provide some more context than a basic definition, without overwhelming the patient with unnecessary detail, helping the patient understand how everything connects to their care. When discussing medications, include the typical eye drop cap color if known, as this helps with recognition. Explanations of medications and treatments should be smoothly connected to the conditions they are meant to help manage. Where it makes sense, describe how one condition might influence another to give a fuller picture of the patient’s eye health. Use analogies to clarify complex ideas, but don’t overdo it. Avoid terms like then, next, or anything that makes the explanation sound like a step-by-step list. Also avoid phrases like I will answer your questions or language that reveals the explanation comes from a language model. Refrain from speaking in absolutes. Use phrases like may help, might be related to, or could affect. Begin with a simple, respectful introduction of each condition, describing how it might affect vision or daily life. Medications should be introduced in context, with clear mention of their role and cap color when applicable. Treatments can be explained with clarity and reassurance, describing their purpose and how they relate to the overall care plan. Length of response should vary based on how many terms are provided: for one term, keep it concise but informative; for two to four terms, aim for about 200 words; for more, extend the explanation appropriately while maintaining a composed and patient-focused tone. Your overall goal is explanation and understanding, and to give the patient an idea of their overall ophthalmic history.\n\n",
        
        "\n\nDO NOT TELL THE PATIENT WHAT TO DO OR GIVE ADVICE LIKE CONTACTING THE DOCTOR. You should not scare the patient. Avoid extraneous detail outside the scope of the patient history. Ensure that diagnoses, drugs, and treatments are intertwined when appropriate. Speak in a fluid manner. Responses should not need to be more than 220 words, so speak efficiently, while still maintaining fluidity and flow.\n\n",
        
        "Please produce the final response in ",
        switch(
          input$language,
          "en" = "English",
          "es" = "Spanish",
          "zh" = "Chinese",
          "tl" = "Tagalog",
          "vi" = "Vietnamese",
          "English"
        ),
        "."
      )
      
      msg_obj <- llm_message(prompt_text)
      
      ai_result <- chat(
        .llm         = msg_obj,
        .provider    = chatgpt(),
        .model       = "gpt-4o",
        .temperature = 0.7,
        .max_tries   = 3
      )
      
      ai_text <- get_reply(ai_result)
      ai_summary_val(ai_text)
    }
  })
  
  output$ai_summary_ui <- renderUI({
    req(ai_summary_val())
    p(ai_summary_val())
  })
  
  # ------------------ Fast Facts (Glossary) ------------------
  output$fast_facts <- renderUI({
    # If the selected language is not English, display a message in multiple languages
    if (input$language != "en") {
      return(tags$p(
        "Sorry, not available. ",
        "(Lo siento, no disponible. ",
        "对不起，目前无法使用。 ",
        "Paumanhin, hindi magagamit. ",
        "Xin lỗi, không có sẵn.)"
      ))
    }
    
    # Otherwise, proceed with the original logic
    if (is.null(ai_summary_val()) || ai_summary_val() == "") {
      return(tags$p(tr("fast_facts_placeholder")))
    }
    
    summary_lower <- tolower(ai_summary_val())
    terms_lower <- tolower(gloss$term)
    
    matched_idx <- vapply(seq_along(terms_lower), function(i) {
      grepl(paste0("\\b", terms_lower[i], "\\b"), summary_lower)
    }, logical(1))
    
    matched <- gloss[matched_idx, , drop = FALSE]
    if (nrow(matched) == 0) {
      return(tags$p(tr("no_glossary_terms")))
    }
    matched <- matched[!duplicated(matched$definition), , drop = FALSE]
    
    bullet_list <- lapply(seq_len(nrow(matched)), function(i) {
      tags$li(
        tags$strong(matched$term[i]),
        ": ",
        matched$definition[i]
      )
    })
    
    tags$ul(bullet_list, style = "list-style-type: disc; padding-left: 20px;")
  })
  
  # ------------------ Medication Schedule ------------------
  output$med_schedule <- renderText({
    drugs <- final_drug_list()
    if (length(drugs) == 0) return(tr("no_medications_scheduled"))
    
    schedule_lines <- sapply(drugs, function(entry) {
      freq <- entry$frequency
      interval <- 12 / (max(freq - 1, 1))
      start <- as.POSIXct("2023-01-01 08:00", tz = "UTC")
      times <- start + (0:(freq - 1)) * interval * 3600
      times_str <- format(times, "%I:%M %p")
      paste0(entry$row$DRUG, " (", entry$row$BRAND, "): ", paste(times_str, collapse = ", "))
    })
    
    paste(schedule_lines, collapse = "\n")
  })
  
  # ------------------ Calendar & Pain Log Logic ------------------
  nextAppointment <- reactiveVal(NULL)
  
  appointmentEvent <- reactive({
    if (is.null(nextAppointment())) {
      return(data.frame())
    }
    data.frame(
      start       = nextAppointment(),
      end         = nextAppointment(),
      rendering   = "background",
      color       = "aqua",
      borderColor = "aqua",
      allDay      = TRUE,
      title       = "",
      stringsAsFactors = FALSE
    )
  })
  
  selectedDateEvent <- reactive({
    req(input$selected_date)
    if (input$selected_date == as.character(Sys.Date())) {
      data.frame(start = character(), end = character(), stringsAsFactors = FALSE)
    } else {
      data.frame(
        start       = input$selected_date,
        end         = input$selected_date,
        rendering   = "background",
        color       = "lightgrey",
        borderColor = "lightgrey",
        allDay      = TRUE,
        title       = "",
        stringsAsFactors = FALSE
      )
    }
  })
  
  output$calendar <- renderFullcalendar({
    fullcalendar(
      events = rbind(selectedDateEvent(), appointmentEvent()),
      options = list(
        defaultDate = as.character(Sys.Date()),
        defaultView = "month",
        header = list(left = "", center = "title", right = "prev,next")
      ),
      callbacks = list(
        dayClick = htmlwidgets::JS("
          function(date, jsEvent, view) {
            let clicked = date.format('YYYY-MM-DD');
            Shiny.setInputValue('selected_date', clicked);
          }
        ")
      )
    )
  })
  
  output$current_date_label <- renderText({
    req(input$selected_date)
    input$selected_date
  })
  
  showAppointmentModal <- function() {
    showModal(modalDialog(
      title = tr("schedule_next_appointment"),
      radioButtons("appt_yes_no", tr("another_appointment"),
                   choices = c(tr("yes"), tr("no")), selected = tr("no")),
      conditionalPanel(
        condition = "input.appt_yes_no == 'Yes' || input.appt_yes_no == 'Sí' 
                     || input.appt_yes_no == '是' || input.appt_yes_no == 'Oo'
                     || input.appt_yes_no == 'Vâng'",  # covers multiple languages for "Yes"
        dateInput("appt_date_input", tr("appointment_date"), value = Sys.Date() + 1)
      ),
      footer = tagList(
        actionButton("appt_submit", tr("submit")),
        modalButton(tr("cancel"))
      ),
      easyClose = FALSE
    ))
  }
  
  observeEvent(input$tabs, {
    if (input$tabs == "Calendar" && is.null(nextAppointment())) {
      showAppointmentModal()
    }
  })
  
  observeEvent(input$reschedule_btn, {
    showAppointmentModal()
  })
  
  observeEvent(input$appt_submit, {
    # For "Yes" in multiple languages, we do a simple check:
    if (input$appt_yes_no %in% c(tr("yes"), "Yes", "Sí", "是", "Oo", "Vâng")) {
      nextAppointment(as.character(input$appt_date_input))
    } else {
      nextAppointment(NULL)
    }
    removeModal()
  })
  
  daily_data <- reactiveVal(list())
  
  observeEvent(input$selected_date, {
    req(input$selected_date)
    stored_data <- daily_data()
    if (!is.null(stored_data[[ input$selected_date ]])) {
      updateNumericInput(session, "pain_log", value = stored_data[[ input$selected_date ]]$pain)
    } else {
      updateNumericInput(session, "pain_log", value = NA)
    }
  })
  
  observeEvent(input$save_btn, {
    req(input$selected_date)
    stored_data <- daily_data()
    if (is.null(stored_data[[ input$selected_date ]])) {
      stored_data[[ input$selected_date ]] <- list(medications = "", pain = input$pain_log, notes = "")
    } else {
      stored_data[[ input$selected_date ]]$pain <- input$pain_log
    }
    daily_data(stored_data)
    
    showModal(modalDialog(
      title = paste(tr("symptoms_cause_question"), input$selected_date),
      textAreaInput("modal_notes", tr("notes_label"),
                    value = if(!is.null(stored_data[[ input$selected_date ]])) stored_data[[ input$selected_date ]]$notes else "",
                    width = "100%", height = "150px"),
      footer = tagList(
        modalButton(tr("cancel")),
        actionButton("modal_save", tr("submit"))
      ),
      easyClose = FALSE
    ))
  })
  
  observeEvent(input$modal_save, {
    req(input$selected_date)
    stored_data <- daily_data()
    stored_data[[ input$selected_date ]]$notes <- input$modal_notes
    daily_data(stored_data)
    removeModal()
  })
  
  output$notes_display <- renderUI({
    dd <- daily_data()
    if(length(dd) == 0) return(p(tr("no_notes_saved")))
    keys <- sort(names(dd))
    note_lines <- sapply(keys, function(date_key) {
      note <- dd[[date_key]]$notes
      if(is.null(note) || note == "") return(NA)
      formatted_date <- format(as.Date(date_key), "%m/%d/%y")
      # remove leading zero from month/day for neatness
      formatted_date <- sub("^0", "", formatted_date)
      formatted_date <- sub("/0", "/", formatted_date)
      paste0(formatted_date, " - ", note)
    }, USE.NAMES = FALSE)
    note_lines <- note_lines[!is.na(note_lines)]
    if(length(note_lines) == 0) return(p(tr("no_notes_saved")))
    tagList(lapply(note_lines, p))
  })
  
  pain_df <- reactive({
    dd <- daily_data()
    if (length(dd) == 0) {
      return(data.frame(date = as.Date(character()), pain = numeric()))
    }
    do.call(rbind, lapply(names(dd), function(d) {
      data.frame(date = as.Date(d), pain = as.numeric(dd[[d]]$pain))
    }))
  })
  
  output$pain_chart <- renderPlot({
    all_dates <- seq(Sys.Date() - 6, Sys.Date(), by = "day")
    df <- pain_df()
    df_all <- merge(data.frame(date = all_dates), df, by = "date", all.x = TRUE)
    ggplot(df_all, aes(x = date, y = pain)) +
      geom_line(group = 1, na.rm = TRUE, color = "blue") +
      geom_point(na.rm = TRUE, color = "red") +
      scale_x_date(breaks = all_dates, date_labels = "%m/%d") +
      scale_y_continuous(limits = c(0, 10)) +
      labs(x = tr("date"), y = tr("pain_level")) +
      theme_minimal()
  })
}

shinyApp(ui, server)