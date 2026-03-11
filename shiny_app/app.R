library(shiny)
library(bslib)
library(jsonlite)
library(magick)

# =============================================================================
# Config
# =============================================================================

N_QUIZ     <- 20L
FACES_DIR  <- "www/faces"
WARPED_DIR <- "www/faces_warped"
INDEX_FILE <- file.path(FACES_DIR, "faces_index.json")

if (!file.exists(INDEX_FILE)) {
  stop("faces_index.json not found.\nRun generate_aligned_faces.R first.")
}

face_index   <- read_json(INDEX_FILE, simplifyVector = TRUE)
all_faces    <- face_index$faces
male_faces   <- all_faces[grepl("_m\\.png$", all_faces)]
female_faces <- all_faces[grepl("_f\\.png$", all_faces)]

# =============================================================================
# Helpers
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

compute_composite <- function(face_files) {
  paths <- ifelse(
    file.exists(file.path(WARPED_DIR, face_files)),
    file.path(WARPED_DIR, face_files),
    file.path(FACES_DIR,  face_files)
  )
  stack     <- image_read(paths)
  composite <- image_average(stack)
  out_path  <- tempfile(fileext = ".png")
  image_write(composite, out_path, format = "png")
  out_path
}

get_low_rated <- function(face_ids, ratings) {
  scores <- vapply(face_ids, function(f) ratings[[f]] %||% 3L, integer(1))
  df     <- data.frame(id = face_ids, score = scores, stringsAsFactors = FALSE)
  df     <- df[order(df$score), ]
  low    <- df$id[df$score <= 2]
  if (length(low) < 2) low <- df$id[seq_len(min(3L, nrow(df)))]
  low
}

get_high_rated <- function(face_ids, ratings) {
  scores <- vapply(face_ids, function(f) ratings[[f]] %||% 3L, integer(1))
  df     <- data.frame(id = face_ids, score = scores, stringsAsFactors = FALSE)
  df     <- df[order(df$score, decreasing = TRUE), ]
  high   <- df$id[df$score >= 4]
  if (length(high) < 2) high <- df$id[seq_len(min(3L, nrow(df)))]
  high
}

# =============================================================================
# UI helpers
# =============================================================================

ui_intro <- function() {
  div(class = "state intro-state",
    div(class = "intro-inner",
      div(class = "eyebrow", "AP Psychology with Mr. Galego"),
      h1("FaceLab", class = "app-title"),
      p(class = "intro-lead", "Select faces. Average them. See the science."),
      div(class = "intro-rule"),
      p(class = "intro-science",
        "The averageness hypothesis predicts that mathematical composites of faces \u2014",
        "even unattractive ones \u2014 are rated as more attractive than the individuals within them.",
        "Averaging cancels asymmetries and unusual features, leaving a face your brain",
        "reads as healthy and familiar."
      ),
      p(class = "intro-choose", "Choose which faces to view:"),
      div(class = "mode-buttons",
        actionButton("btn_gender_m", "Male Faces",   class = "btn-main btn-mode"),
        actionButton("btn_gender_f", "Female Faces", class = "btn-main btn-mode btn-mode-alt")
      ),
      div(class = "credit",
        "Face stimuli & averaging methodology by ",
        tags$a("Dr. Lisa DeBruine",
          href = "https://www.gla.ac.uk/schools/psychologyneuroscience/staff/lisadebruine/",
          target = "_blank"),
        ", University of Glasgow Face Research Lab"
      )
    )
  )
}

ui_mode_type <- function(gender) {
  label <- if (gender == "male") "male" else "female"
  div(class = "state intro-state",
    div(class = "intro-inner",
      div(class = "eyebrow", "AP Psychology with Mr. Galego"),
      h1("FaceLab", class = "app-title"),
      p(class = "intro-lead",
        paste0("You\u2019re viewing ", label, " faces.")
      ),
      div(class = "intro-rule"),
      p(class = "intro-choose", "Choose your experience:"),
      div(class = "mode-buttons",
        actionButton("btn_mode_quiz", "Quiz Mode",      class = "btn-main btn-mode"),
        actionButton("btn_mode_grid", "Free Selection", class = "btn-main btn-mode btn-mode-alt")
      ),
      div(class = "mode-descriptions",
        div(class = "mode-desc", "Rate 20 faces 1\u20135. We\u2019ll average your picks."),
        div(class = "mode-desc", "Pick any faces from the full grid and average them.")
      )
    )
  )
}

ui_quiz_mode <- function() {
  div(class = "state intro-state",
    div(class = "intro-inner",
      div(class = "eyebrow", "AP Psychology with Mr. Galego"),
      h1("FaceLab", class = "app-title"),
      p(class = "intro-lead", "Which faces should be averaged at the end?"),
      div(class = "intro-rule"),
      p(class = "intro-choose", "Average your:"),
      div(class = "mode-buttons",
        actionButton("btn_begin_low",  "Least Attractive", class = "btn-main btn-mode"),
        actionButton("btn_begin_high", "Most Attractive",  class = "btn-main btn-mode btn-mode-alt")
      )
    )
  )
}

ui_rating <- function(face_ids, current_idx) {
  req(!is.null(face_ids), current_idx >= 1L)
  n   <- length(face_ids)
  pct <- round(100 * (current_idx - 1) / n)
  div(class = "state rating-state",
    div(class = "progress-track",
      div(class = "progress-fill", style = paste0("width:", pct, "%"))
    ),
    div(class = "progress-text",
      span(class = "progress-current", current_idx),
      span(class = "progress-sep", " / "),
      span(class = "progress-total", n)
    ),
    div(class = "face-frame", uiOutput("rating_face_ui")),
    div(class = "rating-section",
      div(class = "rating-scale-labels",
        span("Unattractive"), span("Attractive")
      ),
      div(class = "rating-buttons",
        lapply(1:5, function(r) {
          actionButton(paste0("rate_", r), label = as.character(r), class = "rate-btn")
        })
      )
    )
  )
}

ui_processing <- function() {
  div(class = "state processing-state",
    div(class = "processing-inner",
      div(class = "pulse-ring"),
      p("Compositing your faces...", class = "processing-text")
    )
  )
}

ui_result <- function(low_faces, ratings, mode) {
  req(!is.null(low_faces))
  n_low  <- length(low_faces)
  label  <- if (mode == "high") "most attractive" else "least attractive"
  eyebrow <- paste("Average of your", n_low,
                   if (n_low == 1) paste(label, "face") else paste(label, "faces"))
  div(class = "state result-state",
    div(class = "result-eyebrow", eyebrow),
    div(class = "result-image-wrap",
      imageOutput("composite_img", height = "auto")
    ),
    div(class = "thumbs-wrap",
      div(class = "thumbs-label", "Faces that went in:"),
      div(class = "thumbs-row",
        lapply(low_faces, function(f) {
          score <- ratings[[f]] %||% "?"
          div(class = "thumb-item",
            tags$img(src = paste0("faces/", f), class = "thumb-img",
                     alt = paste("rated", score)),
            div(class = "thumb-score",
              lapply(1:5, function(i) {
                span(class = if (i <= score) "star star-on" else "star star-off", "\u2605")
              })
            )
          )
        })
      )
    ),
    div(class = "science-callout",
      p("\u201cAveraging faces cancels asymmetries and unusual features.",
        "What remains is a face that looks familiar to the human brain \u2014",
        "and familiarity reads as beauty.\u201d")
    ),
    div(class = "result-actions",
      downloadButton("download_composite", "Download Image", class = "btn-main btn-download"),
      actionButton("btn_again", "Try Again", class = "btn-main btn-secondary")
    )
  )
}

ui_grid_shell <- function(grid_faces) {
  div(class = "state grid-state",
    div(class = "grid-topbar",
      span(class = "grid-topbar-title", "FaceLab"),
      tags$div(style = "flex:1"),          # pushes button to far right
      actionButton("btn_back", "\u2190 Back", class = "btn-back")
    ),
    div(class = "grid-layout",
      div(class = "preview-panel",
        div(class = "preview-slot", uiOutput("preview_slot")),
        uiOutput("preview_controls")
      ),
      div(class = "faces-grid-panel",
        div(class = "grid-instructions",
          "Click to select \u2014 click again to deselect"
        ),
        # onclick="faceCardClick(this)" is the most reliable approach in Shiny
        div(class = "face-grid", id = "face-grid",
          lapply(grid_faces, function(f) {
            div(
              class            = "face-card",
              `data-face-id`   = f,
              onclick          = "faceCardClick(this)",
              tags$img(src = paste0("faces/", f), class = "face-thumb", alt = "")
            )
          })
        )
      )
    )
  )
}

# =============================================================================
# UI shell
# =============================================================================

ui <- page_fluid(
  title = "FaceLab",
  theme = bs_theme(
    bg           = "#0d0d0d",
    fg           = "#efefef",
    primary      = "#ffffff",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    bootswatch   = NULL
  ),
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "icon", type = "image/png", href = "blslogo.png")
  ),
  # Script at end of body so jQuery is guaranteed to be loaded
  div(class = "app-shell", uiOutput("page")),
  tags$script(src = "app.js")
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  s <- reactiveValues(
    state          = "intro",
    gender         = NULL,
    # Quiz
    face_ids       = NULL,
    current_idx    = 1L,
    ratings        = list(),
    low_faces      = NULL,
    mode           = "low",
    # Grid
    grid_faces     = NULL,
    selected       = character(0),
    preview_face   = NULL,
    # Shared
    composite_path = NULL
  )

  onSessionEnded(function() {
    isolate({
      if (!is.null(s$composite_path) && file.exists(s$composite_path))
        file.remove(s$composite_path)
    })
  })

  # -- Page router ------------------------------------------------------------
  output$page <- renderUI({
    switch(s$state,
      intro      = ui_intro(),
      mode_type  = ui_mode_type(s$gender),
      quiz_mode  = ui_quiz_mode(),
      rating     = ui_rating(s$face_ids, s$current_idx),
      processing = ui_processing(),
      result     = ui_result(s$low_faces, s$ratings, s$mode),
      grid       = ui_grid_shell(s$grid_faces)
    )
  })

  # -- Intro: gender ----------------------------------------------------------
  observeEvent(input$btn_gender_m, { s$gender <- "male";   s$state <- "mode_type" })
  observeEvent(input$btn_gender_f, { s$gender <- "female"; s$state <- "mode_type" })

  # -- Mode type: Quiz or Grid ------------------------------------------------
  observeEvent(input$btn_mode_quiz, { s$state <- "quiz_mode" })
  observeEvent(input$btn_mode_grid, {
    s$grid_faces   <- if (s$gender == "male") male_faces else female_faces
    s$selected     <- character(0)
    s$preview_face <- NULL
    s$state        <- "grid"
  })

  # -- Quiz mode: Least or Most -----------------------------------------------
  begin_quiz <- function(mode) {
    pool          <- if (s$gender == "male") male_faces else female_faces
    s$mode        <- mode
    s$face_ids    <- sample(pool, min(N_QUIZ, length(pool)))
    s$current_idx <- 1L
    s$ratings     <- list()
    s$state       <- "rating"
  }
  observeEvent(input$btn_begin_low,  { req(s$state == "quiz_mode"); begin_quiz("low")  })
  observeEvent(input$btn_begin_high, { req(s$state == "quiz_mode"); begin_quiz("high") })

  # -- Rating -----------------------------------------------------------------
  lapply(1:5, function(r) {
    observeEvent(input[[paste0("rate_", r)]], {
      req(s$state == "rating")
      s$ratings[[ s$face_ids[s$current_idx] ]] <- r
      if (s$current_idx >= N_QUIZ) {
        s$state <- "processing"
      } else {
        s$current_idx <- s$current_idx + 1L
      }
    }, ignoreInit = TRUE)
  })

  # -- Processing → Result ----------------------------------------------------
  observe({
    req(s$state == "processing")
    isolate({
      picked           <- if (s$mode == "high") get_high_rated(s$face_ids, s$ratings)
                         else                   get_low_rated(s$face_ids, s$ratings)
      s$low_faces      <- picked
      s$composite_path <- compute_composite(picked)
      s$state          <- "result"
    })
  })

  # -- Result: composite image ------------------------------------------------
  output$composite_img <- renderImage({
    req(s$state == "result", s$composite_path, file.exists(s$composite_path))
    list(src = s$composite_path, contentType = "image/png",
         width = "100%", alt = "Your composite face")
  }, deleteFile = FALSE)

  output$rating_face_ui <- renderUI({
    req(s$state == "rating", s$face_ids, s$current_idx)
    tags$img(src = paste0("faces/", s$face_ids[s$current_idx]),
             class = "rating-face-img", alt = "Rate this face")
  })

  observeEvent(input$btn_again, { clean_up(); s$state <- "intro" })

  # -- Download composite (works for both quiz result and grid) ---------------
  output$download_composite <- downloadHandler(
    filename = function() {
      paste0("facelab_composite_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(s$composite_path, file.exists(s$composite_path))
      file.copy(s$composite_path, file)
    }
  )

  # -- Grid: preview slot -----------------------------------------------------
  output$preview_slot <- renderUI({
    if (!is.null(s$composite_path) && file.exists(s$composite_path)) {
      imageOutput("grid_composite_img", height = "auto", width = "100%")
    } else if (!is.null(s$preview_face)) {
      tags$img(src = paste0("faces/", s$preview_face),
               class = "preview-face-img", alt = "Preview")
    } else {
      div(class = "preview-empty",
        div(class = "preview-icon", "\u25a2"),
        p("Click a face to preview")
      )
    }
  })

  output$grid_composite_img <- renderImage({
    req(s$state == "grid", s$composite_path, file.exists(s$composite_path))
    list(src = s$composite_path, contentType = "image/png",
         width = "100%", alt = "Composite face")
  }, deleteFile = FALSE)

  # -- Grid: controls ---------------------------------------------------------
  output$preview_controls <- renderUI({
    n   <- length(s$selected)
    has <- !is.null(s$composite_path) && file.exists(s$composite_path)
    div(class = "preview-controls",
      p(class = "selection-count",
        if      (n == 0) "Select faces on the right"
        else if (n == 1) "1 face selected \u2014 select 1 more"
        else             paste(n, "faces selected")
      ),
      if (!has) {
        actionButton("btn_average", "Average Selected",
          class    = paste("btn-main btn-generate", if (n < 2) "btn-disabled" else ""),
          disabled = if (n < 2) "disabled" else NULL
        )
      } else {
        tagList(
          p(class = "composite-done", "Composite ready"),
          downloadButton("download_composite", "Download Image",
            class = "btn-main btn-download"),
          div(class = "btn-pair",
            actionButton("btn_clear_grid", "Clear",
              class = "btn-main btn-secondary btn-pair-item"),
            actionButton("btn_reset_grid", "Start Over",
              class = "btn-main btn-secondary btn-pair-item")
          )
        )
      }
    )
  })

  # -- Grid: JS → server events -----------------------------------------------
  # ignoreNULL=FALSE so deselecting all faces (NULL) still clears s$selected
  observeEvent(input$selected_faces, {
    val        <- input$selected_faces
    s$selected <- if (is.null(val)) character(0) else val
    if (!is.null(s$composite_path)) {
      if (file.exists(s$composite_path)) file.remove(s$composite_path)
      s$composite_path <- NULL
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$preview_face, {
    s$preview_face <- input$preview_face
  }, ignoreNULL = TRUE)

  observeEvent(input$btn_average, {
    req(length(s$selected) >= 2)
    s$composite_path <- compute_composite(s$selected)
  })

  # "Clear" — wipe composite + selection but stay in grid
  observeEvent(input$btn_clear_grid, {
    if (!is.null(s$composite_path) && file.exists(s$composite_path))
      file.remove(s$composite_path)
    s$composite_path <- NULL
    s$selected       <- character(0)
    s$preview_face   <- NULL
    session$sendCustomMessage("clearSelection", list())
  })

  # "Start Over" — go all the way back to intro
  observeEvent(input$btn_reset_grid, {
    clean_up()
    s$state <- "intro"
    session$sendCustomMessage("clearSelection", list())
  })

  observeEvent(input$btn_back, {
    clean_up()
    s$state <- "intro"
    session$sendCustomMessage("clearSelection", list())
  })

  clean_up <- function() {
    if (!is.null(s$composite_path) && file.exists(s$composite_path))
      file.remove(s$composite_path)
    s$composite_path <- NULL
    s$selected       <- character(0)
    s$preview_face   <- NULL
    s$gender         <- NULL
    s$grid_faces     <- NULL
  }
}

shinyApp(ui, server)
