library(shiny)
library(bslib)
library(jsonlite)
library(magick)

# =============================================================================
# Config
# =============================================================================

N_QUIZ     <- 20L   # faces shown per rating session
FACES_DIR  <- "www/faces"          # originals — shown in rating UI & thumbnails
WARPED_DIR <- "www/faces_warped"   # Shepards-warped — used only for compositing
INDEX_FILE <- file.path(FACES_DIR, "faces_index.json")

# Graceful startup check
if (!file.exists(INDEX_FILE)) {
  stop(
    "faces_index.json not found.\n",
    "Run generate_aligned_faces.R first to prepare the face images."
  )
}

face_index <- read_json(INDEX_FILE, simplifyVector = TRUE)
all_faces  <- face_index$faces   # character vector: "001_03.png", etc.

# =============================================================================
# Helper functions
# =============================================================================

# Average pre-aligned faces → save to OS temp dir → return absolute path.
# Using tempfile() (not www/tmp/) so this works on ShinyApps.io where www/
# is read-only at runtime.
compute_composite <- function(face_files) {
  # Use warped copies so pixel-averaging produces a sharp composite.
  # Fall back to originals for any file missing from faces_warped/.
  paths <- ifelse(
    file.exists(file.path(WARPED_DIR, face_files)),
    file.path(WARPED_DIR, face_files),
    file.path(FACES_DIR,  face_files)
  )
  stack     <- image_read(paths)
  composite <- image_average(stack)
  out_path  <- tempfile(fileext = ".png")
  image_write(composite, out_path, format = "png")
  out_path   # absolute path; served via renderImage
}

# Return files for the lowest-rated faces (rated 1-2, or bottom 3 if too few)
get_low_rated <- function(face_ids, ratings) {
  scores <- vapply(face_ids, function(f) ratings[[f]] %||% 3L, integer(1))
  df     <- data.frame(id = face_ids, score = scores, stringsAsFactors = FALSE)
  df     <- df[order(df$score), ]
  low    <- df$id[df$score <= 2]
  if (length(low) < 2) low <- df$id[seq_len(min(3L, nrow(df)))]
  low
}

# Return files for the highest-rated faces (rated 4-5, or top 3 if too few)
get_high_rated <- function(face_ids, ratings) {
  scores <- vapply(face_ids, function(f) ratings[[f]] %||% 3L, integer(1))
  df     <- data.frame(id = face_ids, score = scores, stringsAsFactors = FALSE)
  df     <- df[order(df$score, decreasing = TRUE), ]
  high   <- df$id[df$score >= 4]
  if (length(high) < 2) high <- df$id[seq_len(min(3L, nrow(df)))]
  high
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# UI helpers — one function per state
# =============================================================================

ui_intro <- function() {
  div(class = "state intro-state",
    div(class = "intro-inner",
      div(class = "eyebrow", "AP Psychology with Mr. Galego"),
      h1("FaceLab", class = "app-title"),
      p(class = "intro-lead",
        "Rate 20 faces. Then watch your least attractive ones",
        "become something you might rate differently."
      ),
      div(class = "intro-rule"),
      p(class = "intro-science",
        "The averageness hypothesis predicts that mathematical composites of faces \u2014",
        "even unattractive ones \u2014 are rated as more attractive than the individuals within them.",
        "Averaging cancels asymmetries and unusual features, leaving a face your brain",
        "reads as healthy and familiar."
      ),
      div(class = "mode-buttons",
        actionButton("btn_begin_low",  "Least Attractive", class = "btn-main btn-mode"),
        actionButton("btn_begin_high", "Most Attractive",  class = "btn-main btn-mode btn-mode-alt")
      ),
      div(class = "credit",
        "Face stimuli & averaging methodology by ",
        tags$a("Dr. Lisa DeBruine", href = "https://www.gla.ac.uk/schools/psychologyneuroscience/staff/lisadebruine/", target = "_blank"),
        ", University of Glasgow Face Research Lab"
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
    div(class = "face-frame",
      uiOutput("rating_face_ui")
    ),
    div(class = "rating-section",
      div(class = "rating-scale-labels",
        span("Unattractive"),
        span("Attractive")
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
            tags$img(src = paste0("faces/", f), class = "thumb-img", alt = paste("rated", score)),
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
      actionButton("btn_again", "Try Again", class = "btn-main btn-secondary")
    )
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- page_fixed(
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
    tags$script(src = "app.js"),
    tags$link(rel = "icon", type = "image/png", href = "blslogo.png")
  ),
  div(class = "app-shell",
    uiOutput("page")
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  s <- reactiveValues(
    state          = "intro",
    face_ids       = NULL,
    current_idx    = 1L,
    ratings        = list(),
    composite_path = NULL,
    low_faces      = NULL,
    mode           = "low"   # "low" = least attractive, "high" = most attractive
  )

  # Clean up temp composite on disconnect
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
      rating     = ui_rating(s$face_ids, s$current_idx),
      processing = ui_processing(),
      result     = ui_result(s$low_faces, s$ratings, s$mode)
    )
  })

  # -- Intro ------------------------------------------------------------------
  begin_quiz <- function(mode) {
    s$mode        <- mode
    s$face_ids    <- sample(all_faces, N_QUIZ)
    s$current_idx <- 1L
    s$ratings     <- list()
    s$state       <- "rating"
  }
  observeEvent(input$btn_begin_low,  begin_quiz("low"))
  observeEvent(input$btn_begin_high, begin_quiz("high"))

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
      picked <- if (s$mode == "high")
        get_high_rated(s$face_ids, s$ratings)
      else
        get_low_rated(s$face_ids, s$ratings)
      path             <- compute_composite(picked)
      s$low_faces      <- picked
      s$composite_path <- path
      s$state          <- "result"
    })
  })

  # -- Result: composite image ------------------------------------------------
  output$composite_img <- renderImage({
    req(s$state == "result", s$composite_path, file.exists(s$composite_path))
    list(src = s$composite_path, contentType = "image/png",
         width = "100%", alt = "Your composite face")
  }, deleteFile = FALSE)

  # -- Result: Try Again ------------------------------------------------------
  observeEvent(input$btn_again, {
    if (!is.null(s$composite_path) && file.exists(s$composite_path))
      file.remove(s$composite_path)
    s$composite_path <- NULL
    s$state          <- "intro"
  })

  # -- Rating face image ------------------------------------------------------
  output$rating_face_ui <- renderUI({
    req(s$state == "rating", s$face_ids, s$current_idx)
    tags$img(
      src   = paste0("faces/", s$face_ids[s$current_idx]),
      class = "rating-face-img",
      alt   = "Rate this face"
    )
  })
}

shinyApp(ui, server)
