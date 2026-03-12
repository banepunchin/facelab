library(shiny)
library(bslib)
library(jsonlite)
library(magick)

# =============================================================================
# Config
# =============================================================================

FACES_DIR  <- "www/faces"
WARPED_DIR <- "www/faces_warped"
INDEX_FILE <- file.path(FACES_DIR, "faces_index.json")

if (!file.exists(INDEX_FILE)) {
  stop("faces_index.json not found.\nRun generate_aligned_faces.R first.")
}

face_index <- read_json(INDEX_FILE, simplifyVector = TRUE)
all_faces  <- face_index$faces

# =============================================================================
# Helpers
# =============================================================================

compute_composite <- function(face_files) {
  paths     <- ifelse(
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
      div(class = "mode-buttons",
        actionButton("btn_begin", "Begin", class = "btn-main btn-mode")
      ),
      div(class = "credit",
        "Face stimuli & averaging methodology by ",
        tags$a("Dr. Lisa DeBruine",
          href   = "https://www.gla.ac.uk/schools/psychologyneuroscience/staff/lisadebruine/",
          target = "_blank"),
        ", University of Glasgow Face Research Lab"
      )
    )
  )
}

ui_grid_shell <- function(grid_faces) {
  div(class = "state grid-state",
    div(class = "grid-topbar",
      span(class = "grid-topbar-title", "FaceLab"),
      tags$div(style = "flex:1"),
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
        div(class = "face-grid", id = "face-grid",
          lapply(grid_faces, function(f) {
            div(
              class          = "face-card",
              `data-face-id` = f,
              onmouseover    = "faceHover(this)",
              onclick        = "faceCardClick(this)",
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
    tags$link(rel = "icon", type = "image/png", href = "blslogo.png"),
    # Critical grid layout inlined — cannot be stale-cached like a static file
    tags$style(HTML("
      .shiny-html-output { display: block; width: 100%; }
      .container-fluid    { padding-left: 0 !important; padding-right: 0 !important; }
      .grid-state {
        display: flex !important; flex-direction: column !important;
        min-height: 100vh; width: 100%; max-width: none !important;
        background: #0d0d0d;
      }
      .grid-topbar {
        display: flex !important; align-items: center !important;
        width: 100%; padding: 12px 24px;
        border-bottom: 1px solid #2a2a2a; flex-shrink: 0;
      }
      .grid-layout {
        display: flex !important; flex: 1;
        gap: 20px; padding: 20px 24px;
        overflow: hidden; height: calc(100vh - 49px);
      }
      .preview-panel {
        width: 340px; flex-shrink: 0;
        display: flex !important; flex-direction: column !important; gap: 14px;
      }
      .faces-grid-panel { flex: 1 !important; min-width: 0; overflow-y: auto; }
      .face-grid {
        display: grid !important;
        grid-template-columns: repeat(auto-fill, minmax(110px, 1fr)) !important;
        gap: 6px !important;
      }
      .face-card {
        position: relative !important; cursor: pointer !important;
        border-radius: 6px !important; overflow: hidden !important;
        background: #222 !important;
        transition: box-shadow 0.15s ease !important;
      }
      .face-card:hover    { box-shadow: inset 0 0 0 2px rgba(255,255,255,0.35) !important; }
      .face-card.selected { box-shadow: inset 0 0 0 3px #ffffff !important; }
      .face-thumb {
        width: 100% !important; aspect-ratio: 1/1 !important;
        object-fit: cover !important; display: block !important;
      }
      .preview-slot {
        width: 100% !important;
        background: rgba(255,255,255,0.02) !important;
        border: 1px dashed rgba(255,255,255,0.1) !important;
        border-radius: 8px !important; overflow: hidden !important;
        min-height: 300px !important;
      }
      .preview-slot img { width: 100% !important; height: auto !important; display: block !important; }
    "))
  ),
  uiOutput("page"),
  tags$script(HTML("
function faceHover(el) {
  var faceId = el.getAttribute('data-face-id');
  Shiny.setInputValue('preview_face', faceId, { priority: 'event' });
}
function faceCardClick(el) {
  var $card  = $(el);
  $card.toggleClass('selected');
  if ($card.hasClass('selected')) {
    $card.append('<div class=\"face-check\">\\u2713</div>');
  } else {
    $card.find('.face-check').remove();
  }
  var selected = [];
  $('.face-card.selected').each(function () {
    selected.push($(this).attr('data-face-id'));
  });
  Shiny.setInputValue('selected_faces', selected.length > 0 ? selected : null, { priority: 'event' });
}
Shiny.addCustomMessageHandler('clearSelection', function (_msg) {
  $('.face-card').removeClass('selected').find('.face-check').remove();
});
  "))
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  s <- reactiveValues(
    state          = "intro",
    grid_faces     = NULL,
    selected       = character(0),
    preview_face   = NULL,
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
    if (s$state == "grid") {
      ui_grid_shell(s$grid_faces)
    } else {
      div(class = "app-shell", ui_intro())
    }
  })

  # -- Intro: begin -----------------------------------------------------------
  observeEvent(input$btn_begin, {
    s$grid_faces     <- all_faces
    s$selected       <- character(0)
    s$preview_face   <- NULL
    s$composite_path <- NULL
    s$state          <- "grid"
  })

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

  # "Start Over" — go back to intro
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

  # -- Download composite -----------------------------------------------------
  output$download_composite <- downloadHandler(
    filename = function() {
      paste0("facelab_composite_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(s$composite_path, file.exists(s$composite_path))
      file.copy(s$composite_path, file)
    }
  )

  clean_up <- function() {
    if (!is.null(s$composite_path) && file.exists(s$composite_path))
      file.remove(s$composite_path)
    s$composite_path <- NULL
    s$selected       <- character(0)
    s$preview_face   <- NULL
    s$grid_faces     <- NULL
  }
}

shinyApp(ui, server)
