# =============================================================================
# generate_aligned_faces.R
#
# Run this ONCE before launching the Shiny app.
#
# What it does:
#   Loads all neutral_front faces (jpg + tem), aligns them all to a common
#   shape using webmorphR::align(), and saves individual PNGs to
#   shiny_app/www/faces/.
#
#   ALIGNMENT STRATEGY (tries best option first):
#   1. Procrustes (all ~189 landmark points) — sharpest composites, aligns
#      eyes AND mouth AND nose simultaneously. Requires geomorph.
#      Uses options(rgl.useNULL = TRUE) to bypass the XQuartz/OpenGL error
#      on macOS ARM without needing XQuartz installed.
#   2. Eye alignment fallback (pt1=0, pt2=1) — if Procrustes still fails,
#      aligns by eye points only. Works everywhere, slightly blurrier mouth.
#
#   Because all output faces share the same coordinate system,
#   any subset can be averaged with magick::image_average() in milliseconds
#   at runtime — no re-alignment ever happens in the Shiny app.
#
# Run from webmorphR-master/ directory:
#   source("generate_aligned_faces.R")
# =============================================================================

if (!requireNamespace("devtools",  quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("magick",    quietly = TRUE)) install.packages("magick")
if (!requireNamespace("jsonlite",  quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("geomorph",  quietly = TRUE)) install.packages("geomorph")

# KEY FIX: tell rgl to run headless (no OpenGL window) before geomorph loads.
# This prevents the XQuartz/libGLU error on macOS ARM.
options(rgl.useNULL = TRUE)

devtools::load_all(".")
library(magick)
library(jsonlite)

SOURCE_DIR <- "/Users/hashiabdulle/Downloads/neutral_front"
OUT_DIR    <- "shiny_app/www/faces"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -- Load stimuli -------------------------------------------------------------
message("Loading stimuli from: ", SOURCE_DIR)
all_stims <- read_stim(SOURCE_DIR)
n_total   <- length(all_stims)
message("Loaded ", n_total, " faces.\n")

# -- Align --------------------------------------------------------------------
# Try Procrustes first (best quality). Fall back to 2-point eye alignment.

message("Attempting Procrustes alignment (all landmark points)...")
aligned <- tryCatch({
  a <- align(all_stims, procrustes = TRUE, fill = "white")
  message("Procrustes alignment succeeded. Composites will be sharp across the full face.\n")
  a
}, error = function(e) {
  message("Procrustes failed: ", conditionMessage(e))
  message("Falling back to 2-point eye alignment...\n")
  align(all_stims, pt1 = 0, pt2 = 1, fill = "white")
})

message("Alignment done.\n")

# -- Save aligned faces -------------------------------------------------------
message("Saving ", n_total, " aligned faces to: ", OUT_DIR)
face_ids <- character(n_total)

for (i in seq_len(n_total)) {
  nm      <- names(aligned)[i]
  outfile <- file.path(OUT_DIR, paste0(nm, ".png"))
  image_write(aligned[[i]]$img, path = outfile, format = "png")
  face_ids[i] <- paste0(nm, ".png")
  if (i %% 10 == 0) message("  ", i, " / ", n_total)
}

# -- Write index --------------------------------------------------------------
info <- image_info(aligned[[1]]$img)

index <- list(
  faces   = face_ids,
  width   = as.integer(info$width),
  height  = as.integer(info$height),
  n_total = as.integer(n_total)
)

write_json(index,
           path       = file.path(OUT_DIR, "faces_index.json"),
           pretty     = TRUE,
           auto_unbox = TRUE)

message("\nDone! ", n_total, " aligned faces + faces_index.json saved to: ", OUT_DIR)
message("You can now launch the Shiny app: shiny::runApp('shiny_app')")
