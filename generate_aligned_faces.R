# =============================================================================
# generate_aligned_faces.R  —  Shape-Based Warping Edition
#
# Run this ONCE before launching the Shiny app.
#
# HOW IT WORKS (shape-based warping, not just eye alignment):
#   1. Parses all 189 landmark points from each face's .tem file
#   2. Computes the mean landmark shape across all faces
#   3. Uses ImageMagick's Shepards distortion to warp each face's features
#      (eyes, nose, mouth, jaw) to the mean shape
#   4. Saves warped PNGs to shiny_app/www/faces/
#
# Because every face shares the same shape, pixel averaging at runtime
# produces a sharp composite across the whole face — not just the eyes.
#
# Run from webmorphR-master/ directory in RStudio:
#   source("generate_aligned_faces.R")
# =============================================================================

if (!requireNamespace("magick",   quietly = TRUE)) install.packages("magick")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

library(magick)
library(jsonlite)

SOURCE_DIR  <- "/Users/hashiabdulle/Downloads/neutral_front"
OUT_DIR     <- "shiny_app/www/faces"          # originals — shown in rating UI
WARPED_DIR  <- "shiny_app/www/faces_warped"   # warped   — used only for compositing

dir.create(OUT_DIR,    recursive = TRUE, showWarnings = FALSE)
dir.create(WARPED_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Parse a single .tem file ------------------------------------------------
# .tem format: line 1 = n_points, then n_points lines of "x\ty" coords,
# then connectivity data (ignored here).
parse_tem <- function(path) {
  lines    <- readLines(path, warn = FALSE)
  n_points <- as.integer(trimws(lines[1]))
  coords   <- lines[2:(n_points + 1)]
  parts    <- strsplit(trimws(coords), "\\s+")
  x        <- as.numeric(sapply(parts, `[`, 1))
  y        <- as.numeric(sapply(parts, `[`, 2))
  list(x = x, y = y, n = n_points)
}

# --- Discover all jpg+tem pairs ----------------------------------------------
jpg_files <- sort(list.files(SOURCE_DIR, pattern = "\\.jpg$", full.names = TRUE,
                              ignore.case = TRUE))
tem_files <- sub("\\.[Jj][Pp][Ee]?[Gg]$", ".tem", jpg_files)
has_tem   <- file.exists(tem_files)

message(sprintf("Found %d jpg files, %d have matching .tem files",
                length(jpg_files), sum(has_tem)))

jpg_files <- jpg_files[has_tem]
tem_files <- tem_files[has_tem]
n_total   <- length(jpg_files)

if (n_total == 0) stop("No paired jpg+tem files found in: ", SOURCE_DIR)
message(sprintf("Processing %d paired faces\n", n_total))

# --- Parse all .tem files ----------------------------------------------------
message("Parsing landmark files...")
landmarks <- lapply(tem_files, parse_tem)

n_pts <- sapply(landmarks, `[[`, "n")
if (length(unique(n_pts)) > 1) {
  warning("Point counts vary across faces — using only faces with the modal count")
  modal_n   <- as.integer(names(sort(table(n_pts), decreasing = TRUE)[1]))
  keep      <- which(n_pts == modal_n)
  jpg_files <- jpg_files[keep]
  tem_files <- tem_files[keep]
  landmarks <- landmarks[keep]
  n_total   <- length(jpg_files)
}
N_PTS <- landmarks[[1]]$n
message(sprintf("All faces: %d landmarks each\n", N_PTS))

# --- Compute mean shape -------------------------------------------------------
message("Computing mean landmark shape...")
all_x  <- do.call(rbind, lapply(landmarks, `[[`, "x"))   # n_total × N_PTS
all_y  <- do.call(rbind, lapply(landmarks, `[[`, "y"))
mean_x <- colMeans(all_x)
mean_y <- colMeans(all_y)
message(sprintf("Mean shape spans x=[%.0f, %.0f]  y=[%.0f, %.0f]\n",
                min(mean_x), max(mean_x), min(mean_y), max(mean_y)))

# --- Warp each face to mean shape using Shepards distortion ------------------
# Shepards control point format (per ImageMagick):
#   src_x1, src_y1, dst_x1, dst_y1, src_x2, src_y2, dst_x2, dst_y2, ...
# We warp FROM each face's landmark positions TO the mean shape.

message(sprintf("Warping %d faces to mean shape — this takes ~3-8 min total...\n",
                n_total))

face_ids <- character(n_total)
t_start  <- proc.time()["elapsed"]

for (i in seq_len(n_total)) {
  nm  <- tools::file_path_sans_ext(basename(jpg_files[i]))
  lm  <- landmarks[[i]]

  # Build 4×N_PTS matrix, then read column-by-column:
  # result = [sx1, sy1, dx1, dy1, sx2, sy2, dx2, dy2, ...]
  ctrl <- as.numeric(rbind(lm$x, lm$y, mean_x, mean_y))

  img <- image_read(jpg_files[i])

  # Save original (resized) → shown in the rating UI
  orig_out <- file.path(OUT_DIR, paste0(nm, ".png"))
  image_write(image_resize(img, "600x600!"), path = orig_out, format = "png")
  face_ids[i] <- paste0(nm, ".png")

  # Save Shepards-warped → used only by compute_composite() at runtime
  warped <- tryCatch({
    image_distort(img, "Shepards", ctrl, bestfit = FALSE)
  }, error = function(e) {
    message("  [!] Shepards failed for ", nm, ": ", conditionMessage(e),
            "\n      Warped copy = original.")
    img
  })
  warped_out <- file.path(WARPED_DIR, paste0(nm, ".png"))
  image_write(image_resize(warped, "600x600!"), path = warped_out, format = "png")

  if (i %% 10 == 0 || i == n_total) {
    elapsed <- proc.time()["elapsed"] - t_start
    message(sprintf("  %3d / %d   (%.0f s elapsed)", i, n_total, elapsed))
  }
}

# --- Write manifest -----------------------------------------------------------
index <- list(
  faces   = face_ids,
  width   = 600L,
  height  = 600L,
  n_total = as.integer(n_total)
)

write_json(index,
           path       = file.path(OUT_DIR, "faces_index.json"),
           pretty     = TRUE,
           auto_unbox = TRUE)

elapsed_total <- proc.time()["elapsed"] - t_start
message(sprintf(
  "\nDone! %d originals → %s\n      %d warped    → %s\n      faces_index.json written  (%.0f s total)",
  n_total, OUT_DIR, n_total, WARPED_DIR, elapsed_total
))
message("Launch app: shiny::runApp('shiny_app')")
