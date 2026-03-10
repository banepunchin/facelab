# =============================================================================
# generate_composites.R
# Pre-generates averaged face composites for the Face Averager Shiny app.
#
# METHOD: Fully local hybrid pipeline —
#   1. read_stim()        — load jpg+tem pairs
#   2. align(pt1=0,pt2=1) — rotate+scale each face so both eyes land at the
#                           same position (pure math, no XQuartz/rgl needed)
#   3. image_average()    — pixel-blend the eye-aligned faces
#
# This produces clean composites locally with no server or OpenGL dependency.
#
# REQUIRES:
#   install.packages(c("devtools", "magick", "jsonlite"))
#   devtools::install_github("debruine/webmorphR")  # or load_all() below
#
# HOW TO RUN:
#   Open RStudio with webmorphR-master as working directory, then:
#     source("generate_composites.R")
#
# ESTIMATED TIME: ~2–5 minutes (all local, no network calls)
# =============================================================================

# -- 0. Dependencies ----------------------------------------------------------

if (!requireNamespace("devtools",  quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("magick",    quietly = TRUE)) install.packages("magick")
if (!requireNamespace("jsonlite",  quietly = TRUE)) install.packages("jsonlite")

# Load webmorphR from the local source
devtools::load_all(".")

library(magick)
library(jsonlite)

# -- 1. Configuration ---------------------------------------------------------

SOURCE_DIR <- "/Users/hashiabdulle/Downloads/neutral_front"
OUT_DIR    <- "shiny_app/www/composites"

N_VALUES   <- c(1, 2, 5, 10, 20, 50, 100)
N_SEEDS    <- 3     # seeds per n value (n=1 always gets 1 seed)

set.seed(42)

# -- 2. Setup -----------------------------------------------------------------

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -- 3. Load all stimuli (jpg + tem pairs) ------------------------------------

message("Loading stimuli from: ", SOURCE_DIR)
all_stims <- read_stim(SOURCE_DIR)
n_total   <- length(all_stims)
message("Loaded ", n_total, " subjects.\n")

if (n_total == 0) stop("No stimuli found in SOURCE_DIR.")
N_VALUES <- N_VALUES[N_VALUES <= n_total]

# -- 4. Pre-generate random samples -------------------------------------------

samples <- list()
for (n in N_VALUES) {
  seeds <- if (n == 1) 1L else seq_len(N_SEEDS)
  for (s in seeds) {
    key <- paste0("n", n, "_s", s)
    samples[[key]] <- sample(seq_len(n_total), size = n, replace = FALSE)
  }
}

# -- 5. Generate composites ---------------------------------------------------

total_jobs <- length(samples)
job_num    <- 0

for (key in names(samples)) {
  job_num  <- job_num + 1
  out_file <- file.path(OUT_DIR, paste0("avg_", key, ".png"))

  if (file.exists(out_file)) {
    message("[", job_num, "/", total_jobs, "] Skipping (exists): ", basename(out_file))
    next
  }

  idx <- samples[[key]]
  n   <- length(idx)
  message("[", job_num, "/", total_jobs, "] n=", n, " → ", basename(out_file), " ...")

  subset_stims <- all_stims[idx]

  if (n == 1) {
    # Reference image: just save as-is, no averaging needed
    aligned <- subset_stims
  } else {
    # Two-point eye alignment: rotates and scales every face so that
    # points 0 and 1 (left/right eye) land at the same coordinates.
    # Pure local math — no geomorph, no rgl, no XQuartz needed.
    aligned <- tryCatch(
      align(subset_stims, pt1 = 0, pt2 = 1, fill = "white"),
      error = function(e) {
        message("  align() error: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(aligned)) { message("  Skipped."); next }
  }

  # Extract magick images from the stimlist and stack them
  imgs  <- lapply(aligned, function(s) s$img)
  stack <- do.call(c, imgs)

  # Pixel-wise average across the aligned stack
  composite <- if (n == 1) imgs[[1]] else image_average(stack)

  image_write(composite, path = out_file, format = "png")
  message("  Saved.")
}

# -- 6. Manifest --------------------------------------------------------------

seed_map <- list()
for (n in N_VALUES) {
  seeds <- if (n == 1) 1L else seq_len(N_SEEDS)
  seed_map[[as.character(n)]] <- as.integer(seeds)
}

manifest <- list(
  n_values  = as.integer(N_VALUES),
  n_seeds   = as.integer(N_SEEDS),
  n_total   = as.integer(n_total),
  seed_map  = seed_map,
  generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

write_json(manifest,
           path       = file.path(OUT_DIR, "manifest.json"),
           pretty     = TRUE,
           auto_unbox = TRUE)

message("\nDone! ", total_jobs, " composites saved to: ", OUT_DIR)
message("Manifest written to: ", file.path(OUT_DIR, "manifest.json"))
