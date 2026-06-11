# CLAUDE.md — Face Averager Project

## Project Goal
Replicate the interactive "Face Averager" from the Quartz averageness hypothesis article..
- Source: University of Glasgow Face Research / webmorphR library (Lisa DeBruine)
- Output: A fast, minimalist R Shiny app serving pre-generated composite images

---

## Architecture (Three-Phase Plan)

### Phase 1 — Batch Processing (R script, run locally)
- Write `generate_composites.R` to pre-generate averaged face composites at n = 2, 5, 10, 20, 50, etc.
- Uses `webmorphR::read_stim()` + `webmorphR::avg()` for shape-based averaging
- Output images land in `shiny_app/www/composites/`
- Heavy computation runs on user's machine; Claude goes on standby during this phase

### Phase 2 — Shiny App Build
- Single-file or `app.R` in `shiny_app/`
- Stack: R Shiny + `bslib` + custom CSS
- Design: high-end, minimalist — inspired by the Quartz article UI
- The app only reads pre-generated PNGs from `www/` — no live image processing

### Phase 3 — Deploy
- ShinyApps.io or local hosting; TBD

---

## Repository Structure

```
webmorphR-master/
├── R/                        # 48 source files — the full webmorphR package
├── man/                      # 101 documentation files
├── inst/                     # Package assets
├── tests/
├── vignettes/
├── DESCRIPTION               # Package v0.1.1, R >= 4.1.0
├── CLAUDE.md                 # ← this file
├── generate_composites.R     # (TO BE WRITTEN — Phase 1)
└── shiny_app/                # (TO BE WRITTEN — Phase 2)
    ├── app.R
    └── www/
        └── composites/       # Pre-generated PNGs land here
```

---

## Source Image Data

All folders live in `/Users/hashiabdulle/Downloads/`. Each contains ~102 subjects.
File naming: `{subject_id}_{pose_code}.jpg` (e.g. `001_03.jpg`)

| Folder | Pose Code | Expression | Angle | Has .TEM? |
|---|---|---|---|---|
| `neutral_front` | 03 | Neutral | Front | **YES** ← primary target |
| `neutral_left_profile` | 01 | Neutral | Left profile | No |
| `neutral_left_3quarter` | 02 | Neutral | Left 3/4 | No |
| `neutral_right_3quarter` | 04 | Neutral | Right 3/4 | No |
| `neutral_right_profile` | 05 | Neutral | Right profile | **YES** |
| `smiling_left_profile` | 06 | Smiling | Left profile | No |
| `smiling_left_3quarter` | 07 | Smiling | Left 3/4 | No |
| `smiling_right_3quarter` | 09 | Smiling | Right 3/4 | No |
| `smiling_right_profile` | 10 | Smiling | Right profile | No |
| `smiling_front` | 08 | Smiling | Front | TBD |

**Critical**: webmorphR's shape-based averaging (`avg()`) requires paired `.jpg` + `.tem` files.
Only `neutral_front` and `neutral_right_profile` have `.tem` landmark files.
**Primary dataset for the face averager = `neutral_front` (102 subjects, full tem data).**

---

## Key webmorphR Functions

| Function | File | Purpose |
|---|---|---|
| `read_stim()` | `R/read_stim.R` | Load jpg+tem pairs into a stimlist |
| `avg()` | `R/avg.R` | Average a stimlist into a composite |
| `average_tem()` | `R/average_tem.R` | Average landmark templates |
| `write_stim()` | `R/write_stim.R` | Save output images |
| `resize()` | `R/resize.R` | Resize for web |
| `mask_oval()` | `R/mask_oval.R` | Oval face mask for clean composites |

---

## Key Dependencies (from DESCRIPTION)
- magick (image processing backend)
- dplyr, ggplot2
- geomorph (landmark morphometrics)
- jsonlite, httr

---

## Composite Generation Strategy
```r
# Pseudocode — details TBD in generate_composites.R
stims <- read_stim("/Users/hashiabdulle/Downloads/neutral_front")
for n in c(2, 5, 10, 20, 50, 102):
  sample_n_subjects → avg() → write_stim("shiny_app/www/composites/avg_{n}.png")
```
Run multiple seeds per n for variability if desired.

---

## Shiny App UI Concept
- Single slider or click-through control: "Average of N faces"
- Image swaps instantly (pre-loaded PNGs, no computation)
- Side-by-side option: single face vs. composite
- Caption with brief averageness hypothesis explanation
- Dark or neutral background; large centered image display

---

## Status
- [x] Repo scanned and understood
- [x] CLAUDE.md written
- [ ] `generate_composites.R` written (Phase 1)
- [ ] Composites generated (user runs script locally)
- [ ] `shiny_app/app.R` written (Phase 2)
- [ ] App styled with bslib + CSS (Phase 2)
- [ ] Deployed (Phase 3)
