source("scripts/00_settings.R")

        #### LISTING API ####

api <- "https://www.udir.no/api/FagkoderApi/NyeFagkoder"

fetch_page <- function(page, program = "ST") {
  request(api) |>
    req_method("POST") |>
    req_body_json(list(
      currentPageNumber = page,
      checkedValues = list(),
      selectedValue = program
    )) |>
    req_headers(Accept = "application/json") |>
    req_user_agent("research scraper; you@uio.no") |>
    req_throttle(rate = 2) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

# probe page 1 to get the total
probe   <- fetch_page(1)
total   <- probe$payload$numberOfResults
n_pages <- ceiling(total / 25)

all_st <- map_dfr(seq_len(n_pages), function(p) {
  message("page ", p, "/", n_pages)
  ns <- fetch_page(p)$payload$results$newSubject
  as_tibble(ns)
}) |>
  distinct()

nrow(all_st)    # expect 850
names(all_st)   # check which column holds the fagkode

        #### STANDPUNKT FLAG ####

code_col  <- "code"   
grep_base <- "https://data.udir.no/kl06/v201906/fagkoder/"

# each grep record has a vurderingsordning array with one entry per elevtype.
# we want the pupil entry, not the privatist one
get_standpunkt <- function(kode) {
  tryCatch({
    rec <- request(paste0(grep_base, toupper(kode))) |>
      req_headers(Accept = "application/json") |>
      req_user_agent("research scraper; you@uio.no") |>
      req_throttle(rate = 4) |>
      req_retry(max_tries = 3) |>
      req_cache(file.path(tempdir(), "grep_cache")) |>
      req_perform() |>
      resp_body_json(simplifyVector = FALSE)
    
    vo <- rec$vurderingsordning
    if (length(vo) == 0) return(NA_character_)
    
    elev <- keep(vo, function(x) {
      !is.null(x$elevtype) && grepl("eksamen_vurdering_elev$", x$elevtype)
    })
    if (length(elev) == 0) return(NA_character_)
    
    if (isTRUE(elev[[1]]$standpunktvurdering)) "Ja" else "Nei"
  },
  error = function(e) NA_character_)
}

all_st$standpunkt <- map_chr(all_st[[code_col]], get_standpunkt, .progress = TRUE)

table(all_st$standpunkt, useNA = "ifany")

write.csv(all_st, "data.temp/udir_fagkoder_ST.csv", row.names = FALSE)
