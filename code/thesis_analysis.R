# ============================================================================
# thesis_analysis.R  (original name: Script_licenta.R)
# ----------------------------------------------------------------------------
# Lucrare licenta: Analiza si previziunea consumului electric in tarile
# europene.
#
# Bachelor's thesis: Analysis and forecasting of electricity consumption in
# European countries. Run from the repository root, e.g.:
#   setwd("path/to/electricity-consumption-forecasting-europe")
#   source("code/thesis_analysis.R")
#
# Toate graficele sunt afisate in panoul de plot inainte de a fi salvate.
# Dupa fiecare grafic, codul afiseaza un comentariu ghid si asteapta ENTER.
# ============================================================================

t_start <- Sys.time()
cat("\n############################################\n")
cat(" Pornit la:", format(t_start), "\n")
cat("############################################\n")

if (!interactive()) {
  cat("\n[ATENTIE] Nu rulezi in mod interactiv (interactive() == FALSE).\n")
  cat("Acest script foloseste readline() si afiseaza grafice.\n")
  cat("Recomandare: deschide RStudio si ruleaza cu source(\"code/thesis_analysis.R\").\n")
  cat("Daca continui din Rscript, input-urile vor fi citite de la stdin si\n")
  cat("graficele nu vor fi vizibile in panou (doar salvate ca PNG).\n\n")
}

# ============================================================================
# SECTIUNEA 0  -  SETUP (pachete, cai, helperi)
# ============================================================================

pachete_necesare <- c(
  "eurostat",
  "ggplot2",
  "tseries",   # adf.test, pp.test
  "urca",      # ur.kpss, ur.df, ca.jo
  "uroot",     # ch.test (Canova-Hansen)
  "vars",
  "lmtest",
  "forecast",
  "lubridate",
  "tidyr",
  "dplyr"
)
for (p in pachete_necesare) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
invisible(lapply(pachete_necesare, library, character.only = TRUE))

# ----------------------------------------------------------------------------
# Detectie motor ajustare sezoniera (TRAMO-SEATS via RJDemetra cu Java,
# fallback X-13ARIMA-SEATS via seasonal/x13binary).
# ----------------------------------------------------------------------------
MOTOR_SA <- tryCatch({
  requireNamespace("RJDemetra", quietly = TRUE) &&
    !inherits(try(rJava::.jnew("java.lang.Object"), silent = TRUE),
              "try-error")
}, error = function(e) FALSE)

if (MOTOR_SA) {
  library(RJDemetra)
  MOTOR_SA_NUME <- "TRAMO-SEATS"
  cat("Motor ajustare sezoniera: RJDemetra (TRAMO-SEATS)\n")
} else {
  if (!requireNamespace("seasonal",  quietly = TRUE))
    install.packages("seasonal")
  if (!requireNamespace("x13binary", quietly = TRUE))
    install.packages("x13binary")
  library(seasonal); library(x13binary)
  MOTOR_SA_NUME <- "X-13ARIMA-SEATS"
  cat("Motor ajustare sezoniera: seasonal/x13binary (X-13ARIMA-SEATS)\n")
}

select <- dplyr::select
filter <- dplyr::filter
lag    <- dplyr::lag

# Cai relative la radacina repository-ului (relative to the repository root)
DIR_PROIECT      <- "."
DIR_REZULTATE    <- file.path(DIR_PROIECT, "results")
DIR_DATE         <- file.path(DIR_PROIECT, "data")
DIR_UNIVARIATA   <- file.path(DIR_REZULTATE, "univariate")
DIR_MULTIVARIATA <- file.path(DIR_REZULTATE, "multivariate")
DIR_SUMAR        <- file.path(DIR_REZULTATE, "summary")
for (d in c(DIR_REZULTATE, DIR_DATE, DIR_UNIVARIATA,
            DIR_MULTIVARIATA, DIR_SUMAR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

COD_AGREGATE <- c(
  "EU27_2020", "EU28", "EU27_2007", "EU15", "EU",
  "EA20", "EA19", "EA18", "EA",
  "EEA", "EEA30", "EEA31"
)
DATA_INCEPUT <- as.Date("2008-01-01")
DATA_SFARSIT <- as.Date("2025-12-02")
MIN_OBS      <- 216

# ----------------------------------------------------------------------------
# Helperi standard (formatare, salvare grafice fara afisare)
# ----------------------------------------------------------------------------

salveaza_ggplot <- function(grafic, cale, latime = 10, inaltime = 6, dpi = 150) {
  ggsave(filename = cale, plot = grafic,
         width = latime, height = inaltime, dpi = dpi, bg = "white")
}

salveaza_base_plot <- function(cale, expresie_plot,
                               latime = 10, inaltime = 6, dpi = 150) {
  caller_env <- parent.frame()
  png(filename = cale, width = latime, height = inaltime,
      units = "in", res = dpi, bg = "white")
  tryCatch(eval(expresie_plot, envir = caller_env), finally = dev.off())
}

construieste_ts <- function(valori, data_inceput) {
  an   <- as.numeric(format(data_inceput, "%Y"))
  luna <- as.numeric(format(data_inceput, "%m"))
  ts(valori, start = c(an, luna), frequency = 12)
}

fmt <- function(x, zecimale = 4) {
  if (is.null(x) || length(x) == 0) return("NA")
  if (is.na(x)) return("NA")
  formatC(x, format = "f", digits = zecimale)
}

# ----------------------------------------------------------------------------
# Helperi pentru INTERACTIVITATE
# ----------------------------------------------------------------------------

# Citeste un input de la utilizator. Functioneaza atat in sesiune interactiva
# (readline) cat si in Rscript (stdin printr-un conector persistent, ca sa
# nu se piarda pozitia intre apeluri).
citeste_input <- function(prompt = "") {
  if (interactive()) {
    return(readline(prompt))
  }
  cat(prompt)
  flush.console()
  if (!exists(".STDIN_CONN", envir = globalenv())) {
    assign(".STDIN_CONN",
           file("stdin", open = "r", blocking = TRUE),
           envir = globalenv())
  }
  raspuns <- tryCatch(
    readLines(get(".STDIN_CONN", envir = globalenv()),
              n = 1, warn = FALSE),
    error = function(e) character(0))
  if (length(raspuns) == 0) "" else raspuns
}

# Considera "y", "yes", "da", "d" sau ENTER gol drept raspuns afirmativ.
raspuns_da <- function(text) {
  tolower(trimws(text)) %in% c("y", "yes", "da", "d", "")
}

# Afiseaza un comentariu ghid si asteapta apasarea tastei ENTER.
ghid_si_pauza <- function(linii) {
  cat("\n")
  for (l in linii) cat(l, "\n")
  invisible(citeste_input("\nApasa ENTER pentru a continua..."))
}

# Bateria extinsa de teste de stationaritate aplicata pe o serie.
# Afiseaza in consola si returneaza o lista cu p-values / statistici.
#  - adf_simple : tseries::adf.test (cu intercept si trend implicit, lag auto)
#  - pp         : tseries::pp.test (Phillips-Perron)
#  - kpss       : urca::ur.kpss type="mu"
#  - adf_none   : urca::ur.df type="none"
#  - adf_drift  : urca::ur.df type="drift"
#  - adf_trend  : urca::ur.df type="trend"
teste_stationaritate_extinse <- function(serie, eticheta) {
  cat("\n--- Teste stationaritate:", eticheta, "---\n")

  # ADF simplu (tseries) si KPSS - rezumat scurt
  adf_simple_p <- tryCatch(adf.test(serie)$p.value, error = function(e) NA)
  kpss_stat    <- tryCatch(
    summary(ur.kpss(serie, type = "mu"))@teststat,
    error = function(e) NA)
  pp_obj <- tryCatch(pp.test(serie), error = function(e) NULL)
  pp_p   <- if (!is.null(pp_obj)) pp_obj$p.value else NA
  pp_stat <- if (!is.null(pp_obj)) pp_obj$statistic else NA

  cat(sprintf("ADF simplu (tseries) p = %s  (H0: nestationar; p<0.05 = stationara)\n",
              fmt(adf_simple_p)))
  cat(sprintf("Phillips-Perron     p = %s  stat = %s  (H0: radacina unitara)\n",
              fmt(pp_p), fmt(pp_stat)))
  cat(sprintf("PP concluzie        : %s\n",
              if (!is.na(pp_p) && pp_p < 0.05) "STATIONARA" else "NESTATIONARA"))
  cat(sprintf("KPSS stat           = %s  (crit. 5%% = 0.463; mai mic = stationara)\n",
              fmt(kpss_stat)))

  # ADF extins cu 3 specificatii (urca::ur.df)
  cat("\nADF detaliat (urca::ur.df) cu trei specificatii:\n")
  cat("  type=none  : random walk pur (fara constanta, fara trend)\n")
  cat("  type=drift : random walk cu drift (constanta nenula)\n")
  cat("  type=trend : random walk cu trend determinist liniar\n")
  cat("  Pentru consum electric, drift si trend sunt cele mai relevante.\n\n")

  ur_df_none  <- tryCatch(ur.df(serie, type = "none",  selectlags = "AIC"),
                          error = function(e) NULL)
  ur_df_drift <- tryCatch(ur.df(serie, type = "drift", selectlags = "AIC"),
                          error = function(e) NULL)
  ur_df_trend <- tryCatch(ur.df(serie, type = "trend", selectlags = "AIC"),
                          error = function(e) NULL)

  afis_ur_df <- function(obj, eticheta) {
    cat(">>>", eticheta, "<<<\n")
    if (is.null(obj)) {
      cat("(eroare la estimare)\n\n")
      return(NULL)
    }
    sm <- summary(obj)
    tau <- sm@teststat[1]
    cv  <- sm@cval[1, ]
    decizie <- if (tau < cv["5pct"])
      "RESPINGEM H0 (stationara la 5%)" else
      "NU respingem H0 (nestationara la 5%)"
    cat(sprintf("  Statistica tau = %s\n", fmt(tau)))
    cat(sprintf("  Valori critice: 1%% = %s | 5%% = %s | 10%% = %s\n",
                fmt(cv["1pct"]), fmt(cv["5pct"]), fmt(cv["10pct"])))
    cat(sprintf("  Decizie 5%%: %s\n\n", decizie))
    list(tau = as.numeric(tau),
         cv5 = as.numeric(cv["5pct"]),
         stat = !is.na(tau) && tau < cv["5pct"])
  }

  cat("ADF fara constanta si trend (type=none)\n")
  rez_none  <- afis_ur_df(ur_df_none,  "ur.df type=none")
  cat("ADF cu drift/constanta (type=drift)\n")
  rez_drift <- afis_ur_df(ur_df_drift, "ur.df type=drift")
  cat("ADF cu trend determinist (type=trend)\n")
  rez_trend <- afis_ur_df(ur_df_trend, "ur.df type=trend")

  list(adf_simple_p = adf_simple_p,
       kpss_stat    = as.numeric(kpss_stat),
       pp_p         = pp_p,
       pp_stat      = as.numeric(pp_stat),
       adf_none     = rez_none,
       adf_drift    = rez_drift,
       adf_trend    = rez_trend)
}

# Grafic combinat ACF + PACF intr-o singura figura (2 panouri suprapuse).
afiseaza_si_salveaza_acf_pacf_combo <- function(acf_obj, pacf_obj, cale,
                                                titlu_acf, titlu_pacf,
                                                latime = 10, inaltime = 9,
                                                dpi = 150) {
  expresie <- bquote({
    par(mfrow = c(2, 1))
    plot(.(acf_obj),  main = .(titlu_acf))
    plot(.(pacf_obj), main = .(titlu_pacf))
  })
  afiseaza_si_salveaza_base(expresie, cale,
                            latime = latime, inaltime = inaltime, dpi = dpi)
}

# Daca panoul Plots din RStudio este prea mic pentru base plots (ACF, PACF,
# STL etc.) si tot apare "figure margins too large", lasam TRUE. Pune FALSE daca preferi sa fortezi totul
# in panoul RStudio si rezolvi marirea manual.
FALLBACK_FEREASTRA <- TRUE

# Inchide doar device-urile de fisier (png/pdf/...) ramase deschise din
# erori sau salvari anterioare. NU inchide windows() sau RStudioGD.
inchide_devices_fisier <- function() {
  while (dev.cur() > 1L &&
         tolower(names(dev.cur())) %in%
           c("png", "pdf", "bmp", "jpeg", "tiff",
             "cairo_pdf", "cairo_ps")) {
    try(dev.off(), silent = TRUE)
  }
}

# par() compact pentru panoul Plots din RStudio (cand spatiul e limitat).
# Top margin = 2.8 linii: suficient pentru titlu, fara a fi atat de mare incat
# sa cauzeze "figure margins too large" in pane-uri mici.
reseteaza_par_compact <- function() {
  try(par(mfrow = c(1, 1),
          mar      = c(3, 3, 2.8, 0.5),
          mgp      = c(1.6, 0.5, 0),
          oma      = c(0, 0, 0, 0),
          cex.main = 1.0,
          font.main = 2),  # bold pentru titlu
      silent = TRUE)
}

# par() generos pentru fereastra separata windows() (12x7 inch). Avem
# spatiu suficient, asa ca dam titlu mare, citibil si margini lejere.
reseteaza_par_normal <- function() {
  try(par(mfrow = c(1, 1),
          mar      = c(4.5, 4.5, 3.5, 1.5),
          mgp      = c(2.5, 0.8, 0),
          oma      = c(0, 0, 0, 0),
          cex.main = 1.3,
          cex.lab  = 1.1,
          cex.axis = 1.0,
          font.main = 2),
      silent = TRUE)
}

# Pastram reseteaza_par() pentru compatibilitate cu apelurile vechi.
reseteaza_par <- function() reseteaza_par_compact()

# Identifica tipul device-ului activ.
# Returneaza "popup" pentru windows/quartz/x11 (fereastra externa),
# "pane" pentru RStudioGD si "null" daca nu exista device deschis.
tip_device_activ <- function() {
  dc <- dev.cur()
  if (dc <= 1L) return("null")
  nume <- tolower(names(dc))
  if (nume %in% c("windows", "x11", "quartz", "cairo"))
    return("popup")
  if (grepl("rstudio", nume))
    return("pane")
  if (nume %in% c("png", "pdf", "bmp", "jpeg", "tiff",
                  "cairo_pdf", "cairo_ps"))
    return("fisier")
  "pane"
}

# Reset complet inainte de un grafic nou. Aplica par() adaptiv:
#  - daca device-ul activ este o fereastra externa, folosim margini normale
#    (titlu si etichete clare)
#  - daca este pane-ul RStudio, folosim margini compacte
reseteaza_grafic <- function() {
  if (!interactive()) return(invisible(NULL))
  inchide_devices_fisier()
  if (tip_device_activ() == "popup") {
    reseteaza_par_normal()
  } else {
    reseteaza_par_compact()
  }
  invisible(NULL)
}

# Deschide o fereastra separata SAU refoloseste una existenta.
# Daca exista deja o fereastra windows()/quartz()/x11() in dev.list(),
# o reactiveaza prin dev.set() in loc sa deschida una noua. Acest lucru
# evita acumularea de ferestre (cate una per plot) si problema cu
# "graficele urmatoare nu se afiseaza" cauzata de schimbarea device-ului
# activ intre plotari.
deschide_fereastra_separata <- function() {
  if (!interactive()) return(FALSE)
  tryCatch({
    dl <- dev.list()
    win_idx <- which(tolower(names(dl)) %in%
                       c("windows", "x11", "quartz"))
    if (length(win_idx) > 0) {
      # Refoloseste fereastra existenta
      dev.set(dl[win_idx[1]])
    } else {
      # Deschide una noua
      if (.Platform$OS.type == "windows") {
        grDevices::windows(width = 12, height = 7)
      } else if (Sys.info()[["sysname"]] == "Darwin") {
        grDevices::quartz(width = 12, height = 7)
      } else {
        grDevices::x11(width = 12, height = 7)
      }
    }
    reseteaza_par_normal()
    # Adu fereastra in fata (Windows-only)
    if (.Platform$OS.type == "windows") {
      try(grDevices::bringToTop(stay = FALSE), silent = TRUE)
    }
    TRUE
  }, error = function(e) {
    cat("(Nu pot deschide/reactiva fereastra separata:",
        conditionMessage(e), ")\n")
    FALSE
  })
}

# Forteaza randarea plotului curent (relevant in special pentru windows()
# care e buffered pe Windows).
forteaza_render <- function() {
  if (!interactive()) return(invisible(NULL))
  try(grDevices::dev.flush(), silent = TRUE)
  invisible(NULL)
}

# Afiseaza un ggplot pe device-ul potrivit + il salveaza ca PNG.
# Strategia:
#  - Daca exista deja o fereastra externa (am intrat in "mod popup" dintr-un
#    plot anterior), o refolosim direct, nu mai incercam pane.
#  - Altfel incercam pane RStudio; daca esueaza, fallback la fereastra.
afiseaza_si_salveaza_ggplot <- function(grafic, cale,
                                        latime = 10, inaltime = 6, dpi = 150) {
  if (interactive()) {
    # Daca am o fereastra externa deja deschisa, o folosim direct
    dl <- dev.list()
    are_popup <- length(dl) > 0 &&
      any(tolower(names(dl)) %in% c("windows", "x11", "quartz"))

    if (are_popup && FALLBACK_FEREASTRA) {
      deschide_fereastra_separata()  # reactiveaza + bringToTop
      rezultat <- tryCatch({ print(grafic); "ok" },
                           error = function(e) conditionMessage(e))
    } else {
      reseteaza_grafic()
      rezultat <- tryCatch({ print(grafic); "ok" },
                           error = function(e) conditionMessage(e))
      if (!identical(rezultat, "ok") && FALLBACK_FEREASTRA) {
        cat("(ggplot in pane a esuat; deschid fereastra separata)\n")
        if (deschide_fereastra_separata()) {
          rezultat <- tryCatch({ print(grafic); "ok" },
                               error = function(e) conditionMessage(e))
        }
      }
    }

    if (!identical(rezultat, "ok")) {
      cat("(Nu pot afisa ggplot:", rezultat, ")\n")
    }
    forteaza_render()
  }
  salveaza_ggplot(grafic, cale, latime, inaltime, dpi)
}

# Afiseaza un grafic base R pe device-ul potrivit + il salveaza ca PNG.
# Aceeasi strategie ca la ggplot: daca exista deja fereastra externa
# deschisa, o refolosim direct; altfel pane cu fallback la fereastra.
afiseaza_si_salveaza_base <- function(expresie_plot, cale,
                                      latime = 10, inaltime = 6, dpi = 150) {
  caller_env <- parent.frame()

  if (interactive()) {
    incerca_plot <- function() {
      tryCatch({ eval(expresie_plot, envir = caller_env); "ok" },
               error = function(e) conditionMessage(e))
    }

    dl <- dev.list()
    are_popup <- length(dl) > 0 &&
      any(tolower(names(dl)) %in% c("windows", "x11", "quartz"))

    if (are_popup && FALLBACK_FEREASTRA) {
      # Refoloseste fereastra externa existenta
      deschide_fereastra_separata()
      rezultat <- incerca_plot()
    } else {
      # Prima incercare in pane-ul RStudio
      reseteaza_grafic()
      rezultat <- incerca_plot()

      # Fallback la fereastra separata daca pane esueaza
      if (!identical(rezultat, "ok") && FALLBACK_FEREASTRA) {
        cat("(Pane Plots prea mic pentru titlu;",
            "deschid fereastra separata)\n")
        if (deschide_fereastra_separata()) {
          rezultat <- incerca_plot()
        }
      }
    }

    if (!identical(rezultat, "ok")) {
      cat("(Nu pot afisa base plot:", rezultat, ")\n")
    }
    forteaza_render()

    # NU resetam par() la compact la final: daca am ramas pe fereastra
    # externa, urmatorul plot trebuie sa beneficieze de marginile generoase.
    # reseteaza_grafic() de la inceputul urmatoarei plotari va decide
    # singur ce par() sa aplice (compact pt pane, normal pt popup).
    inchide_devices_fisier()
  }

  # Salvare PNG independent de afisare (titlu lizibil cu margini lejere)
  png(filename = cale, width = latime, height = inaltime,
      units = "in", res = dpi, bg = "white")
  tryCatch({
    par(mfrow    = c(1, 1),
        mar      = c(4.5, 4.5, 3.5, 1.5),
        mgp      = c(2.5, 0.8, 0),
        cex.main = 1.3,
        cex.lab  = 1.1,
        cex.axis = 1.0,
        font.main = 2)
    eval(expresie_plot, envir = caller_env)
  }, finally = dev.off())
}

# Tabel ACF/PACF pana la lag 36 cu marcarea lagurilor semnificative.
afiseaza_tabel_acf_pacf <- function(acf_vals, pacf_vals, n_serie) {
  prag <- 1.96 / sqrt(n_serie)
  cat(sprintf("\nPragul de semnificatie 95%% (1.96 / sqrt(n)): +/-%.4f\n", prag))
  cat("\nLag |   ACF    | sig | PACF     | sig\n")
  cat("----+----------+-----+----------+-----\n")
  for (lg in 1:36) {
    a <- if (length(acf_vals)  >= lg + 1) acf_vals[lg + 1] else NA
    p <- if (length(pacf_vals) >= lg)     pacf_vals[lg]    else NA
    sig_a <- if (!is.na(a) && abs(a) > prag) "*" else " "
    sig_p <- if (!is.na(p) && abs(p) > prag) "*" else " "
    cat(sprintf(" %2d | %8.4f |  %s  | %8.4f |  %s\n",
                lg,
                if (is.na(a)) 0 else a, sig_a,
                if (is.na(p)) 0 else p, sig_p))
  }
  cat("\nLagurile marcate cu * depasesc pragul de semnificatie.\n")
  invisible(prag)
}

# Citeste un ordin SARIMA cu validare in intervalul [0, max_val].
citeste_ordin <- function(prompt, max_val) {
  while (TRUE) {
    raspuns <- trimws(citeste_input(prompt))
    if (raspuns == "") return(0L)
    val <- suppressWarnings(as.integer(raspuns))
    if (!is.na(val) && val >= 0 && val <= max_val) return(val)
    cat("Valoare invalida. Introdu un intreg intre 0 si", max_val,
        "(sau apasa ENTER pentru 0).\n")
  }
}

# Cere ordinele p, q, P, Q cu confirmare.
input_ordine_sarima <- function(d, D) {
  cat("\n")
  repeat {
    p <- citeste_ordin("p (AR non-sezonier) [0-3]: ", 3)
    q <- citeste_ordin("q (MA non-sezonier) [0-3]: ", 3)
    P <- citeste_ordin("P (AR sezonier)    [0-2]: ",  2)
    Q <- citeste_ordin("Q (MA sezonier)    [0-2]: ",  2)
    cat(sprintf("\nSpecificatie propusa: SARIMA(%d,%d,%d)(%d,%d,%d)[12]\n",
                p, d, q, P, D, Q))
    if (raspuns_da(citeste_input("Confirmi specificatia? (y/n): "))) break
    cat("Reintroducem ordinele.\n")
  }
  list(p = p, q = q, P = P, Q = Q)
}

# ----------------------------------------------------------------------------
# Ajustare sezoniera (TRAMO-SEATS sau X-13).
#  - detecteaza sezonalitatea cu Canova-Hansen (uroot::ch.test) pe seria in
#    nivel; daca p_joint < 0.05, aplica SA, altfel returneaza originalul
#  - in caz de eroare la motor (Java crash, serie scurta, NA-uri),
#    fallback la seria originala
#  - dir_t opcional: daca este dat, salveaza un grafic de decompozitie
# ----------------------------------------------------------------------------
ajusteaza_sezonier <- function(serie_ts, eticheta, dir_t = NULL) {
  # Detectie sezonalitate: Canova-Hansen, |ACF[12]| pe seria diferentiata,
  # putere sezoniera STL
  ch_p <- tryCatch({
    ch_res <- uroot::ch.test(serie_ts, type = "dummy", lag1 = TRUE)
    as.numeric(ch_res$pvalues[length(ch_res$pvalues)])
  }, error = function(e) NA)
  acf_12_diff <- tryCatch({
    d1 <- diff(serie_ts)
    abs(as.numeric(acf(d1, lag.max = 12, plot = FALSE)$acf[13]))
  }, error = function(e) 0)
  prag_acf <- 2 / sqrt(length(serie_ts) - 1)
  putere_sez <- tryCatch({
    dec <- stl(serie_ts, s.window = "periodic")
    sea <- dec$time.series[, "seasonal"]; rem <- dec$time.series[, "remainder"]
    max(0, 1 - var(rem, na.rm = TRUE) / var(sea + rem, na.rm = TRUE))
  }, error = function(e) 0)

  are_sezonalitate <- (!is.na(ch_p) && ch_p < 0.05) ||
                      (!is.na(acf_12_diff) && acf_12_diff > prag_acf) ||
                      (!is.na(putere_sez) && putere_sez > 0.3)

  if (!are_sezonalitate) {
    cat(sprintf("[SA %s] Fara sezonalitate (CH p=%s, |ACF[12] diff|=%s, F_sez=%s). Sar.\n",
                eticheta, fmt(ch_p), fmt(acf_12_diff), fmt(putere_sez)))
    return(list(sa = serie_ts, original = serie_ts,
                a_fost_ajustata = FALSE, motor = "niciuna",
                test_canova_hansen_p = ch_p))
  }

  # Aplica motor disponibil; fallback la original daca esueaza
  rez <- tryCatch({
    if (MOTOR_SA) {
      spec <- RJDemetra::tramoseats_spec("RSA5")
      mod <- RJDemetra::tramoseats(serie_ts, spec)
      ts_sa <- mod$final$series[, "sa"]
      list(sa = ts_sa, motor = "TRAMO-SEATS")
    } else {
      sea <- seasonal::seas(serie_ts)
      list(sa = seasonal::final(sea), motor = "X-13")
    }
  }, error = function(e) {
    cat(sprintf("[SA %s] Eroare motor: %s. Folosesc seria originala.\n",
                eticheta, conditionMessage(e)))
    list(sa = serie_ts, motor = "esuat -> original")
  })

  cat(sprintf("[SA %s] Aplicat cu %s (CH joint p = %s).\n",
              eticheta, rez$motor, fmt(ch_p)))

  # Grafic decompozitie (original suprapus cu SA)
  if (!is.null(dir_t)) {
    tryCatch({
      df <- data.frame(
        timp    = as.numeric(rep(time(serie_ts), 2)),
        valoare = c(as.numeric(serie_ts), as.numeric(rez$sa)),
        tip     = factor(rep(c("Original", "SA"), each = length(serie_ts)),
                         levels = c("Original", "SA")))
      g <- ggplot(df, aes(x = timp, y = valoare, color = tip)) +
        geom_line(linewidth = 0.7) +
        scale_color_manual(values = c("Original" = "grey50",
                                      "SA"       = "steelblue")) +
        ggtitle(sprintf("Ajustare sezoniera (%s) - %s",
                        rez$motor, eticheta)) +
        xlab("Timp") + ylab("Valoare") +
        theme_minimal() + theme(legend.position = "bottom")
      cale_png <- file.path(dir_t,
                            paste0("00_sa_", gsub(" ", "_", eticheta), ".png"))
      afiseaza_si_salveaza_ggplot(g, cale_png)
    }, error = function(e)
      cat("Eroare grafic SA:", conditionMessage(e), "\n"))
  }

  list(sa = rez$sa, original = serie_ts,
       a_fost_ajustata = (rez$motor != "esuat -> original"),
       motor = rez$motor,
       test_canova_hansen_p = ch_p)
}

cat("Setup incarcat. Directorul proiectului:", DIR_PROIECT, "\n")


# ============================================================================
# SECTIUNEA 1  -  PREGATIREA DATELOR (Eurostat) - AUTOMATA
# ============================================================================

cat("\n========================================\n")
cat(" PARTEA 1: PREGATIREA DATELOR\n")
cat("========================================\n")

cat("\nDescarcare nrg_cb_em (consum electricitate)...\n")
elec_raw <- get_eurostat("nrg_cb_em", time_format = "date",
                         cache = TRUE, update_cache = FALSE)
cat("Descarcare sts_inpr_m (indice productie industriala)...\n")
ipi_raw <- get_eurostat("sts_inpr_m", time_format = "date",
                        cache = TRUE, update_cache = FALSE)
cat("Descarcare nrg_chdd_m (HDD si CDD)...\n")
chdd_raw <- get_eurostat("nrg_chdd_m", time_format = "date",
                         cache = TRUE, update_cache = FALSE)

normalizeaza_coloana_timp <- function(df) {
  if (!"TIME_PERIOD" %in% names(df) && "time" %in% names(df)) {
    df <- df %>% dplyr::rename(TIME_PERIOD = time)
  }
  df
}
elec_raw <- normalizeaza_coloana_timp(elec_raw)
ipi_raw  <- normalizeaza_coloana_timp(ipi_raw)
chdd_raw <- normalizeaza_coloana_timp(chdd_raw)

unitati_disponibile <- unique(elec_raw$unit)
cat("Unitati disponibile in nrg_cb_em:",
    paste(unitati_disponibile, collapse = ", "), "\n")
unitate_elec <- if ("GWH" %in% unitati_disponibile) "GWH" else unitati_disponibile[1]

are_siec <- "siec" %in% names(elec_raw)
elec <- elec_raw %>%
  dplyr::filter(nrg_bal == "AIM",
                unit == unitate_elec,
                TIME_PERIOD >= DATA_INCEPUT,
                TIME_PERIOD <= DATA_SFARSIT)
if (are_siec) {
  elec_e7000 <- elec %>% dplyr::filter(siec == "E7000")
  if (nrow(elec_e7000) > 0) {
    elec <- elec_e7000
  } else {
    cat("Filtrul siec=E7000 a returnat zero randuri. Folosim fara siec.\n")
  }
}
elec <- elec %>%
  dplyr::transmute(geo, data = TIME_PERIOD, elec_gwh = values)

cat("Observatii consum electricitate dupa filtrare:", nrow(elec), "\n")

ipi <- ipi_raw %>%
  dplyr::filter(s_adj == "SCA",
                nace_r2 == "B-D",
                unit == "I21",
                TIME_PERIOD >= DATA_INCEPUT,
                TIME_PERIOD <= DATA_SFARSIT) %>%
  dplyr::transmute(geo, data = TIME_PERIOD, ipi = values)
cat("Observatii IPI dupa filtrare:", nrow(ipi), "\n")

chdd <- chdd_raw %>%
  dplyr::filter(indic_nrg %in% c("HDD", "CDD"),
                TIME_PERIOD >= DATA_INCEPUT,
                TIME_PERIOD <= DATA_SFARSIT) %>%
  dplyr::transmute(geo, data = TIME_PERIOD,
                   indic_nrg, valoare = values) %>%
  tidyr::pivot_wider(names_from = indic_nrg, values_from = valoare) %>%
  dplyr::rename_with(tolower, dplyr::any_of(c("HDD", "CDD"))) %>%
  dplyr::mutate(tdd = hdd + cdd)
cat("Observatii HDD/CDD dupa filtrare:", nrow(chdd), "\n")

panel <- elec %>%
  dplyr::inner_join(ipi,  by = c("geo", "data")) %>%
  dplyr::inner_join(chdd, by = c("geo", "data")) %>%
  dplyr::arrange(geo, data)

cat("\nPanel imbinat: ", nrow(panel), "observatii,",
    length(unique(panel$geo)), "tari (inclusiv agregate).\n")

panel <- panel %>% dplyr::filter(!geo %in% COD_AGREGATE)
panel <- panel %>%
  dplyr::group_by(geo) %>%
  dplyr::arrange(data, .by_group = TRUE) %>%
  dplyr::mutate(
    randuri = dplyr::row_number(),
    randuri_max = max(randuri),
    elimina_coada = is.na(tdd) & randuri > (randuri_max - 3)
  ) %>%
  dplyr::filter(!elimina_coada) %>%
  dplyr::select(-randuri, -randuri_max, -elimina_coada) %>%
  dplyr::ungroup()

diagnostic_tari <- panel %>%
  dplyr::group_by(geo) %>%
  dplyr::summarise(
    nr_obs = dplyr::n(),
    na_elec = sum(is.na(elec_gwh)),
    na_ipi  = sum(is.na(ipi)),
    na_tdd  = sum(is.na(tdd)),
    data_min = min(data),
    data_max = max(data),
    .groups = "drop"
  )
cat("\nDiagnostic per tara (top 30):\n")
print(diagnostic_tari, n = 30)

tari_complete <- diagnostic_tari %>%
  dplyr::filter(na_elec == 0, na_ipi == 0, na_tdd == 0,
                nr_obs >= MIN_OBS) %>%
  dplyr::pull(geo)

cat("\nTari selectate (date complete, >=", MIN_OBS, "obs.):",
    length(tari_complete), "\n")
cat(paste(tari_complete, collapse = ", "), "\n")

panel <- panel %>% dplyr::filter(geo %in% tari_complete)
write.csv(panel, file.path(DIR_DATE, "panel_clean.csv"), row.names = FALSE)
write.csv(diagnostic_tari, file.path(DIR_DATE, "country_coverage.csv"),
          row.names = FALSE)

TARI_ANALIZA <- tari_complete
PANEL <- panel

cat("\n========================================\n")
cat(" Pregatirea datelor a fost finalizata.\n")
cat("========================================\n")

# ============================================================================
# SECTIUNEA 2  -  ANALIZA UNIVARIATA INTERACTIVA
# ============================================================================

cat("\n========================================\n")
cat(" PARTEA 2: ANALIZA UNIVARIATA (INTERACTIVA)\n")
cat("========================================\n")

# ----------------------------------------------------------------------------
# Functia principala interactiva
# ----------------------------------------------------------------------------
analiza_univariata_interactiva <- function(cod_tara, panel_data) {

  cat("\n\n==========================================================\n")
  cat(" UNIVARIATA INTERACTIVA: ", cod_tara, "\n", sep = "")
  cat("==========================================================\n")

  dir_t <- file.path(DIR_UNIVARIATA, cod_tara)
  if (!dir.exists(dir_t)) dir.create(dir_t, recursive = TRUE)

  date_tara <- panel_data %>%
    dplyr::filter(geo == cod_tara) %>%
    dplyr::arrange(data)

  if (nrow(date_tara) < MIN_OBS) {
    cat("Tara are sub", MIN_OBS, "observatii. Sare.\n")
    return(NULL)
  }

  # Pasul 1: Constructie ts
  serie <- construieste_ts(date_tara$elec_gwh, min(date_tara$data))

  # ------------------------------------------------------------------------
  # Pasul 2: Analiza exploratorie (3 grafice cu ghiduri)
  # ------------------------------------------------------------------------

  # Graficul 1: serie temporala
  tryCatch({
    p_serie <- autoplot(serie) +
      ggtitle(paste("Consum electricitate", cod_tara, "(GWh)")) +
      xlab("An") + ylab("GWh") + theme_minimal()
    afiseaza_si_salveaza_ggplot(p_serie,
      file.path(dir_t, "01_serie_temporala.png"))
    ghid_si_pauza(c(
      ">>> Graficul prezinta consumul lunar de electricitate. Ce sa observi:",
      "    - Tendinta generala (creste, scade, sau stabila?)",
      "    - Forta sezonalitatii (cat de mari sunt oscillatiile anuale?)",
      "    - Anomalii vizibile (COVID 2020, criza energetica 2022)"
    ))
  }, error = function(e) cat("Eroare grafic serie:", conditionMessage(e), "\n"))

  # Graficul 2: profil sezonier
  tryCatch({
    p_seas <- ggseasonplot(serie, year.labels = FALSE) +
      ggtitle(paste("Profil sezonier", cod_tara)) +
      xlab("Luna") + ylab("GWh") + theme_minimal()
    afiseaza_si_salveaza_ggplot(p_seas,
      file.path(dir_t, "02_sezonier.png"))
    ghid_si_pauza(c(
      ">>> Profil sezonier (cate o linie pe an)."
    ))
  }, error = function(e) cat("Eroare grafic sezonier:", conditionMessage(e), "\n"))

  # Graficul 3: STL
  tryCatch({
    descomp <- stl(serie, s.window = "periodic")
    afiseaza_si_salveaza_base(
      quote(plot(descomp, main = paste("Decompozitie STL", cod_tara))),
      file.path(dir_t, "03_stl.png"))
    ghid_si_pauza(c(
      ">>> Decompozitie STL (panouri: date, sezonier, trend, reziduu)."
    ))
  }, error = function(e) cat("Eroare STL:", conditionMessage(e), "\n"))

  # ------------------------------------------------------------------------
  # Pasul 3: Box-Cox INTERACTIV
  # ------------------------------------------------------------------------
  cat("\n--- Box-Cox ---\n")
  lambda <- tryCatch(BoxCox.lambda(serie), error = function(e) 1)
  serie_bc <- BoxCox(serie, lambda)
  cat("Lambda optim calculat:", round(lambda, 4), "\n")

  tryCatch({
    df_bc <- data.frame(
      timp    = as.numeric(rep(time(serie), 2)),
      valoare = c(as.numeric(serie), as.numeric(serie_bc)),
      tip     = factor(rep(c("1. Original", "2. Transformat Box-Cox"),
                           each = length(serie)),
                       levels = c("1. Original", "2. Transformat Box-Cox"))
    )
    g_bc <- ggplot(df_bc, aes(x = timp, y = valoare)) +
      geom_line(color = "steelblue") +
      facet_wrap(~ tip, scales = "free_y", ncol = 1) +
      ggtitle(paste0("Comparatie Box-Cox (lambda = ",
                     round(lambda, 3), ") - ", cod_tara)) +
      xlab("Timp") + ylab("Valoare") + theme_minimal()
    afiseaza_si_salveaza_ggplot(g_bc,
      file.path(dir_t, "00_boxcox_comparatie.png"))
    ghid_si_pauza(c(
      ">>> Comparatie Box-Cox (seria originala vs. transformata).",
      paste0("    - Lambda = ", round(lambda, 4),
             ". Daca este in intervalul 0.7 .. 1.3, poti raspunde n.")
    ))
  }, error = function(e) cat("Eroare grafic Box-Cox:", conditionMessage(e), "\n"))

  aplica_boxcox <- raspuns_da(citeste_input(
    "Aplicam transformarea Box-Cox? (y/n): "))
  serie_lucru   <- if (aplica_boxcox) serie_bc else serie
  lambda_arima  <- if (aplica_boxcox) lambda else NULL
  cat("Decizie:", if (aplica_boxcox) "APLICATA" else "NU se aplica", "\n")

  # ------------------------------------------------------------------------
  # Pasul 4: Diferentiere INTERACTIVA (D, apoi d) cu baterie extinsa de teste
  # ------------------------------------------------------------------------

  # ====== A. SERIA ORIGINALA: baterie extinsa de teste ======
  teste_orig <- teste_stationaritate_extinse(serie_lucru, "Seria originala")
  adf_orig  <- teste_orig$adf_simple_p
  kpss_orig <- teste_orig$kpss_stat

  acf_orig  <- Acf(serie_lucru,  lag.max = 36, plot = FALSE)
  pacf_orig <- Pacf(serie_lucru, lag.max = 36, plot = FALSE)

  afiseaza_si_salveaza_base(
    quote(plot(acf_orig, main = paste("ACF serie originala", cod_tara))),
    file.path(dir_t, "04a_acf_original.png"))
  ghid_si_pauza(c(
    ">>> ACF seria originala.",
    "    Pentru date lunare cu sezonalitate vizibila, se recomanda D=1."
  ))

  afiseaza_si_salveaza_base(
    quote(plot(pacf_orig, main = paste("PACF serie originala", cod_tara))),
    file.path(dir_t, "04b_pacf_original.png"))
  ghid_si_pauza(c(
    ">>> PACF seria originala."
  ))

  # ====== B. DIFERENTIERE SEZONIERA D ======
  aplica_D <- raspuns_da(citeste_input(
    "Aplicam diferentiere sezoniera (D=1)? (y/n): "))

  ch_test_stat <- NA
  ch_test_p    <- NA
  ch_test_text <- "(nu s-a aplicat)"

  if (aplica_D) {
    D <- 1
    serie_d12 <- diff(serie_lucru, lag = 12)
    cat("\n--- Aplicat D=1 ---\n")

    # B.1 Grafic seria diferentiata
    tryCatch({
      g_d12 <- autoplot(serie_d12) +
        ggtitle(paste("Seria dupa diferentiere sezoniera (D=1) -",
                      cod_tara)) +
        xlab("An") + ylab("Diferenta sezoniera (GWh)") +
        theme_minimal()
      afiseaza_si_salveaza_ggplot(g_d12,
        file.path(dir_t, "06_serie_dupa_D1.png"))
      ghid_si_pauza(c(
        ">>> Seria dupa D=1.",
        "    - Daca exista inca trend sau drift, poate fi nevoie si de d=1.",
        "    - Variantele extreme la 2020 (COVID) sau 2022 (criza energie)",
        "      apar clar in seria diferentiata."
      ))
    }, error = function(e)
      cat("Eroare grafic seria D=1:", conditionMessage(e), "\n"))

    # B.2 Baterie extinsa de teste pe seria dupa D=1
    teste_d12 <- teste_stationaritate_extinse(serie_d12, "Dupa D=1")
    adf_d12  <- teste_d12$adf_simple_p
    kpss_d12 <- teste_d12$kpss_stat

    # B.3 ACF + PACF combinate intr-o singura figura
    acf_d12  <- Acf(serie_d12,  lag.max = 36, plot = FALSE)
    pacf_d12 <- Pacf(serie_d12, lag.max = 36, plot = FALSE)
    afiseaza_si_salveaza_acf_pacf_combo(
      acf_d12, pacf_d12,
      file.path(dir_t, "06_acf_pacf_dupa_D1.png"),
      titlu_acf  = paste("ACF dupa D=1 -",  cod_tara),
      titlu_pacf = paste("PACF dupa D=1 -", cod_tara))
    ghid_si_pauza(c(
      ">>> ACF si PACF dupa D=1.",
      "    - Daca decrestere generala inca este lenta -> considera si d=1.",
      "    - Verifica lagurile 12, 24, 36 pentru a citi P si Q sezoniere."
    ))

    # B.4 Test Canova-Hansen pentru stationaritate sezoniera
    cat("\n--- Test Canova-Hansen pentru stationaritate sezoniera ---\n")
    ch_res <- tryCatch(
      uroot::ch.test(serie_d12, type = "dummy", lag1 = TRUE),
      error = function(e) {
        cat("Canova-Hansen esuat:", conditionMessage(e), "\n"); NULL
      })
    if (!is.null(ch_res)) {
      print(ch_res)
      # ch.test returneaza statistic per luna + un "joint" (ultima valoare).
      # Pentru concluzia globala folosim valoarea joint.
      n_stat <- length(ch_res$statistic)
      ch_test_stat <- tryCatch(as.numeric(ch_res$statistic[n_stat]),
                               error = function(e) NA)
      ch_test_p    <- tryCatch(as.numeric(ch_res$pvalues[n_stat]),
                               error = function(e) NA)
      decizie_ch <- if (!is.na(ch_test_p) && ch_test_p < 0.05)
        "RESPINGEM H0 -> sezonier nestationar, ar putea fi nevoie de D=2" else
        "NU respingem H0 -> sezonier stationar, D=1 e suficient"
      cat(sprintf("\nCanova-Hansen joint: stat = %s, p = %s\n",
                  fmt(ch_test_stat), fmt(ch_test_p)))
      cat(">>>", decizie_ch, "\n")
      ch_test_text <- sprintf("joint stat = %s, p = %s",
                              fmt(ch_test_stat), fmt(ch_test_p))
    }
    ghid_si_pauza(c(
      ">>> Canova-Hansen: daca p < 0.05, ar putea fi nevoie de D=2",
      "    (rar pentru date de consum electric)."
    ))

    # B.5 Decizie D=2 (foarte rara)
    aplica_D2 <- raspuns_da(citeste_input(
      "Aplicam diferentiere sezoniera suplimentara (D=2)? (y/n): "))
    if (aplica_D2) {
      D <- 2
      serie_d12 <- diff(serie_d12, lag = 12)
      cat("\n--- Aplicat D=2. Reteste pe seria dubla diferentiata ---\n")
      teste_d12 <- teste_stationaritate_extinse(serie_d12, "Dupa D=2")
      adf_d12   <- teste_d12$adf_simple_p
      kpss_d12  <- teste_d12$kpss_stat
    }
  } else {
    D <- 0
    serie_d12 <- serie_lucru
    adf_d12   <- adf_orig
    kpss_d12  <- kpss_orig
    teste_d12 <- teste_orig
  }

  # ====== C. DIFERENTIERE REGULATA d ======
  aplica_d <- raspuns_da(citeste_input(
    "\nAplicam si diferentiere regulata (d=1)? (y/n): "))
  if (aplica_d) {
    d <- 1
    serie_stationara <- diff(serie_d12)
    cat(sprintf("\n--- Aplicat d=1 (peste D=%d) ---\n", D))

    # C.1 Grafic seria dupa d=1
    tryCatch({
      g_dd <- autoplot(serie_stationara) +
        ggtitle(paste("Seria dupa D=", D, " si d=1 -", cod_tara, sep="")) +
        xlab("An") + ylab("Diferenta dubla (GWh)") +
        theme_minimal()
      afiseaza_si_salveaza_ggplot(g_dd,
        file.path(dir_t, "07_serie_dupa_D1_d1.png"))
      ghid_si_pauza(c(
        ">>> Seria dupa diferentierea regulata d=1 peste D."
      ))
    }, error = function(e)
      cat("Eroare grafic seria d=1:", conditionMessage(e), "\n"))

    # C.2 Baterie extinsa de teste pe seria finala
    teste_final_obj <- teste_stationaritate_extinse(serie_stationara,
      sprintf("Dupa D=%d si d=1 (seria stationara finala)", D))
    adf_final  <- teste_final_obj$adf_simple_p
    kpss_final <- teste_final_obj$kpss_stat

    # C.3 ACF + PACF combinate dupa D si d
    acf_dd  <- Acf(serie_stationara,  lag.max = 36, plot = FALSE)
    pacf_dd <- Pacf(serie_stationara, lag.max = 36, plot = FALSE)
    afiseaza_si_salveaza_acf_pacf_combo(
      acf_dd, pacf_dd,
      file.path(dir_t, "07_acf_pacf_dupa_D1_d1.png"),
      titlu_acf  = sprintf("ACF dupa D=%d si d=1 - %s",  D, cod_tara),
      titlu_pacf = sprintf("PACF dupa D=%d si d=1 - %s", D, cod_tara))
    ghid_si_pauza(c(
      ">>> ACF si PACF dupa D si d.",
      "    - Aceasta este versiunea pe baza careia introduci p, q, P, Q",
      "      la pasul de identificare manuala SARIMA."
    ))
  } else {
    d <- 0
    serie_stationara <- serie_d12
    teste_final_obj <- teste_d12
    adf_final  <- adf_d12
    kpss_final <- kpss_d12
  }

  cat(sprintf("\n--- Seria stationara finala (d=%d, D=%d) ---\n", d, D))
  cat("ADF simplu p :", fmt(adf_final),  "\n")
  cat("KPSS stat    :", fmt(kpss_final), "\n")

  # ------------------------------------------------------------------------
  # Pasul 5: ACF/PACF pe seria stationara + INPUT MANUAL ordine
  # ------------------------------------------------------------------------
  acf_obj   <- Acf(serie_stationara,  lag.max = 36, plot = FALSE)
  pacf_obj  <- Pacf(serie_stationara, lag.max = 36, plot = FALSE)
  acf_vals  <- as.numeric(acf_obj$acf)
  pacf_vals <- as.numeric(pacf_obj$acf)

  afiseaza_si_salveaza_base(
    quote(plot(acf_obj,
               main = paste("ACF serie stationara FINALA", cod_tara))),
    file.path(dir_t, "04_acf.png"))
  ghid_si_pauza(c(
    ">>> ACF seria stationara finala. Pe baza acestui grafic vei alege q si Q.",
    "    - Numara lagurile consecutive de la 1 in sus care depasesc pragul.",
    "    - Verifica si lagurile sezoniere 12, 24 (si eventual 36)."
  ))
  afiseaza_si_salveaza_base(
    quote(plot(pacf_obj,
               main = paste("PACF serie stationara FINALA", cod_tara))),
    file.path(dir_t, "05_pacf.png"))
  ghid_si_pauza(c(
    ">>> PACF seria stationara finala. Pe baza ei vei alege p si P.",
    "    - Numara lagurile consecutive de la 1 in sus care depasesc pragul.",
    "    - Verifica si lagurile sezoniere 12, 24."
  ))

  prag <- afiseaza_tabel_acf_pacf(acf_vals, pacf_vals,
                                  length(serie_stationara))

  ord <- input_ordine_sarima(d, D)
  ord$d <- d
  ord$D <- D
  ord$prag       <- prag
  ord$acf_lag1   <- acf_vals[2]
  ord$acf_lag2   <- if (length(acf_vals)  >= 3)  acf_vals[3]  else NA
  ord$acf_lag3   <- if (length(acf_vals)  >= 4)  acf_vals[4]  else NA
  ord$acf_lag12  <- if (length(acf_vals)  >= 13) acf_vals[13] else NA
  ord$acf_lag24  <- if (length(acf_vals)  >= 25) acf_vals[25] else NA
  ord$pacf_lag1  <- pacf_vals[1]
  ord$pacf_lag2  <- if (length(pacf_vals) >= 2)  pacf_vals[2] else NA
  ord$pacf_lag3  <- if (length(pacf_vals) >= 3)  pacf_vals[3] else NA
  ord$pacf_lag12 <- if (length(pacf_vals) >= 12) pacf_vals[12] else NA
  ord$pacf_lag24 <- if (length(pacf_vals) >= 24) pacf_vals[24] else NA

  cat(sprintf("\nSpecificatie SARIMA aleasa: SARIMA(%d,%d,%d)(%d,%d,%d)[12]\n",
              ord$p, ord$d, ord$q, ord$P, ord$D, ord$Q))

  # ------------------------------------------------------------------------
  # Pasul 6: Train / test (AUTOMAT)
  # ------------------------------------------------------------------------
  h_test  <- 24
  n_total <- length(serie)
  if (n_total <= h_test + 24) {
    cat("Serie prea scurta pentru test 24 luni. Sare tara.\n")
    return(NULL)
  }
  train <- window(serie, end = time(serie)[n_total - h_test])
  test  <- window(serie, start = time(serie)[n_total - h_test + 1])

  # ------------------------------------------------------------------------
  # Pasul 7: Estimare modele (AUTOMAT)
  # ------------------------------------------------------------------------
  cat("\n--- Estimare modele (poate dura cateva secunde) ---\n")

  model_manual <- tryCatch(
    Arima(train,
          order    = c(ord$p, ord$d, ord$q),
          seasonal = c(ord$P, ord$D, ord$Q),
          lambda   = lambda_arima,
          method   = "CSS-ML"),
    error = function(e) {
      cat("Eroare SARIMA manual:", conditionMessage(e), "\n"); NULL
    })

  cat("Estimam auto.arima (cautare completa, poate dura)...\n")
  model_auto <- tryCatch(
    auto.arima(train, seasonal = TRUE,
               stepwise = FALSE, approximation = FALSE,
               lambda = lambda_arima),
    error = function(e) {
      cat("Eroare auto.arima:", conditionMessage(e), "\n"); NULL
    })

  cat("Estimam ETS...\n")
  model_ets <- tryCatch(ets(train),
                        error = function(e) {
                          cat("Eroare ETS:", conditionMessage(e), "\n"); NULL
                        })

  # ------------------------------------------------------------------------
  # Pasul 8: Diagnostice reziduuri (afisate cu ghid)
  # ------------------------------------------------------------------------
  diag_lb <- function(model, eticheta) {
    if (is.null(model)) return(list(p = NA, ok = NA))
    lb <- tryCatch(Box.test(residuals(model), lag = 24,
                            type = "Ljung-Box"),
                   error = function(e) NULL)
    if (is.null(lb)) return(list(p = NA, ok = NA))
    cat("\n--- Reziduuri", eticheta, "---\n")
    cat("Ljung-Box p =", round(lb$p.value, 4),
        "(p > 0.05 = reziduuri OK)\n")

    if (interactive()) {
      # Refoloseste fereastra externa daca exista deja
      dl_r <- dev.list()
      are_popup_r <- length(dl_r) > 0 &&
        any(tolower(names(dl_r)) %in% c("windows", "x11", "quartz"))

      if (are_popup_r && FALLBACK_FEREASTRA) {
        deschide_fereastra_separata()
        rezultat_chk <- tryCatch({ checkresiduals(model); "ok" },
                                 error = function(e) conditionMessage(e))
      } else {
        reseteaza_grafic()
        rezultat_chk <- tryCatch({ checkresiduals(model); "ok" },
                                 error = function(e) conditionMessage(e))
        if (!identical(rezultat_chk, "ok") && FALLBACK_FEREASTRA) {
          cat("(checkresiduals in pane a esuat;",
              "deschid fereastra separata)\n")
          if (deschide_fereastra_separata()) {
            rezultat_chk <- tryCatch({ checkresiduals(model); "ok" },
                                     error = function(e) conditionMessage(e))
          }
        }
      }
      if (!identical(rezultat_chk, "ok"))
        cat("(Nu pot afisa diagnosticul:", rezultat_chk, ")\n")
      forteaza_render()
      # checkresiduals lasa par(mfrow=c(2,2)); reset doar mfrow ca sa nu
      # afecteze urmatorul plot, fara a schimba marginile pe popup
      try(par(mfrow = c(1, 1)), silent = TRUE)
      inchide_devices_fisier()
    }
    tryCatch({
      png(filename = file.path(dir_t,
                               paste0("06_reziduuri_", eticheta, ".png")),
          width = 10, height = 6, units = "in", res = 150, bg = "white")
      checkresiduals(model)
      dev.off()
    }, error = function(e) cat("Eroare grafic reziduuri", eticheta, "\n"))

    ghid_si_pauza(c(
      paste0(">>> Diagnostic reziduuri ", eticheta, "."),
      paste0("    - Ljung-Box p = ", round(lb$p.value, 4),
             if (lb$p.value > 0.05) " (OK)" else " (PROBLEMATIC)")
    ))
    list(p = lb$p.value, ok = lb$p.value > 0.05)
  }

  lb_manual <- diag_lb(model_manual, "manual")
  lb_auto   <- diag_lb(model_auto,   "auto")
  lb_ets    <- diag_lb(model_ets,    "ets")

  # ------------------------------------------------------------------------
  # Pasul 9: Prognoze (afisate cu ghid)
  # ------------------------------------------------------------------------
  prognoza_safe <- function(model) {
    if (is.null(model)) return(NULL)
    tryCatch(forecast(model, h = h_test),
             error = function(e) {
               cat("Eroare prognoza:", conditionMessage(e), "\n"); NULL
             })
  }

  fc_manual <- prognoza_safe(model_manual)
  fc_auto   <- prognoza_safe(model_auto)
  fc_ets    <- prognoza_safe(model_ets)

  arata_prognoza <- function(fc, eticheta) {
    if (is.null(fc)) return(invisible(NULL))
    tryCatch({
      g <- autoplot(fc) +
        autolayer(test, series = "Real") +
        ggtitle(paste("Prognoza", eticheta, cod_tara)) +
        xlab("An") + ylab("GWh") + theme_minimal()
      afiseaza_si_salveaza_ggplot(g, file.path(dir_t,
        paste0("07_prognoza_", eticheta, ".png")))
      ghid_si_pauza(c(
        paste0(">>> Prognoza ", eticheta, " vs. valori reale."),
        "    - Linie albastra = prognoza punctuala; negru = valori reale."
      ))
    }, error = function(e) cat("Eroare grafic prognoza", eticheta, "\n"))
  }
  arata_prognoza(fc_manual, "manual")
  arata_prognoza(fc_auto,   "auto")
  arata_prognoza(fc_ets,    "ets")

  # ------------------------------------------------------------------------
  # Pasul 10: Acuratete (AUTOMAT - calcul) + extrage si MASE in-sample
  # ------------------------------------------------------------------------
  extrage_acc <- function(fc, model) {
    rez <- list(rmse_in = NA, mae_in = NA, mape_in = NA, mase_in = NA,
                aicc = NA,
                rmse_out = NA, mae_out = NA, mape_out = NA, mase_out = NA)
    if (is.null(fc)) return(rez)
    a <- tryCatch(accuracy(fc, test), error = function(e) NULL)
    if (is.null(a)) return(rez)
    if ("Training set" %in% rownames(a)) {
      rez$rmse_in <- a["Training set", "RMSE"]
      rez$mae_in  <- a["Training set", "MAE"]
      rez$mape_in <- a["Training set", "MAPE"]
      if ("MASE" %in% colnames(a)) rez$mase_in <- a["Training set", "MASE"]
    }
    if ("Test set" %in% rownames(a)) {
      rez$rmse_out <- a["Test set", "RMSE"]
      rez$mae_out  <- a["Test set", "MAE"]
      rez$mape_out <- a["Test set", "MAPE"]
      if ("MASE" %in% colnames(a)) rez$mase_out <- a["Test set", "MASE"]
    }
    if (!is.null(model) && !is.null(model$aicc)) rez$aicc <- model$aicc
    rez
  }

  acc_manual <- extrage_acc(fc_manual, model_manual)
  acc_auto   <- extrage_acc(fc_auto,   model_auto)
  acc_ets    <- extrage_acc(fc_ets,    model_ets)

  arma_auto <- if (!is.null(model_auto))
    sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[%d]",
            model_auto$arma[1], model_auto$arma[6], model_auto$arma[2],
            model_auto$arma[3], model_auto$arma[7], model_auto$arma[4],
            model_auto$arma[5]) else NA
  tip_ets <- if (!is.null(model_ets)) model_ets$method else NA

  spec_manual <- sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[12]",
                         ord$p, ord$d, ord$q, ord$P, ord$D, ord$Q)

  # ------------------------------------------------------------------------
  # Pasul 11: Doua tabele comparative AFISATE (out-of-sample si in-sample)
  # ------------------------------------------------------------------------
  cat("\n\n========== ACURATETE OUT-OF-SAMPLE (24 luni test) - ",
      cod_tara, " ==========\n", sep = "")
  cat(sprintf("%-7s %-10s %-10s %-10s %-10s %-10s\n",
              "Model", "RMSE", "MAE", "MAPE", "MASE", "LB_p"))
  cat(strrep("-", 65), "\n", sep = "")
  linie_out <- function(eticheta, acc, lb_p) {
    cat(sprintf("%-7s %-10s %-10s %-10s %-10s %-10s\n",
                eticheta,
                fmt(acc$rmse_out, 2),
                fmt(acc$mae_out,  2),
                fmt(acc$mape_out, 2),
                fmt(acc$mase_out, 3),
                fmt(lb_p, 4)))
  }
  linie_out("1 Man",  acc_manual, lb_manual$p)
  linie_out("2 Auto", acc_auto,   lb_auto$p)
  linie_out("3 ETS",  acc_ets,    lb_ets$p)
  cat(strrep("-", 65), "\n", sep = "")
  cat("Specificatii: 1 Manual =", spec_manual, "\n")
  cat("              2 Auto   =", arma_auto, "\n")
  cat("              3 ETS    =", tip_ets, "\n")

  cat("\n========== ACURATETE IN-SAMPLE (set antrenament) - ",
      cod_tara, " ==========\n", sep = "")
  cat(sprintf("%-7s %-10s %-10s %-10s %-10s %-10s %-10s\n",
              "Model", "RMSE", "MAE", "MAPE", "MASE", "AICc", "LB_p"))
  cat(strrep("-", 75), "\n", sep = "")
  linie_in <- function(eticheta, acc, lb_p) {
    cat(sprintf("%-7s %-10s %-10s %-10s %-10s %-10s %-10s\n",
                eticheta,
                fmt(acc$rmse_in, 2),
                fmt(acc$mae_in,  2),
                fmt(acc$mape_in, 2),
                fmt(acc$mase_in, 3),
                fmt(acc$aicc,    2),
                fmt(lb_p, 4)))
  }
  linie_in("1 Man",  acc_manual, lb_manual$p)
  linie_in("2 Auto", acc_auto,   lb_auto$p)
  linie_in("3 ETS",  acc_ets,    lb_ets$p)
  cat(strrep("-", 75), "\n\n", sep = "")

  # ------------------------------------------------------------------------
  # Pasul 12: ALEGEREA INTERACTIVA a modelului castigator
  # ------------------------------------------------------------------------
  optiuni_disponibile <- character()
  if (!is.null(model_manual)) optiuni_disponibile <- c(optiuni_disponibile, "1")
  if (!is.null(model_auto))   optiuni_disponibile <- c(optiuni_disponibile, "2")
  if (!is.null(model_ets))    optiuni_disponibile <- c(optiuni_disponibile, "3")

  cat(">>>Care dintre aceste modele este optim?\n")
  cat("    1 - SARIMA manual\n")
  cat("    2 - auto.arima\n")
  cat("    3 - ETS\n")
  if (length(optiuni_disponibile) < 3) {
    cat("    (Disponibile in aceasta tara: ",
        paste(optiuni_disponibile, collapse = ", "), ")\n", sep = "")
  }

  while (TRUE) {
    alegere_user <- trimws(citeste_input("Alegerea ta [1/2/3]: "))
    if (alegere_user %in% optiuni_disponibile) break
    cat("Raspuns invalid sau model indisponibil. Optiuni valide: ",
        paste(optiuni_disponibile, collapse = "/"), "\n", sep = "")
  }

  idx_castigator <- as.integer(alegere_user)
  castigator      <- c("manual", "auto", "ets")[idx_castigator]
  castigator_eticheta <- c("SARIMA manual", "auto.arima", "ETS")[idx_castigator]
  spec_castigator <- c(spec_manual, arma_auto, tip_ets)[idx_castigator]
  model_castigator <- list(model_manual, model_auto, model_ets)[[idx_castigator]]
  mape_castigator  <- c(acc_manual$mape_out, acc_auto$mape_out,
                        acc_ets$mape_out)[idx_castigator]

  cat(sprintf("\n>>> Castigator ales: %s (%s) | MAPE OOS = %s%%\n",
              castigator_eticheta, spec_castigator,
              fmt(mape_castigator, 2)))

  # ------------------------------------------------------------------------
  # Pasul 13: REFIT pe seria COMPLETA si prognoza 24 luni cu IC 80% si 95%
  # ------------------------------------------------------------------------
  cat("\n--- Refit pe seria completa si prognoza 24 luni ---\n")
  serie_completa <- serie

  model_final <- tryCatch({
    if (castigator == "manual") {
      Arima(serie_completa,
            order    = c(ord$p, ord$d, ord$q),
            seasonal = c(ord$P, ord$D, ord$Q),
            lambda   = lambda_arima,
            method   = "CSS-ML")
    } else if (castigator == "auto") {
      ord_a <- forecast::arimaorder(model_auto)
      Arima(serie_completa,
            order    = c(ord_a[["p"]], ord_a[["d"]], ord_a[["q"]]),
            seasonal = list(order  = c(ord_a[["P"]], ord_a[["D"]], ord_a[["Q"]]),
                            period = ord_a[["Frequency"]]),
            lambda   = lambda_arima)
    } else {
      ets(serie_completa, model = model_ets)
    }
  }, error = function(e) {
    cat("Refit pe seria completa esuat:", conditionMessage(e),
        "; folosim modelul antrenat pe train.\n")
    model_castigator
  })

  fc_final <- tryCatch(
    forecast(model_final, h = 24, level = c(80, 95)),
    error = function(e) {
      cat("Eroare prognoza finala:", conditionMessage(e), "\n")
      NULL
    })

  # Construire tabel prognoza (date calendaristice + valori)
  tabel_prognoza <- NULL
  if (!is.null(fc_final)) {
    ultima_data <- max(date_tara$data)
    date_fc <- seq.Date(from = ultima_data, by = "1 month",
                        length.out = 25)[-1]
    tabel_prognoza <- data.frame(
      Luna      = format(date_fc, "%Y-%m-%d"),
      Prognoza  = round(as.numeric(fc_final$mean), 2),
      IC_80_inf = round(as.numeric(fc_final$lower[, 1]), 2),
      IC_80_sup = round(as.numeric(fc_final$upper[, 1]), 2),
      IC_95_inf = round(as.numeric(fc_final$lower[, 2]), 2),
      IC_95_sup = round(as.numeric(fc_final$upper[, 2]), 2),
      stringsAsFactors = FALSE
    )

    # Salvare CSV cu tabelul de prognoza
    write.csv(tabel_prognoza,
              file.path(dir_t, "prognoza_finala_2ani.csv"),
              row.names = FALSE)

    # Afisare tabel in consola
    cat("\n========== PROGNOZA FINALA 2026-2027 (24 luni) - ",
        cod_tara, " ==========\n", sep = "")
    cat(sprintf("%-12s %-10s %-11s %-11s %-11s %-11s\n",
                "Luna", "Prognoza", "IC_80_inf", "IC_80_sup",
                "IC_95_inf", "IC_95_sup"))
    cat(strrep("-", 75), "\n", sep = "")
    for (i in seq_len(nrow(tabel_prognoza))) {
      cat(sprintf("%-12s %-10s %-11s %-11s %-11s %-11s\n",
                  tabel_prognoza$Luna[i],
                  fmt(tabel_prognoza$Prognoza[i],  2),
                  fmt(tabel_prognoza$IC_80_inf[i], 2),
                  fmt(tabel_prognoza$IC_80_sup[i], 2),
                  fmt(tabel_prognoza$IC_95_inf[i], 2),
                  fmt(tabel_prognoza$IC_95_sup[i], 2)))
    }
    cat(strrep("-", 75), "\n", sep = "")

    # Grafic prognoza finala: ultimii 5 ani de istoric + 2 ani prognoza
    tryCatch({
      start_fc_time <- time(fc_final$mean)[1]
      g_final <- autoplot(fc_final, include = 60) +
        geom_vline(xintercept = start_fc_time - 1 / 24,
                   linetype = "dashed", color = "red", alpha = 0.7) +
        ggtitle(paste0("Prognoza finala 2 ani - ", cod_tara,
                       " (", castigator_eticheta, ": ",
                       spec_castigator, ")")) +
        xlab("An") + ylab("Consum (GWh)") +
        theme_minimal() +
        theme(legend.position = "bottom")
      afiseaza_si_salveaza_ggplot(g_final,
        file.path(dir_t, "14_prognoza_finala_2ani.png"),
        latime = 12, inaltime = 6)
      ghid_si_pauza(c(
        ">>> Prognoza finala pe 24 luni cu intervale 80% si 95%.",
        "    - Linia continua albastra = prognoza punctuala.",
        "    - Banda mai inchisa = IC 80%, banda mai deschisa = IC 95%.",
        "    - Linia verticala rosie = inceputul perioadei prognozate.",
        "    - Sunt afisati doar ultimii 5 ani de istoric pentru claritate."
      ))
    }, error = function(e)
      cat("Eroare grafic prognoza finala:", conditionMessage(e), "\n"))
  }

  # ------------------------------------------------------------------------
  # Pasul 14: Fisier rezumat
  # ------------------------------------------------------------------------
  cale_rez <- file.path(dir_t, paste0("rezumat_", cod_tara, ".txt"))
  con <- file(cale_rez, "w", encoding = "UTF-8")

  linii_prognoza <- if (!is.null(tabel_prognoza)) {
    c(
      "",
      "10. PROGNOZA FINALA 2026-2027",
      paste("    Model folosit:", castigator_eticheta, "-", spec_castigator),
      paste("    Refit pe seria completa (",
            nrow(date_tara), "luni de la",
            format(min(date_tara$data), "%Y-%m"), "pana la",
            format(max(date_tara$data), "%Y-%m"), ")"),
      "",
      sprintf("    %-12s %-10s %-11s %-11s %-11s %-11s",
              "Luna", "Prognoza", "IC_80_inf", "IC_80_sup",
              "IC_95_inf", "IC_95_sup"),
      paste("   ", strrep("-", 72)),
      sapply(seq_len(nrow(tabel_prognoza)), function(i)
        sprintf("    %-12s %-10s %-11s %-11s %-11s %-11s",
                tabel_prognoza$Luna[i],
                fmt(tabel_prognoza$Prognoza[i],  2),
                fmt(tabel_prognoza$IC_80_inf[i], 2),
                fmt(tabel_prognoza$IC_80_sup[i], 2),
                fmt(tabel_prognoza$IC_95_inf[i], 2),
                fmt(tabel_prognoza$IC_95_sup[i], 2)))
    )
  } else {
    c("", "10. PROGNOZA FINALA: ESUATA (modelul nu a putut fi refit)")
  }

  writeLines(c(
    paste("Rezumat analiza univariata pentru tara:", cod_tara),
    paste(rep("=", 60), collapse = ""),
    "",
    "1. TRANSFORMARE BOX-COX",
    paste("   Lambda optim:", fmt(lambda)),
    paste("   Decizie utilizator:", aplica_boxcox),
    "",
    "2. STATIONARITATE SI DIFERENTIERE",
    "   --- Seria originala ---",
    paste("   ADF simplu (tseries) p :", fmt(teste_orig$adf_simple_p)),
    paste("   Phillips-Perron stat   :", fmt(teste_orig$pp_stat),
          " p =", fmt(teste_orig$pp_p)),
    paste("   KPSS stat              :", fmt(teste_orig$kpss_stat),
          " (crit. 5% = 0.463)"),
    paste("   ADF ur.df type=none  tau =",
          fmt(if (!is.null(teste_orig$adf_none))  teste_orig$adf_none$tau  else NA),
          " (cv5 =",
          fmt(if (!is.null(teste_orig$adf_none))  teste_orig$adf_none$cv5  else NA), ")"),
    paste("   ADF ur.df type=drift tau =",
          fmt(if (!is.null(teste_orig$adf_drift)) teste_orig$adf_drift$tau else NA),
          " (cv5 =",
          fmt(if (!is.null(teste_orig$adf_drift)) teste_orig$adf_drift$cv5 else NA), ")"),
    paste("   ADF ur.df type=trend tau =",
          fmt(if (!is.null(teste_orig$adf_trend)) teste_orig$adf_trend$tau else NA),
          " (cv5 =",
          fmt(if (!is.null(teste_orig$adf_trend)) teste_orig$adf_trend$cv5 else NA), ")"),
    "",
    paste("   --- Dupa diferentiere sezoniera (D =", D, ") ---"),
    paste("   ADF simplu p :", fmt(teste_d12$adf_simple_p)),
    paste("   Phillips-Perron stat:", fmt(teste_d12$pp_stat),
          " p =", fmt(teste_d12$pp_p)),
    paste("   KPSS stat    :", fmt(teste_d12$kpss_stat)),
    paste("   Canova-Hansen:", ch_test_text),
    "",
    paste("   Decizie d (regular):", d),
    paste("   --- Seria stationara finala (d=", d, ", D=", D, ") ---", sep=""),
    paste("   ADF simplu p :", fmt(adf_final)),
    paste("   KPSS stat    :", fmt(kpss_final)),
    paste("   PP p         :",
          fmt(if (!is.null(teste_final_obj$pp_p)) teste_final_obj$pp_p else NA)),
    "",
    "3. VALORI ACF / PACF LA LAGURI CHEIE",
    paste("   ACF lag 1 :", fmt(ord$acf_lag1)),
    paste("   ACF lag 2 :", fmt(ord$acf_lag2)),
    paste("   ACF lag 3 :", fmt(ord$acf_lag3)),
    paste("   ACF lag 12:", fmt(ord$acf_lag12)),
    paste("   ACF lag 24:", fmt(ord$acf_lag24)),
    paste("   PACF lag 1 :", fmt(ord$pacf_lag1)),
    paste("   PACF lag 2 :", fmt(ord$pacf_lag2)),
    paste("   PACF lag 3 :", fmt(ord$pacf_lag3)),
    paste("   PACF lag 12:", fmt(ord$pacf_lag12)),
    paste("   PACF lag 24:", fmt(ord$pacf_lag24)),
    paste("   Pragul de semnificatie 95%:", fmt(ord$prag)),
    "",
    "4. ORDINE INTRODUSE MANUAL",
    paste("  ", spec_manual),
    "",
    "5. SPECIFICATII MODELE ESTIMATE",
    paste("   Model auto.arima:", arma_auto),
    paste("   Model ETS (tip):", tip_ets),
    "",
    "6. LJUNG-BOX (lag 24, p > 0.05 = reziduuri OK)",
    paste("   Manual:", fmt(lb_manual$p), " adecvat:", lb_manual$ok),
    paste("   Auto  :", fmt(lb_auto$p),   " adecvat:", lb_auto$ok),
    paste("   ETS   :", fmt(lb_ets$p),    " adecvat:", lb_ets$ok),
    "",
    "7. ACURATETE OUT-OF-SAMPLE (ultimele 24 luni)",
    "   Model    RMSE       MAE        MAPE       MASE       LB_p",
    sprintf("   Manual   %-10s %-10s %-10s %-10s %-10s",
            fmt(acc_manual$rmse_out, 2), fmt(acc_manual$mae_out, 2),
            fmt(acc_manual$mape_out, 2), fmt(acc_manual$mase_out, 3),
            fmt(lb_manual$p, 4)),
    sprintf("   Auto     %-10s %-10s %-10s %-10s %-10s",
            fmt(acc_auto$rmse_out, 2), fmt(acc_auto$mae_out, 2),
            fmt(acc_auto$mape_out, 2), fmt(acc_auto$mase_out, 3),
            fmt(lb_auto$p, 4)),
    sprintf("   ETS      %-10s %-10s %-10s %-10s %-10s",
            fmt(acc_ets$rmse_out, 2), fmt(acc_ets$mae_out, 2),
            fmt(acc_ets$mape_out, 2), fmt(acc_ets$mase_out, 3),
            fmt(lb_ets$p, 4)),
    "",
    "8. ACURATETE IN-SAMPLE (set antrenament)",
    "   Model    RMSE       MAE        MAPE       MASE       AICc       LB_p",
    sprintf("   Manual   %-10s %-10s %-10s %-10s %-10s %-10s",
            fmt(acc_manual$rmse_in, 2), fmt(acc_manual$mae_in, 2),
            fmt(acc_manual$mape_in, 2), fmt(acc_manual$mase_in, 3),
            fmt(acc_manual$aicc, 2),    fmt(lb_manual$p, 4)),
    sprintf("   Auto     %-10s %-10s %-10s %-10s %-10s %-10s",
            fmt(acc_auto$rmse_in, 2), fmt(acc_auto$mae_in, 2),
            fmt(acc_auto$mape_in, 2), fmt(acc_auto$mase_in, 3),
            fmt(acc_auto$aicc, 2),    fmt(lb_auto$p, 4)),
    sprintf("   ETS      %-10s %-10s %-10s %-10s %-10s %-10s",
            fmt(acc_ets$rmse_in, 2), fmt(acc_ets$mae_in, 2),
            fmt(acc_ets$mape_in, 2), fmt(acc_ets$mase_in, 3),
            fmt(acc_ets$aicc, 2),    fmt(lb_ets$p, 4)),
    "",
    paste("9. MODEL CASTIGATOR (ales de utilizator):",
          castigator_eticheta, "-", spec_castigator),
    paste("   MAPE out-of-sample al castigatorului:",
          fmt(mape_castigator, 2), "%"),
    linii_prognoza
  ), con)
  close(con)

  cat("=== TERMINAT:", cod_tara, "(rezumat + prognoza finala salvate) ===\n")

  list(tara = cod_tara,
       lambda = lambda, d = d, D = D,
       p = ord$p, q = ord$q, P = ord$P, Q = ord$Q,
       model_manual_spec = spec_manual,
       model_auto_spec   = arma_auto,
       model_ets_spec    = tip_ets,
       lb_manual = lb_manual$p,
       lb_auto   = lb_auto$p,
       lb_ets    = lb_ets$p,
       mape_manual = acc_manual$mape_out,
       mape_auto   = acc_auto$mape_out,
       mape_ets    = acc_ets$mape_out,
       castigator           = castigator,
       castigator_eticheta  = castigator_eticheta,
       spec_castigator      = spec_castigator,
       mape_castigator      = mape_castigator)
}

# ----------------------------------------------------------------------------
# RESUME: ce tari au fost deja procesate?
# ----------------------------------------------------------------------------
tari_existente <- list.dirs(DIR_UNIVARIATA, full.names = FALSE,
                            recursive = FALSE)
tari_existente <- tari_existente[
  tari_existente %in% TARI_ANALIZA &
    sapply(tari_existente, function(t)
      file.exists(file.path(DIR_UNIVARIATA, t,
                            paste0("rezumat_", t, ".txt"))))
]

tari_de_procesat <- TARI_ANALIZA
if (length(tari_existente) > 0) {
  cat("\n>>> RELUARE\n")
  cat("Tari deja procesate (au rezumat.txt):\n")
  cat("   ", paste(tari_existente, collapse = ", "), "\n")
  cat("Optiuni:\n")
  cat("  1 = Continua de unde am ramas (sare peste cele procesate) [default]\n")
  cat("  2 = Reia totul de la inceput (suprascrie)\n")
  cat("  3 = Reia de la o tara anume\n")
  alegere <- trimws(citeste_input("Alegere [1/2/3] (default 1): "))
  if (alegere == "" || alegere == "1") {
    tari_de_procesat <- setdiff(TARI_ANALIZA, tari_existente)
  } else if (alegere == "3") {
    tara_start <- trimws(toupper(citeste_input(
      "Cod tara de la care reluam (ex: RO): ")))
    idx <- which(TARI_ANALIZA == tara_start)
    if (length(idx) > 0) {
      tari_de_procesat <- TARI_ANALIZA[idx:length(TARI_ANALIZA)]
    } else {
      cat("Cod tara invalid. Continuam cu toate tarile.\n")
    }
  }
  # alegere == "2" pastreaza TARI_ANALIZA intact
}

cat("\nVom procesa", length(tari_de_procesat), "tari:\n")
cat("   ", paste(tari_de_procesat, collapse = ", "), "\n\n")

# ----------------------------------------------------------------------------
# Bucla interactiva peste tari
# ----------------------------------------------------------------------------
rezultate_univariata <- list()
erori_univariata     <- character()
oprire_ceruta        <- FALSE

if (length(tari_de_procesat) == 0) {
  cat("Nu mai sunt tari de procesat in univariata. Trec mai departe.\n")
} else {
  for (i in seq_along(tari_de_procesat)) {
    tara <- tari_de_procesat[i]
    rez <- tryCatch(
      analiza_univariata_interactiva(tara, PANEL),
      error = function(e) {
        msg <- paste0(tara, ": ", conditionMessage(e))
        cat("EROARE GLOBALA tara", tara, ":", conditionMessage(e), "\n")
        erori_univariata <<- c(erori_univariata, msg)
        NULL
      })
    if (!is.null(rez)) rezultate_univariata[[tara]] <- rez

    # Curatare device-uri intre tari: inchide fereastra externa daca exista
    # ca urmatoarea tara sa porneasca cu stare grafica curata.
    if (interactive()) {
      dl_clean <- dev.list()
      win_idx_clean <- which(tolower(names(dl_clean)) %in%
                               c("windows", "x11", "quartz"))
      for (idx in rev(win_idx_clean)) {
        try(dev.off(dl_clean[idx]), silent = TRUE)
      }
    }

    if (i < length(tari_de_procesat)) {
      raspuns <- citeste_input(paste0(
        "\nContinuam cu urmatoarea tara (", tari_de_procesat[i + 1],
        ")? (y/n): "))
      if (!raspuns_da(raspuns)) {
        cat("\nOprire ceruta. Progresul univariat este salvat.\n")
        oprire_ceruta <- TRUE
        break
      }
    }
  }
}

REZULTATE_UNIVARIATA <- rezultate_univariata
ERORI_UNIVARIATA     <- erori_univariata

cat("\n========================================\n")
cat(" Univariata: ", length(REZULTATE_UNIVARIATA),
    "tari procesate in aceasta sesiune,",
    length(ERORI_UNIVARIATA), "erori.\n")
cat("========================================\n")


# ============================================================================
# SECTIUNEA 3  -  ANALIZA MULTIVARIATA (VAR / VECM) - INTERACTIVA
#
# Fiecare tara trece prin 10 checkpoint-uri unde utilizatorul vede testele
# si poate confirma sau schimba decizia. ENTER pastreaza comportamentul
# automatic implicit (reproduce v1).
# ============================================================================

raspuns_multi <- citeste_input("\nRulam si analiza multivariata? (y/n): ")
ruleaza_multi <- raspuns_da(raspuns_multi)

REZULTATE_MULTIVARIATA <- list()
ERORI_MULTIVARIATA     <- character()

if (ruleaza_multi) {

  cat("\n========================================\n")
  cat(" PARTEA 3: ANALIZA MULTIVARIATA (INTERACTIVA)\n")
  cat("========================================\n")

  # --------------------------------------------------------------------------
  # Pachete suplimentare pentru ramura ARDL (caz mix I(0)/I(1)).
  # Acelasi mecanism conditional ca in SECTIUNEA 0: instalam doar daca lipsesc,
  # apoi incarcam. lmtest este deja incarcat in setup; il pastram in lista
  # pentru completitudine. ARDL/dynlm/strucchange/sandwich se incarca aici
  # (lazy) doar cand utilizatorul ruleaza analiza multivariata.
  # --------------------------------------------------------------------------
  pachete_ardl <- c("ARDL", "dynlm", "lmtest", "sandwich", "strucchange")
  for (p in pachete_ardl) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, dependencies = TRUE)
    }
  }
  invisible(lapply(pachete_ardl, library, character.only = TRUE))

  fixeaza_call_var <- function(model, p_value) {
    if (is.null(model)) return(model)
    if (!is.null(model$call) && "p" %in% names(model$call)) {
      model$call$p <- as.integer(p_value)
    }
    if (!is.null(model$call) && "type" %in% names(model$call)) {
      model$call$type <- as.character(model$call$type)
    }
    model
  }

  ordin_integrare <- function(serie) {
    serie <- na.omit(serie)
    if (length(serie) < 30) return(NA)
    adf_lvl  <- tryCatch(adf.test(serie)$p.value, error = function(e) NA)
    kpss_lvl <- tryCatch(
      summary(ur.kpss(serie, type = "mu"))@teststat, error = function(e) NA)
    i0 <- (!is.na(adf_lvl)  && adf_lvl  < 0.05) &&
          (!is.na(kpss_lvl) && kpss_lvl < 0.463)
    if (i0) return(0)
    diff1   <- diff(serie)
    adf_d1  <- tryCatch(adf.test(diff1)$p.value, error = function(e) NA)
    kpss_d1 <- tryCatch(
      summary(ur.kpss(diff1, type = "mu"))@teststat, error = function(e) NA)
    i1 <- (!is.na(adf_d1)  && adf_d1  < 0.05) &&
          (!is.na(kpss_d1) && kpss_d1 < 0.463)
    if (i1) return(1)
    return(1)
  }

  # Helper: raporteaza ADF + KPSS pe nivel si prima diferenta pentru o
  # singura serie. Returneaza o lista cu p-values.
  raport_stationaritate <- function(serie) {
    serie <- na.omit(serie)
    adf_lvl  <- tryCatch(adf.test(serie)$p.value, error = function(e) NA)
    kpss_lvl <- tryCatch(as.numeric(
      summary(ur.kpss(serie, type = "mu"))@teststat),
      error = function(e) NA)
    diff1    <- diff(serie)
    adf_d1   <- tryCatch(adf.test(diff1)$p.value, error = function(e) NA)
    kpss_d1  <- tryCatch(as.numeric(
      summary(ur.kpss(diff1, type = "mu"))@teststat),
      error = function(e) NA)
    list(adf_lvl = adf_lvl, kpss_lvl = kpss_lvl,
         adf_d1  = adf_d1,  kpss_d1  = kpss_d1)
  }

  granger_pairwise <- function(y, x, order_p) {
    tryCatch({
      test <- lmtest::grangertest(y ~ x, order = order_p)
      test$`Pr(>F)`[2]
    }, error = function(e) NA)
  }

  rang_cointegrare <- function(jo) {
    tryCatch({
      teststat <- jo@teststat
      cv5      <- jo@cval[, "5pct"]
      r <- 0
      for (i in seq_along(teststat)) {
        if (teststat[i] > cv5[i]) {
          r <- length(teststat) - i + 1
        } else break
      }
      min(r, length(teststat) - 1)
    }, error = function(e) NA)
  }

  # Helper interactiv: cere o clasificare I(0)/I(1)/other pentru o variabila
  cere_clasificare <- function(eticheta, default_int) {
    default_str <- if (is.na(default_int)) "I(1)" else
      if (default_int == 0) "I(0)" else "I(1)"
    prompt <- sprintf("   %s [I(0)/I(1)/other; default=%s]: ",
                      eticheta, default_str)
    raspuns <- tolower(trimws(citeste_input(prompt)))
    if (raspuns == "") raspuns <- tolower(default_str)
    if (raspuns %in% c("i(0)", "i0", "0", "io"))   return(0)
    if (raspuns %in% c("i(1)", "i1", "1"))         return(1)
    if (raspuns %in% c("other", "alta", "alt"))    return(NA)
    cat("   Raspuns neclar, folosim default", default_str, "\n")
    if (default_str == "I(0)") 0 else 1
  }

  # Estimeaza un model VAR/VECM pe baza tipului si lagului ales.
  # Returneaza lista cu (var_obj, tip_model, rang, vectori_beta, alpha_vec)
  # sau NULL daca esueaza.
  estimeaza_model <- function(Y_loc, tip, p_use, rang_use) {
    if (tip == "VECM") {
      jo <- tryCatch(
        ca.jo(Y_loc, type = "trace", ecdet = "const",
              K = max(2, p_use)),
        error = function(e) NULL)
      if (is.null(jo)) return(NULL)
      vecm_est <- tryCatch(cajorls(jo, r = rang_use),
                           error = function(e) NULL)
      vec_obj <- tryCatch(vec2var(jo, r = rang_use),
                          error = function(e) NULL)
      list(var_obj = vec_obj,
           tip_model = "VECM",
           rang = rang_use,
           vectori_beta = if (!is.null(vecm_est)) vecm_est$beta else NULL,
           alpha_vec    = if (!is.null(vecm_est)) coef(vecm_est$rlm) else NULL,
           johansen_obj = jo)
    } else if (tip == "VAR_DIFF") {
      m <- tryCatch(VAR(diff(Y_loc), p = p_use, type = "const"),
                    error = function(e) NULL)
      list(var_obj = fixeaza_call_var(m, p_use),
           tip_model = "VAR_DIFF", rang = 0,
           vectori_beta = NULL, alpha_vec = NULL, johansen_obj = NULL)
    } else {
      m <- tryCatch(VAR(Y_loc, p = p_use, type = "const"),
                    error = function(e) NULL)
      list(var_obj = fixeaza_call_var(m, p_use),
           tip_model = "VAR", rang = 0,
           vectori_beta = NULL, alpha_vec = NULL, johansen_obj = NULL)
    }
  }

  # Ruleaza si afiseaza diagnostice multiple. Returneaza p-values selectate.
  ruleaza_diagnostice <- function(var_obj, p_use, dir_t = NULL, cod_tara = "") {
    if (is.null(var_obj)) return(list(serial_p = NA, arch_p = NA, norm_p = NA))
    lag_pt <- c(max(p_use + 4, 12), max(p_use + 4, 16), max(p_use + 4, 20))
    lag_arch <- c(5, 12)

    cat("\n--- Diagnostice reziduuri ---\n")
    cat("Portmanteau (Ljung-Box multivariat); p > 0.05 = fara autocorelare\n")
    p_serial <- sapply(lag_pt, function(lg) {
      tryCatch(
        serial.test(var_obj, lags.pt = lg,
                    type = "PT.asymptotic")$serial$p.value,
        error = function(e) NA)
    })
    for (i in seq_along(lag_pt))
      cat(sprintf("  lag = %2d  -> p = %s\n", lag_pt[i], fmt(p_serial[i])))

    cat("\nARCH multivariat; p > 0.05 = fara heteroscedasticitate\n")
    p_arch <- sapply(lag_arch, function(lg) {
      tryCatch(arch.test(var_obj, lags.multi = lg)$arch.mul$p.value,
               error = function(e) NA)
    })
    for (i in seq_along(lag_arch))
      cat(sprintf("  lag = %2d  -> p = %s\n", lag_arch[i], fmt(p_arch[i])))

    cat("\nJarque-Bera multivariat; p > 0.05 = reziduuri normale\n")
    p_norm <- tryCatch(
      normality.test(var_obj)$jb.mul$JB$p.value,
      error = function(e) NA)
    cat("  p =", fmt(p_norm), "\n")

    # Grafice reziduuri per ecuatie (serie temporala + ACF)
    if (!is.null(dir_t)) {
      reziduuri <- tryCatch(residuals(var_obj), error = function(e) NULL)
      if (!is.null(reziduuri) && is.matrix(reziduuri)) {
        for (ec in colnames(reziduuri)) {
          tryCatch({
            r <- reziduuri[, ec]
            t_idx <- seq_along(r)
            df_r <- data.frame(t = t_idx, rez = r)
            g_r <- ggplot(df_r, aes(x = t, y = rez)) +
              geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
              geom_line(color = "steelblue") +
              ggtitle(paste("Reziduuri ecuatia", ec, "-", cod_tara)) +
              xlab("Observatie") + ylab("Reziduu") + theme_minimal()
            afiseaza_si_salveaza_ggplot(g_r,
              file.path(dir_t, paste0("04_reziduuri_serie_", ec, ".png")))
          }, error = function(e)
            cat("Eroare reziduuri", ec, ":", conditionMessage(e), "\n"))
          tryCatch({
            a_r <- Acf(reziduuri[, ec], lag.max = 24, plot = FALSE)
            afiseaza_si_salveaza_base(
              quote(plot(a_r, main = paste("ACF reziduuri", ec, "-", cod_tara))),
              file.path(dir_t, paste0("05_reziduuri_acf_", ec, ".png")))
          }, error = function(e)
            cat("Eroare ACF reziduuri", ec, ":", conditionMessage(e), "\n"))
        }
        ghid_si_pauza(c(
          ">>> Grafice reziduuri (serie + ACF) pentru fiecare ecuatie."
        ))
      }
    }

    list(serial_p = p_serial[2],   # raportam lag = max(p+4, 16) ca default
         arch_p   = p_arch[1],     # raportam lag = 5 ca default
         norm_p   = p_norm,
         all_serial = p_serial,
         all_arch   = p_arch,
         lag_pt = lag_pt, lag_arch = lag_arch)
  }

  # ==========================================================================
  # HELPERI PENTRU RAMURA ARDL (mix I(0)/I(1)) si pentru imbogatirea
  # rezultatelor VAR/VECM (PARTEA B.1).
  # ==========================================================================

  # Metrici OOS standard (folosit de OOS-ARDL; oglindeste calc_metrici din VAR).
  calc_metrici_oos <- function(actual, fc) {
    ok <- !is.na(actual) & !is.na(fc)
    if (!any(ok)) return(list(rmse = NA, mae = NA, mape = NA))
    a <- actual[ok]; f <- fc[ok]
    list(rmse = sqrt(mean((a - f)^2)),
         mae  = mean(abs(a - f)),
         mape = mean(abs((a - f) / a)) * 100)
  }

  # Verificare anti-I(2): blocheaza ARDL daca o serie e clasificata "other" (NA)
  # sau daca prima diferenta e inca nestationara (ADF p >= 0.05 si KPSS >= 0.463).
  # Foloseste rapoartele de la CHECKPOINT 2.
  verifica_anti_i2 <- function(ordine_int, rapoarte) {
    motive <- character()
    for (v in names(ordine_int)) {
      if (is.na(ordine_int[v])) {
        motive <- c(motive, sprintf(
          "%s clasificata 'other' (ordin de integrare necunoscut)", v))
        next
      }
      r <- rapoarte[[v]]
      adf_d1_ns  <- is.na(r$adf_d1)  || r$adf_d1  >= 0.05
      kpss_d1_ns <- !is.na(r$kpss_d1) && r$kpss_d1 >= 0.463
      if (adf_d1_ns && kpss_d1_ns) {
        motive <- c(motive, sprintf(
          "%s: prima diferenta inca nestationara (posibil I(2))", v))
      }
    }
    list(permis = length(motive) == 0, motive = motive)
  }

  # Parser pentru numele coeficientilor ARDL (conventie dynlm):
  #   "(Intercept)"  -> intercept
  #   "L(VAR, k)"    -> lag k al variabilei VAR
  #   "VAR"          -> lag 0 (contemporan) al regresorului VAR
  parse_termeni_ardl <- function(nume_coef) {
    lapply(nume_coef, function(nm) {
      if (nm == "(Intercept)") return(list(type = "intercept"))
      m <- regmatches(nm, regexec("^L\\(([^,]+),\\s*([0-9]+)\\)$", nm))[[1]]
      if (length(m) == 3)
        return(list(type = "lag", var = m[2], lag = as.integer(m[3])))
      list(type = "lag", var = nm, lag = 0L)
    })
  }

  # Prognoza recursiva ARDL din coeficienti (robusta la operatorii de lag).
  #   hist_list : lista numita var -> valori observate (dep are lungime n0)
  #   scen_list : lista numita regresor -> traiectorie viitoare (lungime h)
  prognoza_ardl_recursiv <- function(co, termeni, dep, hist_list, h, scen_list) {
    serii <- hist_list
    for (rg in names(scen_list))
      serii[[rg]] <- c(hist_list[[rg]], scen_list[[rg]])
    n0 <- length(hist_list[[dep]])
    out <- numeric(h)
    for (i in 1:h) {
      t <- n0 + i
      val <- 0
      for (j in seq_along(co)) {
        tm <- termeni[[j]]
        if (tm$type == "intercept") { val <- val + co[j]; next }
        s <- serii[[tm$var]]
        idx <- t - tm$lag
        xv <- if (!is.null(s) && idx >= 1 && idx <= length(s)) s[idx] else NA
        val <- val + co[j] * xv
      }
      out[i] <- val
      serii[[dep]] <- c(serii[[dep]], val)
    }
    out
  }

  # A.4 - Selectia + estimarea ARDL, testul Bounds, multiplicatorii LR si ECM.
  # Afiseaza pasii si intoarce o lista cu obiectele si valorile extrase.
  estimeaza_ardl <- function(Y_loc, dep = "Elec", regs = c("IPI", "TDD"),
                             max_lag = NULL, selection = "AIC") {
    fr <- frequency(Y_loc)
    if (is.null(max_lag))
      max_lag <- if (fr >= 12) 12 else if (fr >= 4) 4 else max(2, fr)
    max_ord_vec <- rep(as.integer(max_lag), length(regs) + 1)
    form <- as.formula(paste(dep, "~", paste(regs, collapse = " + ")))

    # Pachetul ARDL cere un data.frame obisnuit, nu un obiect ts/mts. Convertim
    # aici (frecventa a fost deja preluata mai sus din obiectul ts original),
    # pastrand numele coloanelor identice cu dep/regs.
    df_ardl <- as.data.frame(Y_loc)
    colnames(df_ardl) <- colnames(Y_loc)
    lipsa <- setdiff(c(dep, regs), colnames(df_ardl))
    if (length(lipsa) > 0) {
      cat("Eroare ARDL: coloane lipsa in date:", paste(lipsa, collapse = ", "),
          "(disponibile:", paste(colnames(df_ardl), collapse = ", "), ")\n")
      return(NULL)
    }

    cat("\n--- A.4.1: Selectia automata a ordinului ARDL (auto_ardl, AIC) ---\n")
    cat(sprintf("Formula: %s | max_order = c(%s) | frecventa = %d\n",
                deparse(form), paste(max_ord_vec, collapse = ","), fr))
    auto <- tryCatch(
      ARDL::auto_ardl(form, data = df_ardl, max_order = max_ord_vec,
                      selection = selection),
      error = function(e) {
        cat("Eroare auto_ardl:", conditionMessage(e), "\n"); NULL })
    if (is.null(auto) || is.null(auto$best_model)) return(NULL)
    model <- auto$best_model
    ord   <- auto$best_order
    cat("Ordin ARDL selectat (dep, ", paste(regs, collapse = ", "), "): ",
        paste(ord, collapse = ", "), "\n", sep = "")

    cat("\n--- A.4.2: Estimarea modelului ARDL ---\n")
    print(summary(model))
    cat(">>> Interpretare: lagurile lui", dep,
        "= persistenta (inertia consumului);\n")
    cat("    coeficientii regresorilor (contemporan + laguri)",
        "= efecte distribuite in timp.\n")

    cat("\n--- A.4.3: Test Bounds (cointegrare ARDL, case = 3) ---\n")
    bf <- tryCatch(ARDL::bounds_f_test(model, case = 3, alpha = 0.05),
                   error = function(e) NULL)
    bt <- tryCatch(ARDL::bounds_t_test(model, case = 3, alpha = 0.05),
                   error = function(e) NULL)
    bf_F <- bf_i0 <- bf_i1 <- bf_p <- NA
    bt_t <- bt_i0 <- bt_i1 <- bt_p <- NA
    dec_f <- dec_t <- "indisponibil"
    # Acces defensiv la tabelul Bounds: numele coloanelor difera intre versiunile
    # pachetului ARDL. Incercam numele preferat, apoi alternative, apoi pozitia;
    # daca nimic nu se potriveste, logam un avertisment in loc de NA tacut.
    get_bound_val <- function(tab, prefer, alternative, poz, eticheta) {
      if (is.null(tab) || is.null(dim(tab))) {
        cat(sprintf("AVERTISMENT bounds: tabel indisponibil pentru '%s'.\n",
                    eticheta)); return(NA)
      }
      cn <- colnames(tab)
      if (prefer %in% cn) return(as.numeric(tab[1, prefer]))
      for (a in alternative) if (a %in% cn) return(as.numeric(tab[1, a]))
      if (!is.na(poz) && poz <= ncol(tab)) {
        cat(sprintf("AVERTISMENT bounds: coloana '%s' lipseste; folosesc pozitia %d ('%s').\n",
                    prefer, poz, cn[poz]))
        return(as.numeric(tab[1, poz]))
      }
      cat(sprintf("AVERTISMENT bounds: nu pot extrage '%s' (coloane: %s).\n",
                  eticheta, paste(cn, collapse = ", ")))
      NA
    }
    if (!is.null(bf)) {
      print(bf)
      bf_F  <- get_bound_val(bf$tab, "statistic", c("F", "stat"), 1, "F-statistic")
      bf_i0 <- get_bound_val(bf$tab, "Lower-bound I(0)",
                             c("I(0)", "Lower I(0)", "lb"), 2, "limita I(0)")
      bf_i1 <- get_bound_val(bf$tab, "Upper-bound I(1)",
                             c("I(1)", "Upper I(1)", "ub"), 3, "limita I(1)")
      bf_p  <- get_bound_val(bf$tab, "p.value",
                             c("p-value", "pvalue", "p_value"), NA, "p.value (F)")
      dec_f <- if (!is.na(bf_F) && bf_F > bf_i1)
        "F > limita I(1): relatie de termen lung CONFIRMATA"
      else if (!is.na(bf_F) && bf_F < bf_i0)
        "F < limita I(0): FARA relatie de termen lung"
      else "F intre limite: rezultat INCONCLUDENT"
      cat(">>>", dec_f, "\n")
    }
    if (!is.null(bt)) {
      print(bt)
      bt_t  <- get_bound_val(bt$tab, "statistic", c("t", "stat"), 1, "t-statistic")
      bt_i0 <- get_bound_val(bt$tab, "Lower-bound I(0)",
                             c("I(0)", "Lower I(0)", "lb"), 2, "limita I(0)")
      bt_i1 <- get_bound_val(bt$tab, "Upper-bound I(1)",
                             c("I(1)", "Upper I(1)", "ub"), 3, "limita I(1)")
      bt_p  <- get_bound_val(bt$tab, "p.value",
                             c("p-value", "pvalue", "p_value"), NA, "p.value (t)")
      dec_t <- if (!is.na(bt_t) && bt_t < bt_i1)
        "t sub limita I(1): relatie de termen lung CONFIRMATA"
      else if (!is.na(bt_t) && bt_t > bt_i0)
        "t peste limita I(0): FARA relatie de termen lung"
      else "t intre limite: rezultat INCONCLUDENT"
      cat(">>>", dec_t, "\n")
    }

    cat("\n--- A.4.4: Multiplicatori pe termen lung (relatia de echilibru) ---\n")
    mult <- tryCatch(ARDL::multipliers(model, type = "lr"),
                     error = function(e) NULL)
    lr_eq <- NA
    if (!is.null(mult)) {
      print(mult)
      ic <- mult$Estimate[mult$Term == "(Intercept)"]
      const_txt <- if (length(ic)) fmt(ic[1], 3) else "0"
      termeni_eq <- sapply(regs, function(rg) {
        e <- mult$Estimate[mult$Term == rg]
        if (!length(e)) return("")
        e <- e[1]
        sprintf("%s %s*%s", if (e >= 0) "+" else "-", fmt(abs(e), 3), rg)
      })
      lr_eq <- paste(dep, "=", const_txt, paste(termeni_eq, collapse = " "))
      cat(">>> Ecuatie de echilibru (termen lung):", lr_eq, "\n")
      cat("    Semn + magnitudine + semnificatie: vezi coloana Pr(>|t|).\n")
    }

    cat("\n--- A.4.5: Forma ECM (UECM nerestrictionat + RECM restrictionat) ---\n")
    u  <- tryCatch(ARDL::uecm(model), error = function(e) NULL)
    rc <- tryCatch(ARDL::recm(model, case = 3), error = function(e) NULL)
    if (!is.null(u)) {
      cat(">>> UECM (ECM nerestrictionat):\n"); print(summary(u))
    }
    ect_row <- NULL
    if (!is.null(rc)) {
      cat("\n>>> RECM (ECM restrictionat, case = 3):\n"); print(summary(rc))
      ect_row <- tryCatch({
        cf <- summary(rc)$coefficients
        idx <- grep("ect", rownames(cf), ignore.case = TRUE)[1]
        if (is.na(idx)) NULL else cf[idx, ]
      }, error = function(e) NULL)
      if (!is.null(ect_row)) {
        est_ect <- as.numeric(ect_row[["Estimate"]])[1]; p_ect <- as.numeric(ect_row[["Pr(>|t|)"]])[1]
        stabil <- isTRUE(!is.na(est_ect) && est_ect < 0 && est_ect > -1 &&
                           !is.na(p_ect) && p_ect < 0.05)
        cat(sprintf(
          ">>> ECT = %s (p = %s). %s\n", fmt(est_ect, 4), fmt(p_ect, 4),
          if (stabil)
            sprintf("Negativ, semnificativ, in (-1,0): ajustare STABILA; %s%% din dezechilibru corectat/perioada.",
                    fmt(abs(est_ect) * 100, 1))
          else
            "ATENTIE: ECT nu indeplineste conditia (negativ, semnificativ, in (-1,0))."))
      }
    }

    list(model = model, order = ord, formula = form, dep = dep, regs = regs,
         bounds_f = bf, bounds_t = bt, multipliers = mult, uecm = u, recm = rc,
         ect = ect_row, lr_eq = lr_eq,
         bf_F = bf_F, bf_i0 = bf_i0, bf_i1 = bf_i1, bf_p = bf_p, dec_f = dec_f,
         bt_t = bt_t, bt_i0 = bt_i0, bt_i1 = bt_i1, bt_p = bt_p, dec_t = dec_t)
  }

  # A.5 - Diagnostice ARDL (ecuatie unica): BG, BP, HAC, normalitate, grafice.
  diagnostice_ardl <- function(model, dep, dir_t = NULL, cod_tara = "") {
    cat("\n--- A.5: Diagnostice ARDL (test pe o singura ecuatie) ---\n")
    bg4 <- tryCatch(lmtest::bgtest(model, order = 4)$p.value,
                    error = function(e) NA)
    bg8 <- tryCatch(lmtest::bgtest(model, order = 8)$p.value,
                    error = function(e) NA)
    cat("Breusch-Godfrey (autocorelare; H0: fara autocorelare)\n")
    cat(sprintf("  order = 4 -> p = %s\n  order = 8 -> p = %s\n",
                fmt(bg4), fmt(bg8)))
    bp <- tryCatch(lmtest::bptest(model)$p.value, error = function(e) NA)
    cat("Breusch-Pagan (heteroscedasticitate; H0: homoscedasticitate)\n")
    cat(sprintf("  p = %s\n", fmt(bp)))
    res <- tryCatch(as.numeric(residuals(model)), error = function(e) NULL)
    jb <- if (!is.null(res))
      tryCatch(tseries::jarque.bera.test(res)$p.value, error = function(e) NA) else NA
    sw <- if (!is.null(res))
      tryCatch(shapiro.test(res)$p.value, error = function(e) NA) else NA
    cat("Normalitate reziduuri (H0: normale)\n")
    cat(sprintf("  Jarque-Bera p = %s | Shapiro-Wilk p = %s\n",
                fmt(jb), fmt(sw)))

    are_probleme <- (!is.na(bg4) && bg4 < 0.05) || (!is.na(bp) && bp < 0.05)
    if (are_probleme) {
      cat("\n>>> Detectata autocorelare/heteroscedasticitate -> erori robuste HAC (Newey-West):\n")
      tryCatch(print(lmtest::coeftest(model, vcov. = sandwich::NeweyWest(model))),
               error = function(e)
                 cat("Eroare coeftest HAC:", conditionMessage(e), "\n"))
    } else {
      cat(">>> Fara probleme majore de autocorelare/heteroscedasticitate la 5%.\n")
    }

    # Grafice reziduuri (serie + ACF) prin helperii existenti.
    if (!is.null(dir_t) && !is.null(res)) {
      tryCatch({
        df_r <- data.frame(t = seq_along(res), rez = res)
        g_r <- ggplot(df_r, aes(x = t, y = rez)) +
          geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
          geom_line(color = "steelblue") +
          ggtitle(paste("Reziduuri ARDL", dep, "-", cod_tara)) +
          xlab("Observatie") + ylab("Reziduu") + theme_minimal()
        afiseaza_si_salveaza_ggplot(g_r,
          file.path(dir_t, paste0("04_reziduuri_serie_", dep, ".png")))
      }, error = function(e)
        cat("Eroare grafic reziduuri ARDL:", conditionMessage(e), "\n"))
      tryCatch({
        a_r <- Acf(res, lag.max = 24, plot = FALSE)
        afiseaza_si_salveaza_base(
          quote(plot(a_r, main = paste("ACF reziduuri ARDL", dep, "-", cod_tara))),
          file.path(dir_t, paste0("05_reziduuri_acf_", dep, ".png")))
      }, error = function(e)
        cat("Eroare ACF reziduuri ARDL:", conditionMessage(e), "\n"))
    }
    list(bg4 = bg4, bg8 = bg8, bp = bp, jb = jb, sw = sw)
  }

  # A.6 - Stabilitate ARDL (CUSUM + OLS-CUSUM) prin strucchange::efp.
  # Abordare robusta: model.frame -> prima coloana redenumita 'y' -> formula y ~ .
  cusum_ardl <- function(model, dir_t = NULL, cod_tara = "") {
    cat("\n--- A.6: Stabilitate ARDL (CUSUM, OLS-CUSUM) ---\n")
    cat("H0: coeficienti stabili (relevant pentru socurile din perioada:\n")
    cat("    criza financiara, COVID, criza energetica).\n")
    mf <- tryCatch(model.frame(model), error = function(e) NULL)
    if (is.null(mf)) { cat("Nu pot extrage model.frame.\n"); return(list(stabil = NA)) }
    names(mf)[1] <- "y"
    p_rec <- p_ols <- NA
    e_rec <- tryCatch(strucchange::efp(y ~ ., data = mf, type = "Rec-CUSUM"),
                      error = function(e) NULL)
    e_ols <- tryCatch(strucchange::efp(y ~ ., data = mf, type = "OLS-CUSUM"),
                      error = function(e) NULL)
    if (!is.null(e_rec)) {
      sc <- tryCatch(strucchange::sctest(e_rec), error = function(e) NULL)
      if (!is.null(sc)) { cat("\nRecursive CUSUM:\n"); print(sc); p_rec <- sc$p.value }
      if (!is.null(dir_t))
        tryCatch(afiseaza_si_salveaza_base(
          quote(plot(e_rec, main = paste("CUSUM (recursiv) -", cod_tara))),
          file.path(dir_t, "06_cusum.png")),
          error = function(e) cat("Eroare plot CUSUM:", conditionMessage(e), "\n"))
    }
    if (!is.null(e_ols)) {
      sc2 <- tryCatch(strucchange::sctest(e_ols), error = function(e) NULL)
      if (!is.null(sc2)) { cat("\nOLS-CUSUM:\n"); print(sc2); p_ols <- sc2$p.value }
      if (!is.null(dir_t))
        tryCatch(afiseaza_si_salveaza_base(
          quote(plot(e_ols, main = paste("OLS-CUSUM -", cod_tara))),
          file.path(dir_t, "07_ols_cusum.png")),
          error = function(e) cat("Eroare plot OLS-CUSUM:", conditionMessage(e), "\n"))
    }
    stabil <- (is.na(p_rec) || p_rec > 0.05) && (is.na(p_ols) || p_ols > 0.05)
    cat(sprintf("\n>>> Stabilitate coeficienti: %s (CUSUM p=%s, OLS-CUSUM p=%s)\n",
                if (isTRUE(stabil)) "DA (nu respinge H0)" else "NU (posibile rupturi)",
                fmt(p_rec), fmt(p_ols)))
    list(stabil = stabil, p_rec = p_rec, p_ols = p_ols)
  }

  # A.7 - Evaluare OOS ARDL: acelasi cadru ca VAR (train/test, benchmark-uri
  # naive, sub-orizonturi), cu prognoza recursiva inertiala a regresorilor.
  oos_ardl <- function(Y, dep, regs, best_order, h_test, ordine_int,
                       dir_t, cod_tara) {
    metric_oos <- list(rmse = NA, mae = NA, mape = NA,
                       rmse_n = NA, mae_n = NA, mape_n = NA,
                       rmse_seas = NA, mae_seas = NA, mape_seas = NA,
                       rmse_mean = NA, mae_mean = NA, mape_mean = NA,
                       sub_horizon = NULL)
    n_total <- nrow(Y)
    if (n_total <= h_test + 24) {
      cat(sprintf("AVERTISMENT: doar %d obs, OOS %d cere >= %d. Sar OOS.\n",
                  n_total, h_test, h_test + 24))
      return(metric_oos)
    }
    Y_train <- window(Y, end = time(Y)[n_total - h_test])
    form <- as.formula(paste(dep, "~", paste(regs, collapse = " + ")))
    # ARDL cere data.frame: convertim sub-esantionul de antrenament (ramane si
    # ca ts pentru extragerea istoricului / scenariului de mai jos).
    df_train <- as.data.frame(Y_train)
    colnames(df_train) <- colnames(Y_train)
    model_tr <- tryCatch(
      ARDL::ardl(form, data = df_train, order = unname(as.integer(best_order))),
      error = function(e) {
        cat("Eroare re-estimare ARDL pe train:", conditionMessage(e), "\n"); NULL })
    if (is.null(model_tr)) return(metric_oos)

    co <- coef(model_tr); termeni <- parse_termeni_ardl(names(co))
    hist_list <- list()
    for (v in c(dep, regs)) hist_list[[v]] <- as.numeric(Y_train[, v])
    scen_list <- list()
    for (rg in regs)
      scen_list[[rg]] <- rep(tail(as.numeric(Y_train[, rg]), 1), h_test)
    cat("Scenariu OOS: regresori inertiali (ultima valoare observata mentinuta).\n")
    fc_dep <- prognoza_ardl_recursiv(co, termeni, dep, hist_list, h_test, scen_list)

    actual <- as.numeric(window(Y, start = time(Y)[n_total - h_test + 1])[, dep])
    n_min <- min(length(fc_dep), length(actual))
    fc_dep <- fc_dep[1:n_min]; actual <- actual[1:n_min]

    ultim_train <- as.numeric(tail(Y_train[, dep], 1))
    naiv_rw   <- rep(ultim_train, length(actual))
    naiv_mean <- rep(mean(as.numeric(Y_train[, dep]), na.rm = TRUE), length(actual))
    all_dep <- as.numeric(Y[, dep])
    idx_start_test <- length(all_dep) - length(actual) + 1
    naiv_seas <- sapply(seq_along(actual), function(i) {
      idx_acum <- idx_start_test + i - 1
      if (idx_acum - 12 >= 1) all_dep[idx_acum - 12] else NA
    })

    m_ardl <- calc_metrici_oos(actual, fc_dep)
    m_rw   <- calc_metrici_oos(actual, naiv_rw)
    m_seas <- calc_metrici_oos(actual, naiv_seas)
    m_mean <- calc_metrici_oos(actual, naiv_mean)

    orizonturi <- intersect(c(1, 3, 6, 12, h_test), 1:length(actual))
    sub_horizon <- do.call(rbind, lapply(orizonturi, function(h) {
      m_h <- calc_metrici_oos(actual[1:h], fc_dep[1:h])
      data.frame(h = h, RMSE = round(m_h$rmse, 2), MAE = round(m_h$mae, 2),
                 MAPE = round(m_h$mape, 2), stringsAsFactors = FALSE)
    }))
    cat("\nMetrici ARDL pe sub-orizonturi:\n"); print(sub_horizon, row.names = FALSE)
    cat(sprintf("\nBenchmark-uri (h=%d):\n", length(actual)))
    cat(sprintf("  Model ARDL : RMSE=%s MAE=%s MAPE=%s%%\n",
                fmt(m_ardl$rmse, 2), fmt(m_ardl$mae, 2), fmt(m_ardl$mape, 2)))
    cat(sprintf("  Naiv RW    : RMSE=%s MAE=%s MAPE=%s%% (potrivit I(1))\n",
                fmt(m_rw$rmse, 2), fmt(m_rw$mae, 2), fmt(m_rw$mape, 2)))
    cat(sprintf("  Naiv MEAN  : RMSE=%s MAE=%s MAPE=%s%% (potrivit I(0))\n",
                fmt(m_mean$rmse, 2), fmt(m_mean$mae, 2), fmt(m_mean$mape, 2)))
    cat(sprintf("  Naiv SEAS  : RMSE=%s MAE=%s MAPE=%s%% (luna t-12)\n",
                fmt(m_seas$rmse, 2), fmt(m_seas$mae, 2), fmt(m_seas$mape, 2)))

    metric_oos <- list(
      rmse = m_ardl$rmse, mae = m_ardl$mae, mape = m_ardl$mape,
      rmse_n = m_rw$rmse, mae_n = m_rw$mae, mape_n = m_rw$mape,
      rmse_seas = m_seas$rmse, mae_seas = m_seas$mae, mape_seas = m_seas$mape,
      rmse_mean = m_mean$rmse, mae_mean = m_mean$mae, mape_mean = m_mean$mape,
      sub_horizon = sub_horizon)

    elec_is_i0 <- isTRUE(!is.na(ordine_int[[dep]]) && ordine_int[[dep]] == 0)
    naiv_potrivit <- if (elec_is_i0) naiv_mean else naiv_rw
    eticheta_naiv <- if (elec_is_i0) "Naiv MEAN" else "Naiv RW"
    tryCatch({
      df_plot <- data.frame(t = seq_along(actual), real = actual,
                            prognoza = fc_dep, naiv_potrivit = naiv_potrivit,
                            naiv_seas = naiv_seas)
      g <- ggplot(df_plot, aes(x = t)) +
        geom_line(aes(y = real, color = "Real"), linewidth = 1) +
        geom_line(aes(y = prognoza, color = "Prognoza ARDL"), linewidth = 1) +
        geom_line(aes(y = naiv_potrivit, color = eticheta_naiv),
                  linewidth = 0.8, linetype = "dashed") +
        geom_line(aes(y = naiv_seas, color = "Naiv SEAS (t-12)"),
                  linewidth = 0.8, linetype = "dotted") +
        labs(title = paste("Prognoza OOS ARDL", cod_tara),
             x = "Luna in setul de test", y = paste(dep), color = NULL) +
        theme_minimal()
      afiseaza_si_salveaza_ggplot(g, file.path(dir_t, "03_prognoza_oos.png"))
    }, error = function(e) cat("Eroare grafic OOS ARDL:", conditionMessage(e), "\n"))
    metric_oos
  }

  # ----------------------------- PARTEA B.1 --------------------------------
  # Modulele radacinilor sistemului. VAR/VAR_DIFF: roots() direct. VECM
  # (vec2var): construim forma companion din coeficientii VAR in niveluri ($A).
  radacini_sistem <- function(var_obj, tip_model) {
    if (inherits(var_obj, "varest")) {
      r <- tryCatch(as.numeric(vars::roots(var_obj)), error = function(e) NA)
      return(list(moduli = r, nota = NULL))
    }
    r <- tryCatch({
      A <- var_obj$A
      p <- length(A); k <- nrow(A[[1]])
      Amat <- do.call(cbind, A)
      comp <- if (p == 1) Amat else
        rbind(Amat, cbind(diag(k * (p - 1)), matrix(0, k * (p - 1), k)))
      Mod(eigen(comp, only.values = TRUE)$values)
    }, error = function(e) NA)
    list(moduli = r,
         nota = "VECM: prin constructie ~ (k-r) radacini = 1 (cointegrare); restul < 1.")
  }

  # R^2 si nr. coeficienti semnificativi (5%) per ecuatie.
  adecvare_ecuatii <- function(var_obj, tip_model, johansen_obj = NULL, rang = 1) {
    tryCatch({
      if (inherits(var_obj, "varest")) {
        sv <- summary(var_obj)$varresult
        do.call(rbind, lapply(names(sv), function(nm) {
          cf <- sv[[nm]]$coefficients
          data.frame(Ecuatie = nm, R2 = round(sv[[nm]]$r.squared, 4),
                     Sig5 = sum(cf[, 4] < 0.05, na.rm = TRUE),
                     Coef = nrow(cf), stringsAsFactors = FALSE)
        }))
      } else if (!is.null(johansen_obj)) {
        cj <- cajorls(johansen_obj, r = rang); sr <- summary(cj$rlm)
        do.call(rbind, lapply(seq_along(sr), function(i) {
          s <- sr[[i]]; cf <- s$coefficients
          data.frame(Ecuatie = names(sr)[i], R2 = round(s$r.squared, 4),
                     Sig5 = sum(cf[, 4] < 0.05, na.rm = TRUE),
                     Coef = nrow(cf), stringsAsFactors = FALSE)
        }))
      } else NULL
    }, error = function(e) NULL)
  }

  # Valori FEVD numerice la orizonturile cerute, pentru fiecare variabila.
  extrage_fevd_numeric <- function(fevd_obj, orizonturi) {
    if (is.null(fevd_obj)) return(NULL)
    tryCatch({
      do.call(rbind, lapply(names(fevd_obj), function(v) {
        M <- fevd_obj[[v]]
        hh <- orizonturi[orizonturi >= 1 & orizonturi <= nrow(M)]
        sub <- round(M[hh, , drop = FALSE] * 100, 1)
        data.frame(Variabila = v, Orizont = hh, sub,
                   check.names = FALSE, row.names = NULL,
                   stringsAsFactors = FALSE)
      }))
    }, error = function(e) NULL)
  }

  # Sumar IRF: pentru fiecare pereche soc->raspuns, daca banda exclude zero
  # (semnificativ) si semnul raspunsului de varf.
  rezuma_irf <- function(irf_obj) {
    if (is.null(irf_obj)) return(NULL)
    tryCatch({
      randuri <- list()
      for (imp in names(irf_obj$irf)) {
        M <- irf_obj$irf[[imp]]; Lo <- irf_obj$Lower[[imp]]; Up <- irf_obj$Upper[[imp]]
        for (resp in colnames(M)) {
          pt <- M[, resp]; lo <- Lo[, resp]; up <- Up[, resp]
          excl <- (lo > 0 & up > 0) | (lo < 0 & up < 0)
          ipk <- which.max(abs(pt))
          randuri[[length(randuri) + 1]] <- data.frame(
            Soc = imp, Raspuns = resp,
            Semnificativ = if (any(excl, na.rm = TRUE)) "DA" else "nu",
            Semn = if (pt[ipk] > 0) "+" else if (pt[ipk] < 0) "-" else "0",
            Varf = round(pt[ipk], 4), stringsAsFactors = FALSE)
        }
      }
      do.call(rbind, randuri)
    }, error = function(e) NULL)
  }

  # ==========================================================================
  # FUNCTIA PRINCIPALA
  # ==========================================================================
  analiza_multivariata <- function(cod_tara, panel_data) {
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat("--- Multivariata:", cod_tara, "---\n")
    cat(strrep("=", 70), "\n", sep = "")
    dir_t <- file.path(DIR_MULTIVARIATA, cod_tara)
    if (!dir.exists(dir_t)) dir.create(dir_t, recursive = TRUE)

    date_tara <- panel_data %>%
      dplyr::filter(geo == cod_tara) %>%
      dplyr::arrange(data)
    if (nrow(date_tara) < MIN_OBS) {
      cat("Tara are sub", MIN_OBS, "observatii. Sare.\n"); return(NULL)
    }

    data_inceput <- min(date_tara$data)
    elec_ts <- construieste_ts(date_tara$elec_gwh, data_inceput)
    ipi_ts  <- construieste_ts(date_tara$ipi,      data_inceput)
    tdd_ts  <- construieste_ts(date_tara$tdd,      data_inceput)

    # ----------------------------------------------------------------
    # PASUL 0: Ajustare sezoniera (Elec si TDD; IPI deja SCA din Eurostat)
    # ----------------------------------------------------------------
    cat("\n--- PASUL 0: Ajustare sezoniera ---\n")
    sa_elec <- ajusteaza_sezonier(elec_ts, paste("Elec", cod_tara), dir_t)
    sa_tdd  <- ajusteaza_sezonier(tdd_ts,  paste("TDD",  cod_tara), dir_t)
    sa_ipi  <- list(sa = ipi_ts, original = ipi_ts, a_fost_ajustata = FALSE,
                    motor = "deja_SA (Eurostat SCA)",
                    test_canova_hansen_p = NA)

    elec_sa <- sa_elec$sa
    ipi_sa  <- sa_ipi$sa
    tdd_sa  <- sa_tdd$sa

    Y <- cbind(Elec = elec_sa, IPI = ipi_sa, TDD = tdd_sa)
    n_total <- nrow(Y)

    ghid_si_pauza(c(
      ">>> Ajustare sezoniera aplicata. Verifica graficul de decompozitie.",
      "    - Daca ajustarea pare distorsionata, considera RSA3 in loc de RSA5."
    ))

    # Log al alegerilor utilizatorului (pentru rezumat)
    log_choices <- list()

    # ----------------------------------------------------------------
    # CHECKPOINT 1: country opt-in
    # ----------------------------------------------------------------
    cat(sprintf("\nObservatii: %d  |  Interval: %s -> %s\n",
                n_total,
                format(min(date_tara$data), "%Y-%m"),
                format(max(date_tara$data), "%Y-%m")))
    cat(sprintf("Medii: Elec = %s GWh  |  IPI = %s  |  TDD = %s\n",
                fmt(mean(elec_ts, na.rm = TRUE), 1),
                fmt(mean(ipi_ts,  na.rm = TRUE), 2),
                fmt(mean(tdd_ts,  na.rm = TRUE), 1)))

    raspuns_co <- tolower(trimws(citeste_input(sprintf(
      "Ruleaza analiza multivariata pentru %s? (y/n/skip-all) [default y]: ",
      cod_tara))))
    if (raspuns_co == "skip-all") {
      cat("Skip-all ales. Oprire intregii analize multivariate.\n")
      return(list(skip_all = TRUE, tara = cod_tara))
    }
    if (raspuns_co != "" && !raspuns_da(raspuns_co)) {
      cat("Sare tara", cod_tara, "la cererea utilizatorului.\n")
      return(NULL)
    }
    log_choices$ran_country <- "y"

    # ----------------------------------------------------------------
    # Curatare tintita (dupa opt-in, ca sa nu pierdem rezultate bune daca
    # utilizatorul abandoneaza la CHECKPOINT 1): sterge rezultatele generate
    # anterior pentru aceasta tara (rezumat + grafice de analiza), ca sa nu
    # ramana fisiere de la un model diferit rulat inainte (ex: rezumat_VAR_DIFF
    # cand acum rulam ARDL). EXCLUDEM graficele SA (00_sa_*.png) fiindca au fost
    # generate chiar acum, in PASUL 0 (inainte de acest checkpoint).
    fisiere_vechi <- list.files(dir_t,
      pattern = "\\.png$|^rezumat_.*\\.txt$", full.names = TRUE)
    fisiere_vechi <- fisiere_vechi[!grepl("^00_sa_", basename(fisiere_vechi))]
    if (length(fisiere_vechi) > 0) {
      unlink(fisiere_vechi)
      cat("Curatat", length(fisiere_vechi), "fisiere vechi din", dir_t, "\n")
    }

    # ----------------------------------------------------------------
    # CHECKPOINT 2: ordinul de integrare per serie
    # ----------------------------------------------------------------
    cat("\n--- CHECKPOINT 2: Ordin de integrare ---\n")
    rapoarte <- list(
      Elec = raport_stationaritate(elec_ts),
      IPI  = raport_stationaritate(ipi_ts),
      TDD  = raport_stationaritate(tdd_ts)
    )
    cat(sprintf("%-6s %-13s %-13s %-13s %-13s\n",
                "Var", "ADF_lvl_p", "KPSS_lvl", "ADF_d1_p", "KPSS_d1"))
    cat(strrep("-", 60), "\n", sep = "")
    for (nm in names(rapoarte)) {
      r <- rapoarte[[nm]]
      cat(sprintf("%-6s %-13s %-13s %-13s %-13s\n", nm,
                  fmt(r$adf_lvl, 4), fmt(r$kpss_lvl, 4),
                  fmt(r$adf_d1, 4),  fmt(r$kpss_d1, 4)))
    }
    cat("(KPSS: crit. 5% = 0.463; stat mai mic = stationara)\n")

    # ACF + PACF pentru cele 3 serii SA in nivel - ajuta la clasificare.
    # Refolosim helperul combo din analiza univariata pentru consistenta.
    for (nm_v in c("Elec", "IPI", "TDD")) {
      s_v <- Y[, nm_v]
      tryCatch({
        a_v <- Acf(s_v,  lag.max = 36, plot = FALSE)
        p_v <- Pacf(s_v, lag.max = 36, plot = FALSE)
        afiseaza_si_salveaza_acf_pacf_combo(
          a_v, p_v,
          file.path(dir_t, paste0("00_acf_pacf_", nm_v, ".png")),
          titlu_acf  = paste("ACF",  nm_v, "(SA, nivel) -", cod_tara),
          titlu_pacf = paste("PACF", nm_v, "(SA, nivel) -", cod_tara))
      }, error = function(e)
        cat("Eroare ACF/PACF", nm_v, ":", conditionMessage(e), "\n"))
    }
    ghid_si_pauza(c(
      ">>> Verifica ACF/PACF, apoi clasifica fiecare serie I(0)/I(1)/other."
    ))

    ordine_auto <- sapply(list(Elec = elec_ts, IPI = ipi_ts, TDD = tdd_ts),
                          ordin_integrare)
    ordine_int <- c(
      Elec = cere_clasificare("Elec", ordine_auto[["Elec"]]),
      IPI  = cere_clasificare("IPI",  ordine_auto[["IPI"]]),
      TDD  = cere_clasificare("TDD",  ordine_auto[["TDD"]])
    )
    log_choices$ordin_user <- ordine_int
    toate_i0 <- all(ordine_int == 0, na.rm = TRUE)
    toate_i1 <- all(ordine_int == 1, na.rm = TRUE)
    cat("Clasificare folosita: Elec =", ordine_int["Elec"],
        ", IPI =", ordine_int["IPI"], ", TDD =", ordine_int["TDD"], "\n")

    # ----------------------------------------------------------------
    # CHECKPOINT 3: lag selection
    # ----------------------------------------------------------------
    cat("\n--- CHECKPOINT 3: Selectia lagului ---\n")
    lag_sel <- tryCatch(VARselect(Y, lag.max = 12, type = "const"),
                        error = function(e) NULL)
    if (is.null(lag_sel)) {
      cat("Eroare VARselect. Sare tara.\n"); return(NULL)
    }
    cat("Criterii pentru lagurile 1..12 (linie = criteriu, coloana = lag):\n")
    print(round(lag_sel$criteria, 2))
    cat("\nLag selectat automat:\n")
    print(lag_sel$selection)

    lag_aic <- as.integer(lag_sel$selection["AIC(n)"])
    lag_sc  <- as.integer(lag_sel$selection["SC(n)"])
    ghid_si_pauza(c(
      ">>> Accepta orice intreg 1..12. ENTER = AIC."
    ))
    lag_chosen_raw <- trimws(citeste_input(sprintf(
      "Lag de folosit (1-12) [default = AIC = %d]: ", lag_aic)))
    if (lag_chosen_raw == "") {
      lag_optim <- lag_aic
    } else {
      lag_optim <- suppressWarnings(as.integer(lag_chosen_raw))
      if (is.na(lag_optim) || lag_optim < 1 || lag_optim > 12) {
        cat("Valoare invalida, folosim AIC =", lag_aic, "\n")
        lag_optim <- lag_aic
      }
    }
    if (lag_optim < 1) lag_optim <- 1
    log_choices$lag <- lag_optim
    cat("Lag folosit:", lag_optim, "\n")

    # ----------------------------------------------------------------
    # CHECKPOINT 4: branch decision (VAR levels / VAR diff / VECM)
    # ----------------------------------------------------------------
    cat("\n--- CHECKPOINT 4: Tip model si rang cointegrare ---\n")
    cat("\n+--------------------------------+---------------------------+-------------------------------+\n")
    cat("| Situatie                       | Model recomandat          | Rezultate disponibile         |\n")
    cat("+--------------------------------+---------------------------+-------------------------------+\n")
    cat("| Serii I(0)                     | VAR in nivel              | Granger scurt, IRF, FEVD      |\n")
    cat("| Serii I(1), cointegrate        | VECM                      | Granger scurt + lung, IRF,FEVD|\n")
    cat("| Serii I(1), necointegrate      | VAR in prime diferente    | Granger scurt, IRF, FEVD      |\n")
    cat("| Serii cu ordine diferite       | ARDL (var. dep. = Elec)   | Bounds test, ECT,             |\n")
    cat("|   (ex: I(0) si I(1))           |                           | termen lung+scurt, CUSUM      |\n")
    cat("+--------------------------------+---------------------------+-------------------------------+\n")
    cat(sprintf("Clasificare curenta: Elec=%s, IPI=%s, TDD=%s\n",
                ordine_int["Elec"], ordine_int["IPI"], ordine_int["TDD"]))
    # Verificare anti-I(2) inainte de a permite ramura ARDL
    anti_i2 <- verifica_anti_i2(ordine_int, rapoarte)
    ardl_permis <- anti_i2$permis

    linie_mat <- if (all(ordine_int == 0, na.rm = TRUE))
      "-> Linia 1 (toate I(0)): VAR in nivel."
    else if (all(ordine_int == 1, na.rm = TRUE))
      "-> Linia 2 sau 3 (toate I(1)): VECM daca cointegreaza, altfel VAR diff."
    else if (ardl_permis)
      "-> Linia 4 (mix I(0)/I(1)): ARDL (var. dep. = Elec); default = ARDL."
    else
      "-> Linia 4 (mix): ARDL indisponibil (vezi avertisment); default = VAR diff."
    cat(linie_mat, "\n")
    if (!toate_i0 && !toate_i1 && !ardl_permis) {
      cat("\n>>> AVERTISMENT: ARDL necesita ca nicio serie sa nu fie I(2).\n")
      for (mm in anti_i2$motive) cat("    -", mm, "\n")
      cat("    ARDL este dezactivat; foloseste VAR pe diferente.\n")
    }

    # Determinam default automat
    if (toate_i0) {
      auto_tip <- "VAR"
      auto_rang <- 0
    } else if (toate_i1) {
      auto_tip <- NA  # se decide dupa Johansen
      auto_rang <- 0
    } else {
      # Mix I(0)/I(1): default ARDL daca e permis, altfel VAR pe diferente
      auto_tip <- if (ardl_permis) "ARDL" else "VAR_DIFF"
      auto_rang <- 0
    }

    johansen_obj_full <- NULL
    if (!toate_i0) {
      # Calculam si afisam Johansen (trace + eigen) cand este relevant
      johansen_obj_full <- tryCatch(
        ca.jo(Y, type = "trace", ecdet = "const",
              K = max(2, lag_optim)),
        error = function(e) NULL)
      johansen_eigen <- tryCatch(
        ca.jo(Y, type = "eigen", ecdet = "const",
              K = max(2, lag_optim)),
        error = function(e) NULL)
      if (!is.null(johansen_obj_full)) {
        cat("\n>>> Johansen TRACE test:\n")
        print(summary(johansen_obj_full))
        auto_rang <- rang_cointegrare(johansen_obj_full)
        if (is.na(auto_tip)) auto_tip <-
          if (!is.na(auto_rang) && auto_rang > 0) "VECM" else "VAR_DIFF"
      }
      if (!is.null(johansen_eigen)) {
        cat("\n>>> Johansen MAXEIGEN test:\n")
        print(summary(johansen_eigen))
      }
    }

    cat(sprintf("\nClasificare integrare: toate_i0=%s  toate_i1=%s\n",
                toate_i0, toate_i1))
    cat(sprintf("Default automat: tip = %s, rang = %s\n",
                auto_tip, auto_rang))
    ghid_si_pauza(c(
      ">>> Alegeri pentru tipul modelului:",
      "    1 = VECM cu rang r (necesar cointegrare)",
      "    2 = VAR pe primele diferente",
      "    3 = VAR in niveluri",
      "    4 = ARDL (recomandat pentru mix I(0)/I(1))",
      "    ENTER = pastreaza default-ul afisat"
    ))
    branch_raw <- trimws(citeste_input("Alegere [1/2/3/4]: "))
    if (branch_raw == "1") {
      tip_model <- "VECM"
    } else if (branch_raw == "2") {
      tip_model <- "VAR_DIFF"
    } else if (branch_raw == "3") {
      tip_model <- "VAR"
    } else if (branch_raw == "4") {
      if (ardl_permis) {
        tip_model <- "ARDL"
      } else {
        cat("ARDL indisponibil (o serie pare I(2) sau 'other');",
            "folosesc VAR pe diferente.\n")
        tip_model <- "VAR_DIFF"
      }
    } else {
      tip_model <- auto_tip
    }

    rang_coint <- 0
    if (tip_model == "VECM") {
      default_r <- if (!is.na(auto_rang) && auto_rang > 0) auto_rang else 1
      r_raw <- trimws(citeste_input(sprintf(
        "Rang cointegrare r (1 sau 2) [default = %d]: ", default_r)))
      if (r_raw == "") {
        rang_coint <- default_r
      } else {
        rang_coint <- suppressWarnings(as.integer(r_raw))
        if (is.na(rang_coint) || rang_coint < 1 || rang_coint > 2) {
          cat("r invalid, folosim default", default_r, "\n")
          rang_coint <- default_r
        }
      }
    }
    log_choices$tip_model <- tip_model
    log_choices$rang_coint <- rang_coint
    cat("Tip model folosit:", tip_model, "  Rang:", rang_coint, "\n")

    # ==================================================================
    # RAMURA ARDL (mix I(0)/I(1)) - pipeline complet, separat de VAR/VECM.
    # Implementeaza A.2 (var. dependenta), A.4 (estimare+bounds+LR+ECM),
    # A.5 (diagnostice), A.6 (CUSUM), A.7 (OOS), A.8 (skip IRF/FEVD/Granger)
    # si B.2/B.3 (rezumat dedicat). Se incheie cu return, deci fluxul VAR/VECM
    # de mai jos (CHECKPOINT 5-10) ramane neatins.
    # ==================================================================
    if (tip_model == "ARDL") {
      # Garda finala anti-I(2) (ar trebui deja asigurata de ardl_permis).
      if (!ardl_permis) {
        cat("ARDL nu este permis pentru aceasta tara (I(2)/other). Sare.\n")
        return(NULL)
      }

      # ---------- A.2: Variabila dependenta si regresori ----------
      cat("\n--- CHECKPOINT 4-ARDL: Variabila dependenta si regresori ---\n")
      cat("ARDL nu este sistem simetric: necesita o variabila dependenta",
          "desemnata.\n")
      cat("Pentru acest sistem, dependenta naturala este Elec (consum",
          "electric), explicata prin IPI si TDD.\n")
      toate_var <- colnames(Y)
      dep_raw <- trimws(citeste_input(
        "Variabila dependenta ARDL [default = Elec]: "))
      dep_ardl <- if (dep_raw == "") "Elec" else dep_raw
      if (!(dep_ardl %in% toate_var)) {
        cat("Variabila", dep_ardl, "necunoscuta; folosesc Elec.\n")
        dep_ardl <- "Elec"
      }
      reg_default <- setdiff(c("Elec", "IPI", "TDD"), dep_ardl)
      reg_raw <- trimws(citeste_input(sprintf(
        "Regresori [default = %s]: ", paste(reg_default, collapse = ", "))))
      regs_ardl <- if (reg_raw == "") reg_default else
        trimws(strsplit(reg_raw, ",")[[1]])
      regs_ardl <- regs_ardl[regs_ardl %in% toate_var & regs_ardl != dep_ardl]
      if (length(regs_ardl) == 0) {
        cat("Regresori invalizi; folosesc default-ul.\n")
        regs_ardl <- reg_default
      }
      fr_ardl <- frequency(Y)
      ml_def <- if (fr_ardl >= 12) 12 else if (fr_ardl >= 4) 4 else max(2, fr_ardl)
      ml_raw <- trimws(citeste_input(sprintf(
        "Ordin maxim laguri auto_ardl [default = %d (un an, frecventa=%d)]: ",
        ml_def, fr_ardl)))
      max_lag_ardl <- if (ml_raw == "") ml_def else
        suppressWarnings(as.integer(ml_raw))
      if (is.na(max_lag_ardl) || max_lag_ardl < 1) max_lag_ardl <- ml_def
      cat(sprintf("ARDL: dep = %s | regresori = %s | max_order = c(%s)\n",
                  dep_ardl, paste(regs_ardl, collapse = ", "),
                  paste(rep(max_lag_ardl, length(regs_ardl) + 1), collapse = ",")))
      log_choices$ardl_dep    <- dep_ardl
      log_choices$ardl_regs   <- paste(regs_ardl, collapse = ", ")
      log_choices$ardl_maxlag <- max_lag_ardl

      # ---------- A.4: Estimare + Bounds + multiplicatori LR + ECM ----------
      est_ardl <- estimeaza_ardl(Y, dep_ardl, regs_ardl,
                                 max_lag = max_lag_ardl, selection = "AIC")
      if (is.null(est_ardl)) {
        # Scriem un rezumat minimal de diagnostic in loc de return(NULL) tacut,
        # ca sa existe intotdeauna un fisier rezumat_<TARA>.txt.
        cale_rez <- file.path(dir_t, paste0("rezumat_", cod_tara, ".txt"))
        writeLines(c(
          paste("Rezumat ARDL pentru tara:", cod_tara),
          paste(rep("=", 60), collapse = ""),
          "EROARE: estimarea ARDL a esuat (auto_ardl a returnat NULL).",
          paste("  Variabila dependenta:", dep_ardl),
          paste("  Regresori:", paste(regs_ardl, collapse = ", ")),
          paste("  Ordin maxim incercat:", max_lag_ardl),
          "Verifica datele de intrare (NA, coloane lipsa) si conversia",
          "mts -> data.frame. Vezi consola pentru mesajul exact de eroare."
        ), cale_rez)
        cat("Estimarea ARDL a esuat; rezumat minimal scris la:", cale_rez, "\n")
        return(NULL)
      }
      ardl_model <- est_ardl$model
      ghid_si_pauza(c(
        ">>> Rezultatele estimarii ARDL sunt afisate mai sus."
      ))

      # ---------- A.5: Diagnostice ----------
      # tryCatch: o eroare aici nu trebuie sa opreasca scrierea rezumatului.
      diag_ardl <- tryCatch(
        diagnostice_ardl(ardl_model, dep_ardl, dir_t, cod_tara),
        error = function(e) {
          cat("[ARDL] diagnostice esuate:", conditionMessage(e), "\n")
          list(bg4 = NA, bg8 = NA, bp = NA, jb = NA, sw = NA)
        })
      ghid_si_pauza(c(
        ">>> p < 0.05 = problema (autocorelare / heteroscedasticitate /",
        "    non-normalitate). Daca exista, raportam erori robuste HAC."
      ))

      # ---------- A.6: Stabilitate (CUSUM) ----------
      # tryCatch: strucchange::efp poate fi fragil; nu blocam rezumatul.
      cusum_res <- tryCatch(
        cusum_ardl(ardl_model, dir_t, cod_tara),
        error = function(e) {
          cat("[ARDL] CUSUM esuat:", conditionMessage(e), "\n")
          list(stabil = NA, p_rec = NA, p_ols = NA)
        })
      ghid_si_pauza(c(
        ">>> CUSUM/OLS-CUSUM: verifica daca traiectoria ramane in benzile de 5%."
      ))

      # ---------- A.8: IRF/FEVD/Granger nu se aplica ----------
      cat("\n--- ARDL: IRF / FEVD / Granger multivariat ---\n")
      cat("ARDL este model cu variabila dependenta unica; IRF/FEVD/Granger\n")
      cat("multivariat nu se aplica. Relatiile sunt date de Bounds test (termen lung),\n")
      cat("coeficientii ECM (ajustare) si dinamica pe termen scurt.\n")

      # ---------- A.7: OOS (acelasi cadru ca VAR) ----------
      cat("\n--- CHECKPOINT 10 (ARDL): Orizont out-of-sample ---\n")
      cat("Prognoza ARDL recursiva cu regresori inertiali (ultima valoare).\n")
      h_raw <- trimws(citeste_input("Orizont OOS in luni [default 24]: "))
      h_test <- if (h_raw == "") 24 else suppressWarnings(as.integer(h_raw))
      if (is.na(h_test) || h_test < 1) h_test <- 24
      log_choices$h_test <- h_test
      # tryCatch: re-estimarea pe sub-esantion / naivul sezonier pot esua;
      # structura de fallback contine EXACT campurile folosite in writeLines.
      metric_oos <- tryCatch(
        oos_ardl(Y, dep_ardl, regs_ardl, est_ardl$order, h_test,
                 ordine_int, dir_t, cod_tara),
        error = function(e) {
          cat("[ARDL] OOS esuat:", conditionMessage(e), "\n")
          list(rmse = NA, mae = NA, mape = NA,
               rmse_n = NA, mae_n = NA, mape_n = NA,
               rmse_mean = NA, mae_mean = NA, mape_mean = NA,
               rmse_seas = NA, mae_seas = NA, mape_seas = NA,
               sub_horizon = NULL)
        })

      # ---------- Dinamica pe termen scurt (din RECM) ----------
      sr_txt <- "(indisponibil)"
      if (!is.null(est_ardl$recm)) {
        cf_sr <- tryCatch(summary(est_ardl$recm)$coefficients,
                          error = function(e) NULL)
        if (!is.null(cf_sr)) {
          este_d <- grepl("^d\\(", rownames(cf_sr)) &
            !is.na(cf_sr[, 4]) & cf_sr[, 4] < 0.05
          dl <- rownames(cf_sr)[este_d]
          # fmt este scalar (if(is.na(x))); il aplicam element-cu-element peste
          # toti coeficientii d(.) semnificativi (dl poate avea lungime > 1).
          sr_txt <- if (length(dl))
            paste(sprintf("%s (coef=%s, p=%s)", dl,
                          vapply(cf_sr[dl, 1], fmt, character(1), 4),
                          vapply(cf_sr[dl, 4], fmt, character(1), 4)),
                  collapse = "; ")
          else "niciun termen d(.) semnificativ la 5%"
        }
      }

      # ---------- B.2: Rezumat dedicat ARDL ----------
      ect_est <- if (!is.null(est_ardl$ect)) as.numeric(est_ardl$ect[["Estimate"]])[1] else NA_real_
      ect_p   <- if (!is.null(est_ardl$ect)) as.numeric(est_ardl$ect[["Pr(>|t|)"]])[1] else NA_real_
      ect_stabil <- isTRUE(!is.na(ect_est) && ect_est < 0 && ect_est > -1 &&
                             !is.na(ect_p) && ect_p < 0.05)
      bounds_da <- isTRUE(!is.na(est_ardl$bf_F) && !is.na(est_ardl$bf_i1) &&
                            as.numeric(est_ardl$bf_F)[1] > as.numeric(est_ardl$bf_i1)[1])
      lr_lines <- if (!is.null(est_ardl$multipliers)) {
        m <- est_ardl$multipliers
        unlist(lapply(regs_ardl, function(rg) {
          i <- which(m$Term == rg)[1]
          if (is.na(i)) return(sprintf("     %s: (indisponibil)", rg))
          pv <- as.numeric(m[i, "Pr(>|t|)"])[1]
          sprintf("     %s: coef = %s (%s)%s, p = %s", rg,
                  fmt(m$Estimate[i], 4),
                  if (m$Estimate[i] >= 0) "pozitiv" else "negativ",
                  if (!is.na(pv) && pv < 0.05) " *semnificativ*" else "",
                  fmt(pv, 4))
        }))
      } else "     (indisponibil)"

      cale_rez <- file.path(dir_t, paste0("rezumat_", cod_tara, ".txt"))
      con <- NULL
      tryCatch({
      con <- file(cale_rez, "w", encoding = "UTF-8")
      writeLines(c(
        paste("Rezumat analiza multivariata (ARDL) pentru tara:", cod_tara),
        paste(rep("=", 60), collapse = ""),
        "",
        "0. ALEGERILE UTILIZATORULUI (pentru reproducibilitate)",
        paste("   Ruleaza tara         :", log_choices$ran_country),
        paste("   Tip model ales       :", log_choices$tip_model),
        paste("   ARDL var. dependenta :", log_choices$ardl_dep),
        paste("   ARDL regresori       :", log_choices$ardl_regs),
        paste("   ARDL max_order (lag) :", log_choices$ardl_maxlag),
        paste("   Orizont OOS          :", log_choices$h_test),
        paste("   Clasificare Elec     :", log_choices$ordin_user["Elec"]),
        paste("   Clasificare IPI      :", log_choices$ordin_user["IPI"]),
        paste("   Clasificare TDD      :", log_choices$ordin_user["TDD"]),
        "",
        "AJUSTARE SEZONIERA:",
        sprintf("  Elec: %s | motor: %s | Canova-Hansen p: %s",
                sa_elec$a_fost_ajustata, sa_elec$motor,
                fmt(sa_elec$test_canova_hansen_p)),
        sprintf("  TDD : %s | motor: %s | Canova-Hansen p: %s",
                sa_tdd$a_fost_ajustata, sa_tdd$motor,
                fmt(sa_tdd$test_canova_hansen_p)),
        "  IPI : deja ajustat sezonier (Eurostat SCA)",
        "",
        "1. ORDIN DE INTEGRARE (ADF + KPSS, confirmat de utilizator)",
        paste("   Elec:", ordine_int["Elec"], " IPI:", ordine_int["IPI"],
              " TDD:", ordine_int["TDD"]),
        "   (mix I(0)/I(1) verificat anti-I(2): nicio serie nu este I(2))",
        "",
        sprintf("3. MODEL ESTIMAT: ARDL(%s) | variabila dep: %s",
                paste(est_ardl$order, collapse = ", "), dep_ardl),
        "",
        "4. BOUNDS TEST (case = 3)",
        sprintf("   F-statistic: %s | limite I(0)/I(1): %s / %s | p = %s | decizie: relatie TL %s",
                fmt(est_ardl$bf_F, 4), fmt(est_ardl$bf_i0, 4),
                fmt(est_ardl$bf_i1, 4), fmt(est_ardl$bf_p, 4),
                if (bounds_da) "DA" else "NU/inconcludent"),
        paste("   ->", est_ardl$dec_f),
        sprintf("   t-statistic: %s | limite I(0)/I(1): %s / %s | p = %s",
                fmt(est_ardl$bt_t, 4), fmt(est_ardl$bt_i0, 4),
                fmt(est_ardl$bt_i1, 4), fmt(est_ardl$bt_p, 4)),
        paste("   ->", est_ardl$dec_t),
        "",
        "5. RELATIA DE TERMEN LUNG (multiplicatori LR)",
        lr_lines,
        paste("   ecuatie echilibru:",
              if (!is.na(est_ardl$lr_eq)) est_ardl$lr_eq else "(indisponibil)"),
        "",
        "6. TERMEN DE CORECTIE (ECT din RECM)",
        sprintf("   coeficient ECT: %s (p = %s) %s", fmt(ect_est, 4), fmt(ect_p, 4),
                if (ect_stabil) "(negativ+semnificativ in (-1,0) = ajustare stabila)"
                else "(NU indeplineste conditia de ajustare stabila)"),
        sprintf("   viteza ajustare: %s%% din dezechilibru corectat per perioada",
                if (!is.na(ect_est)) fmt(abs(ect_est) * 100, 1) else "NA"),
        "",
        "7. DINAMICA PE TERMEN SCURT (coeficienti d(.) semnificativi)",
        paste("   ", sr_txt),
        "",
        "8. DIAGNOSTICE",
        sprintf("   Breusch-Godfrey p (lag4/lag8): %s / %s",
                fmt(diag_ardl$bg4), fmt(diag_ardl$bg8)),
        sprintf("   Breusch-Pagan p: %s | Jarque-Bera p: %s | Shapiro p: %s",
                fmt(diag_ardl$bp), fmt(diag_ardl$jb), fmt(diag_ardl$sw)),
        sprintf("   CUSUM stabil: %s (CUSUM p=%s, OLS-CUSUM p=%s)",
                if (isTRUE(cusum_res$stabil)) "DA" else "NU",
                fmt(cusum_res$p_rec), fmt(cusum_res$p_ols)),
        "",
        paste("9. METRICI OUT-OF-SAMPLE (", h_test,
              " luni, regresori inertiali, var. ", dep_ardl, ")", sep = ""),
        sprintf("   Model ARDL    RMSE = %s  MAE = %s  MAPE = %s%%",
                fmt(metric_oos$rmse, 2), fmt(metric_oos$mae, 2),
                fmt(metric_oos$mape, 2)),
        sprintf("   Naiv RW       RMSE = %s  MAE = %s  MAPE = %s%% (potrivit I(1))",
                fmt(metric_oos$rmse_n, 2), fmt(metric_oos$mae_n, 2),
                fmt(metric_oos$mape_n, 2)),
        sprintf("   Naiv MEAN     RMSE = %s  MAE = %s  MAPE = %s%% (potrivit I(0))",
                fmt(metric_oos$rmse_mean, 2), fmt(metric_oos$mae_mean, 2),
                fmt(metric_oos$mape_mean, 2)),
        sprintf("   Naiv SEAS     RMSE = %s  MAE = %s  MAPE = %s%% (luna t-12)",
                fmt(metric_oos$rmse_seas, 2), fmt(metric_oos$mae_seas, 2),
                fmt(metric_oos$mape_seas, 2)),
        "",
        "   Sub-orizonturi ARDL (h = 1, 3, 6, 12, h_test):",
        if (!is.null(metric_oos$sub_horizon))
          paste(capture.output(print(metric_oos$sub_horizon, row.names = FALSE)),
                collapse = "\n")
        else "   (nu s-a putut calcula)",
        "",
        "10. SINTEZA INTERPRETATIVA (ARDL)",
        sprintf("   - Echilibru pe termen lung: %s (Bounds test).",
                if (bounds_da) "EXISTA" else "neconfirmat"),
        sprintf("   - Viteza de ajustare: %s (ECT = %s).",
                if (ect_stabil)
                  sprintf("%s%% pe luna catre echilibru", fmt(abs(ect_est) * 100, 1))
                else "ajustare neclara/nesemnificativa", fmt(ect_est, 4)),
        paste("   - Dinamica pe termen scurt:", sr_txt),
        "   - Limitari: relatii contemporane intre regresori necaptate;",
        "     posibile rupturi structurale (vezi CUSUM); scenariu OOS inertial."
      ), con)
      close(con)
      cat("Rezumat ARDL salvat la:", cale_rez, "\n")
      }, error = function(e) {
        try(if (!is.null(con)) close(con), silent = TRUE)
        writeLines(c(
          paste("Rezumat ARDL (partial) pentru tara:", cod_tara),
          paste("EROARE la scrierea rezumatului complet:", conditionMessage(e))
        ), cale_rez)
        cat("[ARDL] eroare scriere rezumat; scris rezumat partial.\n")
      })
      cat("=== TERMINAT MULTI (ARDL):", cod_tara, "===\n")

      return(list(
        tara = cod_tara,
        tip_model = "ARDL",
        lag_optim = as.integer(est_ardl$order[1]),
        rang_coint = 0,
        g_ipi_to_elec = NA_real_, g_tdd_to_elec = NA_real_,
        g_elec_to_ipi = NA_real_, g_elec_to_tdd = NA_real_,
        g_ipi_to_tdd  = NA_real_, g_tdd_to_ipi  = NA_real_,
        mape_oos  = metric_oos$mape,
        mape_naiv = metric_oos$mape_n,
        serial_p = diag_ardl$bg4,
        arch_p   = diag_ardl$bp,
        norm_p   = diag_ardl$jb))
    }

    # ----------------------------------------------------------------
    # CHECKPOINT 5: model output review
    # Apoi CHECKPOINT 6: diagnostice + retry
    # ----------------------------------------------------------------
    estimare_completa <- function(Y_loc, p_use) {
      est <- estimeaza_model(Y_loc, tip_model, p_use, rang_coint)
      if (is.null(est) || is.null(est$var_obj)) return(NULL)

      cat("\n--- CHECKPOINT 5: Sumar model estimat ---\n")
      cat("Tip:", est$tip_model, "  lag:", p_use,
          "  rang:", est$rang, "\n\n")
      if (est$tip_model == "VECM" && !is.null(est$johansen_obj)) {
        vecm_est <- tryCatch(cajorls(est$johansen_obj, r = est$rang),
                             error = function(e) NULL)
        if (!is.null(vecm_est)) {
          cat(">>> Coeficienti VECM (cajorls):\n")
          print(summary(vecm_est$rlm))
          cat("\n>>> Vector cointegrare beta:\n")
          print(round(vecm_est$beta, 4))
        }
      } else {
        print(summary(est$var_obj))
      }
      ghid_si_pauza(c(
        ">>> Rezultatele estimarii sunt afisate mai sus."
      ))

      diag_p <- ruleaza_diagnostice(est$var_obj, p_use, dir_t, cod_tara)
      ghid_si_pauza(c(
        ">>> p < 0.05 = problema (autocorelare/heteroscedasticitate/non-normalitate)",
        "    Daca diagnosticele sunt slabe, poti reestima cu alt lag."
      ))
      retry_raw <- trimws(citeste_input(
        "Re-estimezi cu alt lag? (y/n) [default n]: "))
      retry <- raspuns_da(retry_raw) && tolower(retry_raw) != ""
      list(est = est, diag = diag_p, retry = retry)
    }

    rez_est <- estimare_completa(Y, lag_optim)
    if (is.null(rez_est)) {
      cat("Estimarea modelului a esuat. Sare tara.\n"); return(NULL)
    }
    log_choices$retry <- FALSE
    if (rez_est$retry) {
      lag_new_raw <- trimws(citeste_input(sprintf(
        "Lag nou (1-12) [curent = %d]: ", lag_optim)))
      lag_new <- suppressWarnings(as.integer(lag_new_raw))
      if (!is.na(lag_new) && lag_new >= 1 && lag_new <= 12) {
        lag_optim <- lag_new
        log_choices$lag <- lag_optim
        log_choices$retry <- TRUE
        rez_est_new <- estimare_completa(Y, lag_optim)
        if (!is.null(rez_est_new)) rez_est <- rez_est_new
      } else {
        cat("Lag invalid, pastram", lag_optim, "\n")
      }
    }

    est <- rez_est$est
    var_obj <- est$var_obj
    tip_model <- est$tip_model
    rang_coint <- est$rang
    vectori_beta <- est$vectori_beta
    alpha_vec    <- est$alpha_vec
    diag_p <- rez_est$diag
    serial_p <- diag_p$serial_p
    arch_p   <- diag_p$arch_p
    norm_p   <- diag_p$norm_p

    if (is.null(var_obj)) {
      cat("Eroare la estimarea modelului. Sare tara.\n"); return(NULL)
    }

    # ----------------------------------------------------------------
    # PARTEA B.1: Stabilitate sistem + adecvare ecuatii (VAR/VECM)
    # ----------------------------------------------------------------
    cat("\n--- B.1: Stabilitate sistem si adecvare (VAR/VECM) ---\n")
    rad_sistem <- radacini_sistem(var_obj, tip_model)
    if (length(rad_sistem$moduli) > 0 && !all(is.na(rad_sistem$moduli))) {
      cat("Module radacini (polinom caracteristic):",
          paste(vapply(sort(rad_sistem$moduli, decreasing = TRUE),
                       fmt, character(1), 4),
                collapse = ", "), "\n")
      max_rad <- max(rad_sistem$moduli, na.rm = TRUE)
      cat(sprintf("Radacina maxima = %s -> sistem %s\n", fmt(max_rad, 4),
                  if (max_rad < 1) "STABIL (toate < 1)" else
                    "cu radacini >= 1 (vezi nota)"))
    } else {
      cat("Module radacini: (nu s-au putut calcula)\n")
    }
    if (!is.null(rad_sistem$nota)) cat(rad_sistem$nota, "\n")

    cor_rez <- tryCatch({
      cr <- cor(residuals(var_obj))
      dimnames(cr) <- list(colnames(Y), colnames(Y))
      cr
    }, error = function(e) NULL)
    if (!is.null(cor_rez)) {
      cat("\nMatricea de corelatie a reziduurilor (justifica ordinea Cholesky):\n")
      print(round(cor_rez, 3))
    }

    adec_ec <- adecvare_ecuatii(var_obj, tip_model, est$johansen_obj, rang_coint)
    if (!is.null(adec_ec)) {
      cat("\nAdecvare per ecuatie (R^2 si nr. coeficienti semnificativi la 5%):\n")
      print(adec_ec, row.names = FALSE)
    }
    ghid_si_pauza(c(
      ">>> Verifica: modulele radacinilor (toate < 1), corelatia reziduurilor",
      "    si R^2 / coeficientii semnificativi per ecuatie."
    ))

    # ----------------------------------------------------------------
    # CHECKPOINT 7: Cholesky ordering pentru IRF/FEVD
    # ----------------------------------------------------------------
    cat("\n--- CHECKPOINT 7: Ordinea Cholesky pentru IRF/FEVD ---\n")
    ordine_curenta <- colnames(Y)
    cat("Ordinea curenta:", paste(ordine_curenta, collapse = ", "), "\n")
    ghid_si_pauza(c(
      ">>> Introdu variabilele de la cea mai exogena la cea mai endogena",
      "    (sugestie: TDD, IPI, Elec)."
    ))
    reord_raw <- trimws(citeste_input(paste0(
      "Reordonare? (virgule, de ex. TDD,IPI,Elec) [ENTER = pastreaza]: ")))
    if (reord_raw != "") {
      cerere <- trimws(strsplit(reord_raw, ",")[[1]])
      if (length(cerere) == 3 && all(cerere %in% ordine_curenta)) {
        Y <- Y[, cerere]
        cat("Reordonat la:", paste(cerere, collapse = ", "), "\n")
        # Re-estimam pe noua ordine
        est2 <- estimeaza_model(Y, tip_model, lag_optim, rang_coint)
        if (!is.null(est2) && !is.null(est2$var_obj)) {
          var_obj <- est2$var_obj
          vectori_beta <- est2$vectori_beta
          alpha_vec    <- est2$alpha_vec
        } else {
          cat("Re-estimare dupa reordonare a esuat; revin la ordinea initiala.\n")
          Y <- cbind(Elec = elec_sa, IPI = ipi_sa, TDD = tdd_sa)
        }
      } else {
        cat("Input invalid (trebuie 3 variabile din",
            paste(ordine_curenta, collapse = "/"),
            "), pastram ordinea curenta.\n")
      }
    }
    log_choices$cholesky_order <- paste(colnames(Y), collapse = ",")

    # ----------------------------------------------------------------
    # CHECKPOINT 8: IRF horizon si runs
    # ----------------------------------------------------------------
    cat("\n--- CHECKPOINT 8: Parametri IRF/FEVD ---\n")
    ghid_si_pauza(c(
      ">>> n.ahead = numarul de luni dupa soc pentru care urmarim raspunsul.",
      "    runs = numarul de replicari bootstrap pentru benzile de incredere.",
      "    Pentru rezultate finale recomandam 500+ replicari (mai stabil)."
    ))
    nh_raw <- trimws(citeste_input("n.ahead [default 24]: "))
    n_ahead <- if (nh_raw == "") 24 else suppressWarnings(as.integer(nh_raw))
    if (is.na(n_ahead) || n_ahead < 1) n_ahead <- 24
    runs_raw <- trimws(citeste_input("runs [default 200; 500+ pt. final]: "))
    irf_runs <- if (runs_raw == "") 200 else suppressWarnings(as.integer(runs_raw))
    if (is.na(irf_runs) || irf_runs < 50) irf_runs <- 200
    log_choices$n_ahead <- n_ahead
    log_choices$runs    <- irf_runs

    irf_obj  <- NULL
    fevd_obj <- NULL

    cat("IRF cu n.ahead =", n_ahead, "si runs =", irf_runs, "...\n")
    # par(ask = FALSE) suprima prompt-ul "Hit <Return>..." al lui plot.varirf
    # pentru graficele multi-panel; afisarea trece prin helperul standard care
    # gestioneaza display interactiv + save PNG (pane sau popup la nevoie).
    tryCatch({
      irf_obj <- irf(var_obj, n.ahead = n_ahead, boot = TRUE,
                     ci = 0.95, runs = irf_runs)
      try(par(ask = FALSE), silent = TRUE)
      afiseaza_si_salveaza_base(
        quote({ par(ask = FALSE); plot(irf_obj) }),
        file.path(dir_t, "01_irf.png"), latime = 12, inaltime = 9)
    }, error = function(e) cat("Eroare IRF:", conditionMessage(e), "\n"))

    tryCatch({
      fevd_obj <- fevd(var_obj, n.ahead = n_ahead)
      try(par(ask = FALSE), silent = TRUE)
      afiseaza_si_salveaza_base(
        quote({ par(ask = FALSE); plot(fevd_obj) }),
        file.path(dir_t, "02_fevd.png"), latime = 12, inaltime = 9)
    }, error = function(e) cat("Eroare FEVD:", conditionMessage(e), "\n"))

    # --- B.1: valori FEVD numerice + sumar IRF (pentru interpretare cantitativa) ---
    orizonturi_fevd <- sort(unique(c(1, 4, 8, 12, n_ahead)))
    fevd_tab <- extrage_fevd_numeric(fevd_obj, orizonturi_fevd)
    if (!is.null(fevd_tab)) {
      cat("\n>>> FEVD numeric (% din varianta erorii de prognoza, pe sursa de soc):\n")
      print(fevd_tab, row.names = FALSE)
    } else cat("\n>>> FEVD numeric: (indisponibil)\n")
    irf_tab <- rezuma_irf(irf_obj)
    if (!is.null(irf_tab)) {
      cat("\n>>> Sumar IRF (semnificativ = banda exclude zero; semn la varf):\n")
      print(irf_tab, row.names = FALSE)
    } else cat(">>> Sumar IRF: (indisponibil)\n")

    # ----------------------------------------------------------------
    # CHECKPOINT 9: Granger interpretation
    # ----------------------------------------------------------------
    cat("\n--- CHECKPOINT 9: Cauzalitate Granger pairwise ---\n")
    Y_used <- if (tip_model == "VAR_DIFF") diff(Y) else Y
    g_ipi_to_elec <- granger_pairwise(Y_used[, "Elec"], Y_used[, "IPI"], lag_optim)
    g_tdd_to_elec <- granger_pairwise(Y_used[, "Elec"], Y_used[, "TDD"], lag_optim)
    g_elec_to_ipi <- granger_pairwise(Y_used[, "IPI"],  Y_used[, "Elec"], lag_optim)
    g_elec_to_tdd <- granger_pairwise(Y_used[, "TDD"],  Y_used[, "Elec"], lag_optim)
    g_ipi_to_tdd  <- granger_pairwise(Y_used[, "TDD"],  Y_used[, "IPI"],  lag_optim)
    g_tdd_to_ipi  <- granger_pairwise(Y_used[, "IPI"],  Y_used[, "TDD"],  lag_optim)

    cat(sprintf("%-20s %-10s\n", "Directie", "p-value"))
    cat(strrep("-", 35), "\n", sep = "")
    cat(sprintf("%-20s %s\n", "IPI  -> Elec",  fmt(g_ipi_to_elec)))
    cat(sprintf("%-20s %s\n", "TDD  -> Elec",  fmt(g_tdd_to_elec)))
    cat(sprintf("%-20s %s\n", "Elec -> IPI",   fmt(g_elec_to_ipi)))
    cat(sprintf("%-20s %s\n", "Elec -> TDD",   fmt(g_elec_to_tdd)))
    cat(sprintf("%-20s %s\n", "IPI  -> TDD",   fmt(g_ipi_to_tdd)))
    cat(sprintf("%-20s %s\n", "TDD  -> IPI",   fmt(g_tdd_to_ipi)))

    sig5 <- c()
    sig10 <- c()
    pairs <- list(
      "IPI->Elec" = g_ipi_to_elec, "TDD->Elec" = g_tdd_to_elec,
      "Elec->IPI" = g_elec_to_ipi, "Elec->TDD" = g_elec_to_tdd,
      "IPI->TDD"  = g_ipi_to_tdd,  "TDD->IPI"  = g_tdd_to_ipi)
    for (nm in names(pairs)) {
      pp <- pairs[[nm]]
      if (!is.na(pp) && pp < 0.05)  sig5  <- c(sig5,  nm)
      if (!is.na(pp) && pp < 0.10 && pp >= 0.05) sig10 <- c(sig10, nm)
    }
    ghid_si_pauza(c(
      sprintf(">>> Semnificative la 5%%: %s",
              if (length(sig5))  paste(sig5,  collapse = ", ") else "(niciuna)"),
      sprintf(">>> Semnificative la 10%% dar nu la 5%%: %s",
              if (length(sig10)) paste(sig10, collapse = ", ") else "(niciuna)")
    ))

    # ----------------------------------------------------------------
    # CHECKPOINT 10: OOS horizon
    # ----------------------------------------------------------------
    cat("\n--- CHECKPOINT 10: Orizont out-of-sample ---\n")
    cat("Un VAR pe diferente prognozeaza util doar 3-6 luni; la orizont\n")
    cat("lung prognoza converge la o constanta.\n")
    h_raw <- trimws(citeste_input(
      "Orizont OOS in luni [default 24]: "))
    h_test <- if (h_raw == "") 24 else suppressWarnings(as.integer(h_raw))
    if (is.na(h_test) || h_test < 1) h_test <- 24
    log_choices$h_test <- h_test

    metric_oos <- list(rmse = NA, mae = NA, mape = NA,
                       rmse_n = NA, mae_n = NA, mape_n = NA,
                       rmse_seas = NA, mae_seas = NA, mape_seas = NA,
                       rmse_mean = NA, mae_mean = NA, mape_mean = NA,
                       sub_horizon = NULL)
    if (n_total <= h_test + 24) {
      cat(sprintf("AVERTISMENT: doar %d obs total, dar OOS %d cere >= %d.\n",
                  n_total, h_test, h_test + 24))
      sk_raw <- trimws(citeste_input(
        "Sari OOS? (y/n) [default y]: "))
      if (sk_raw == "" || raspuns_da(sk_raw)) {
        cat("Sare evaluarea OOS.\n")
      } else {
        cat("Nu putem rula OOS cu atat de putine date.\n")
      }
    } else {
      Y_train <- window(Y, end = time(Y)[n_total - h_test])
      Y_test  <- window(Y, start = time(Y)[n_total - h_test + 1])
      est_train <- estimeaza_model(Y_train, tip_model, lag_optim,
                                   max(rang_coint, 1))
      var_train <- if (!is.null(est_train)) est_train$var_obj else NULL

      if (!is.null(var_train)) {
        pred <- tryCatch(predict(var_train, n.ahead = h_test, ci = 0.95),
                         error = function(e) NULL)
        if (!is.null(pred)) {
          if (tip_model == "VAR_DIFF") {
            ultim_obs_train <- as.numeric(tail(Y_train[, "Elec"], 1))
            delta_pred <- pred$fcst$Elec[, 1]
            fc_elec <- ultim_obs_train + cumsum(delta_pred)
          } else {
            fc_elec <- pred$fcst$Elec[, 1]
          }
          actual_elec <- as.numeric(Y_test[, "Elec"])
          n_min <- min(length(fc_elec), length(actual_elec))
          fc_elec     <- fc_elec[1:n_min]
          actual_elec <- actual_elec[1:n_min]

          # --- Trei benchmark-uri ---
          ultim_train <- as.numeric(tail(Y_train[, "Elec"], 1))
          # naiv_rw: ultima valoare din train (random walk)
          naiv_rw <- rep(ultim_train, length(actual_elec))
          # naiv_mean: media din train repetata
          naiv_mean <- rep(mean(as.numeric(Y_train[, "Elec"]), na.rm = TRUE),
                           length(actual_elec))
          # naiv_seas: aceeasi luna a anului precedent (t - 12)
          all_elec <- as.numeric(Y[, "Elec"])
          idx_start_test <- length(all_elec) - length(actual_elec) + 1
          naiv_seas <- sapply(seq_along(actual_elec), function(i) {
            idx_acum <- idx_start_test + i - 1
            if (idx_acum - 12 >= 1) all_elec[idx_acum - 12] else NA
          })

          calc_metrici <- function(actual, fc) {
            ok <- !is.na(actual) & !is.na(fc)
            if (!any(ok)) return(list(rmse = NA, mae = NA, mape = NA))
            a <- actual[ok]; f <- fc[ok]
            list(rmse = sqrt(mean((a - f)^2)),
                 mae  = mean(abs(a - f)),
                 mape = mean(abs((a - f) / a)) * 100)
          }
          m_var  <- calc_metrici(actual_elec, fc_elec)
          m_rw   <- calc_metrici(actual_elec, naiv_rw)
          m_seas <- calc_metrici(actual_elec, naiv_seas)
          m_mean <- calc_metrici(actual_elec, naiv_mean)

          # --- Metrici defalcate pe sub-orizonturi ---
          orizonturi <- intersect(c(1, 3, 6, 12, h_test), 1:length(actual_elec))
          sub_horizon <- do.call(rbind, lapply(orizonturi, function(h) {
            m_h <- calc_metrici(actual_elec[1:h], fc_elec[1:h])
            data.frame(h = h,
                       RMSE = round(m_h$rmse, 2),
                       MAE  = round(m_h$mae, 2),
                       MAPE = round(m_h$mape, 2),
                       stringsAsFactors = FALSE)
          }))

          cat("\nMetrici VAR pe sub-orizonturi:\n")
          print(sub_horizon, row.names = FALSE)
          cat(sprintf("\nBenchmark-uri (h=%d):\n", length(actual_elec)))
          cat(sprintf("  Model %s  : RMSE=%s MAE=%s MAPE=%s%%\n",
                      tip_model, fmt(m_var$rmse, 2),
                      fmt(m_var$mae, 2), fmt(m_var$mape, 2)))
          cat(sprintf("  Naiv RW    : RMSE=%s MAE=%s MAPE=%s%% (potrivit I(1))\n",
                      fmt(m_rw$rmse, 2),  fmt(m_rw$mae, 2),  fmt(m_rw$mape, 2)))
          cat(sprintf("  Naiv MEAN  : RMSE=%s MAE=%s MAPE=%s%% (potrivit I(0))\n",
                      fmt(m_mean$rmse, 2), fmt(m_mean$mae, 2), fmt(m_mean$mape, 2)))
          cat(sprintf("  Naiv SEAS  : RMSE=%s MAE=%s MAPE=%s%% (luna t-12)\n",
                      fmt(m_seas$rmse, 2), fmt(m_seas$mae, 2), fmt(m_seas$mape, 2)))

          metric_oos <- list(
            rmse = m_var$rmse, mae = m_var$mae, mape = m_var$mape,
            rmse_n = m_rw$rmse, mae_n = m_rw$mae, mape_n = m_rw$mape,
            rmse_seas = m_seas$rmse, mae_seas = m_seas$mae, mape_seas = m_seas$mape,
            rmse_mean = m_mean$rmse, mae_mean = m_mean$mae, mape_mean = m_mean$mape,
            sub_horizon = sub_horizon)

          # Benchmark potrivit pe grafic: I(0) → naiv_mean; I(1) → naiv_rw
          elec_is_i0 <- !is.na(ordine_int["Elec"]) && ordine_int["Elec"] == 0
          naiv_potrivit <- if (elec_is_i0) naiv_mean else naiv_rw
          eticheta_naiv <- if (elec_is_i0) "Naiv MEAN" else "Naiv RW"

          tryCatch({
            df_plot <- data.frame(
              t = seq_along(actual_elec),
              real = actual_elec,
              prognoza = fc_elec,
              naiv_potrivit = naiv_potrivit,
              naiv_seas = naiv_seas)
            g <- ggplot(df_plot, aes(x = t)) +
              geom_line(aes(y = real,          color = "Real"),         linewidth = 1) +
              geom_line(aes(y = prognoza,      color = paste("Prognoza", tip_model)),
                        linewidth = 1) +
              geom_line(aes(y = naiv_potrivit, color = eticheta_naiv),  linewidth = 0.8,
                        linetype = "dashed") +
              geom_line(aes(y = naiv_seas,     color = "Naiv SEAS (t-12)"),
                        linewidth = 0.8, linetype = "dotted") +
              labs(title = paste("Prognoza OOS", cod_tara),
                   x = "Luna in setul de test", y = "GWh", color = NULL) +
              theme_minimal()
            afiseaza_si_salveaza_ggplot(g,
              file.path(dir_t, "03_prognoza_oos.png"))
          }, error = function(e) cat("Eroare grafic OOS:", conditionMessage(e), "\n"))
        }
      }
    }

    # ----------------------------------------------------------------
    # Interpretare automata si rezumat.txt
    # ----------------------------------------------------------------
    interpretare <- character()
    if (!is.na(g_ipi_to_elec) && g_ipi_to_elec < 0.05)
      interpretare <- c(interpretare,
        "IPI Granger-cauzeaza consumul de electricitate.")
    if (!is.na(g_tdd_to_elec) && g_tdd_to_elec < 0.05)
      interpretare <- c(interpretare,
        "TDD (stresul termic) Granger-cauzeaza consumul de electricitate.")
    if (!is.na(g_elec_to_ipi) && g_elec_to_ipi < 0.05)
      interpretare <- c(interpretare,
        "Electricitatea Granger-cauzeaza IPI (efect feedback).")
    if (length(interpretare) == 0)
      interpretare <- "Nu s-au gasit relatii Granger semnificative la 5%."

    cale_rez <- file.path(dir_t, paste0("rezumat_", cod_tara, ".txt"))
    con <- file(cale_rez, "w", encoding = "UTF-8")
    beta_text <- if (!is.null(vectori_beta)) {
      apply(vectori_beta, 2, function(v)
        paste(round(v, 4), collapse = ", "))
    } else "(nu se aplica)"
    alpha_text <- if (!is.null(alpha_vec)) {
      paste(capture.output(print(round(alpha_vec, 4))), collapse = "\n")
    } else "(nu se aplica)"

    # --- PARTEA B.1: pregatire text pentru rezumat (roots, corelatie
    #     reziduuri, adecvare ecuatii, FEVD numeric, sumar IRF) ---
    are_roots <- length(rad_sistem$moduli) > 0 && !all(is.na(rad_sistem$moduli))
    roots_text <- if (are_roots)
      paste(vapply(sort(rad_sistem$moduli, decreasing = TRUE),
                   fmt, character(1), 4), collapse = ", ")
      else "(nu s-au putut calcula)"
    roots_stabil <- if (are_roots) (max(rad_sistem$moduli, na.rm = TRUE) < 1) else NA
    cor_rez_text <- if (!is.null(cor_rez))
      paste(capture.output(print(round(cor_rez, 3))), collapse = "\n")
      else "(indisponibil)"
    adec_text <- if (!is.null(adec_ec))
      paste(capture.output(print(adec_ec, row.names = FALSE)), collapse = "\n")
      else "(indisponibil)"
    fevd_text <- if (!is.null(fevd_tab))
      paste(capture.output(print(fevd_tab, row.names = FALSE)), collapse = "\n")
      else "(indisponibil)"
    irf_text <- if (!is.null(irf_tab))
      paste(capture.output(print(irf_tab, row.names = FALSE)), collapse = "\n")
      else "(indisponibil)"

    # --- PARTEA B.3: sinteza interpretativa ---
    sr_granger_txt <- if (length(interpretare) == 1 &&
                          grepl("^Nu s-au gasit", interpretare[1]))
      "fara relatii Granger semnificative la 5%"
      else paste(interpretare, collapse = " ")
    sinteza_tl <- if (tip_model == "VECM")
      sprintf("cointegrare (rang %d): exista relatie de echilibru pe termen lung; alpha = viteza ajustarii (vezi sectiunea 4).",
              rang_coint)
    else if (tip_model == "VAR_DIFF")
      "VAR pe diferente: fara relatie de termen lung modelata (doar dinamica pe termen scurt)."
    else
      "VAR in niveluri: dinamica include nivelul, fara mecanism explicit de corectie a erorii."

    writeLines(c(
      paste("Rezumat analiza multivariata pentru tara:", cod_tara),
      paste(rep("=", 60), collapse = ""),
      "",
      "0. ALEGERILE UTILIZATORULUI (pentru reproducibilitate)",
      paste("   Ruleaza tara         :", log_choices$ran_country),
      paste("   Lag ales             :", log_choices$lag),
      paste("   Tip model ales       :", log_choices$tip_model),
      paste("   Rang cointegrare     :", log_choices$rang_coint),
      paste("   Ordine Cholesky      :", log_choices$cholesky_order),
      paste("   n.ahead (IRF/FEVD)   :", log_choices$n_ahead),
      paste("   runs (IRF bootstrap) :", log_choices$runs),
      paste("   Orizont OOS          :", log_choices$h_test),
      paste("   Re-estimare cu alt lag:", log_choices$retry),
      paste("   Clasificare Elec (user):", log_choices$ordin_user["Elec"]),
      paste("   Clasificare IPI  (user):", log_choices$ordin_user["IPI"]),
      paste("   Clasificare TDD  (user):", log_choices$ordin_user["TDD"]),
      "",
      "AJUSTARE SEZONIERA:",
      sprintf("  Elec: %s | motor: %s | Canova-Hansen p: %s",
              sa_elec$a_fost_ajustata, sa_elec$motor,
              fmt(sa_elec$test_canova_hansen_p)),
      sprintf("  TDD : %s | motor: %s | Canova-Hansen p: %s",
              sa_tdd$a_fost_ajustata, sa_tdd$motor,
              fmt(sa_tdd$test_canova_hansen_p)),
      "  IPI : deja ajustat sezonier (Eurostat SCA)",
      "",
      "1. ORDIN DE INTEGRARE (ADF + KPSS, confirmat de utilizator)",
      paste("   Elec:", ordine_int["Elec"]),
      paste("   IPI :", ordine_int["IPI"]),
      paste("   TDD :", ordine_int["TDD"]),
      "",
      "2. SELECTIA LAGULUI",
      paste("   Lag AIC:", lag_aic),
      paste("   Lag SC :", lag_sc),
      paste("   Lag folosit:", lag_optim),
      "",
      paste("3. TIP MODEL ESTIMAT:", tip_model),
      paste("   Rang cointegrare:", rang_coint),
      "",
      "4. VECTOR DE COINTEGRARE BETA (daca VECM)",
      paste("   ", beta_text, collapse = "\n"),
      "",
      "   COEFICIENTI ALPHA:",
      paste("   ", strsplit(alpha_text, "\n")[[1]], collapse = "\n   "),
      "",
      "5. DIAGNOSTICE REZIDUURI (p > 0.05 = OK)",
      paste("   Portmanteau (autocorelare) p:", fmt(serial_p)),
      paste("   ARCH (heteroscedasticitate) p:", fmt(arch_p)),
      paste("   Jarque-Bera (normalitate) p:", fmt(norm_p)),
      "",
      "5.1 STABILITATE SISTEM (module radacini polinom caracteristic; toate < 1 = stabil)",
      paste("   Module:", roots_text),
      paste("   Sistem stabil:",
            if (isTRUE(roots_stabil)) "DA (IRF/FEVD valide)"
            else if (isFALSE(roots_stabil)) "NU (radacini >= 1)" else "necalculat"),
      if (!is.null(rad_sistem$nota)) paste("  ", rad_sistem$nota) else NULL,
      "",
      "5.2 MATRICEA DE CORELATIE A REZIDUURILOR (justifica ordinea Cholesky)",
      cor_rez_text,
      "",
      "5.3 ADECVARE PER ECUATIE (R^2 si nr. coeficienti semnificativi la 5%)",
      adec_text,
      "",
      "5.4 FEVD NUMERIC (% din varianta erorii, orizonturi 1/4/8/12/h_max)",
      fevd_text,
      "",
      "5.5 SUMAR IRF (semnificativ = banda exclude zero; semn la varf)",
      irf_text,
      "",
      "6. CAUZALITATE GRANGER PAIRWISE (p < 0.05 = cauzalitate)",
      paste("   IPI  -> Elec :", fmt(g_ipi_to_elec)),
      paste("   TDD  -> Elec :", fmt(g_tdd_to_elec)),
      paste("   Elec -> IPI  :", fmt(g_elec_to_ipi)),
      paste("   Elec -> TDD  :", fmt(g_elec_to_tdd)),
      paste("   IPI  -> TDD  :", fmt(g_ipi_to_tdd)),
      paste("   TDD  -> IPI  :", fmt(g_tdd_to_ipi)),
      "",
      paste("7. METRICI OUT-OF-SAMPLE (", h_test,
            " luni, doar consum Elec)", sep = ""),
      sprintf("   Model %s    RMSE = %s  MAE = %s  MAPE = %s%%",
              tip_model,
              fmt(metric_oos$rmse, 2),
              fmt(metric_oos$mae,  2),
              fmt(metric_oos$mape, 2)),
      sprintf("   Naiv RW       RMSE = %s  MAE = %s  MAPE = %s%% (potrivit I(1))",
              fmt(metric_oos$rmse_n, 2),
              fmt(metric_oos$mae_n,  2),
              fmt(metric_oos$mape_n, 2)),
      sprintf("   Naiv MEAN     RMSE = %s  MAE = %s  MAPE = %s%% (potrivit I(0))",
              fmt(metric_oos$rmse_mean, 2),
              fmt(metric_oos$mae_mean,  2),
              fmt(metric_oos$mape_mean, 2)),
      sprintf("   Naiv SEAS     RMSE = %s  MAE = %s  MAPE = %s%% (luna t-12)",
              fmt(metric_oos$rmse_seas, 2),
              fmt(metric_oos$mae_seas,  2),
              fmt(metric_oos$mape_seas, 2)),
      "",
      "   Sub-orizonturi VAR (h = 1, 3, 6, 12, h_test):",
      if (!is.null(metric_oos$sub_horizon))
        paste(capture.output(print(metric_oos$sub_horizon, row.names = FALSE)),
              collapse = "\n")
      else "   (nu s-a putut calcula)",
      "",
      "8. INTERPRETARE AUTOMATA",
      paste("   ", interpretare, collapse = "\n   "),
      "",
      "9. SINTEZA INTERPRETATIVA",
      paste("   - Termen scurt (Granger):", sr_granger_txt),
      paste("   - Termen lung:", sinteza_tl),
      sprintf("   - Stabilitate sistem: %s.",
              if (isTRUE(roots_stabil)) "toate radacinile < 1 (IRF/FEVD valide)"
              else if (isFALSE(roots_stabil))
                "exista radacini >= 1 (normal pentru VECM; atentie la IRF/FEVD)"
              else "radacini necalculate"),
      "   - Limitari: relatii contemporane captate doar prin ordinea Cholesky;",
      "     posibile rupturi structurale (COVID, criza energetica) necaptate."
    ), con)
    close(con)

    cat("=== TERMINAT MULTI:", cod_tara, "===\n")

    list(tara = cod_tara,
         tip_model = tip_model,
         lag_optim = lag_optim,
         rang_coint = rang_coint,
         g_ipi_to_elec = g_ipi_to_elec,
         g_tdd_to_elec = g_tdd_to_elec,
         g_elec_to_ipi = g_elec_to_ipi,
         g_elec_to_tdd = g_elec_to_tdd,
         g_ipi_to_tdd  = g_ipi_to_tdd,
         g_tdd_to_ipi  = g_tdd_to_ipi,
         mape_oos  = metric_oos$mape,
         mape_naiv = metric_oos$mape_n,
         serial_p = serial_p,
         arch_p   = arch_p,
         norm_p   = norm_p)
  }

  # ------------------------------------------------------------------
  # RESUME multivariata: scaneaza DIR_MULTIVARIATA pentru tari deja
  # procesate (au rezumat_<TARA>.txt) si lasa user-ul sa aleaga.
  # ------------------------------------------------------------------
  tari_existente_multi <- list.dirs(DIR_MULTIVARIATA, full.names = FALSE,
                                    recursive = FALSE)
  tari_existente_multi <- tari_existente_multi[
    tari_existente_multi %in% TARI_ANALIZA &
      sapply(tari_existente_multi, function(t)
        file.exists(file.path(DIR_MULTIVARIATA, t,
                              paste0("rezumat_", t, ".txt"))))
  ]
  tari_multi_de_procesat <- TARI_ANALIZA
  if (length(tari_existente_multi) > 0) {
    cat("\n>>> RELUARE MULTIVARIATA\n")
    cat("Tari deja procesate (au rezumat.txt):\n")
    cat("   ", paste(tari_existente_multi, collapse = ", "), "\n")
    cat("Optiuni:\n")
    cat("  1 = Continua de unde am ramas (sare peste cele procesate) [default]\n")
    cat("  2 = Reia totul de la inceput (suprascrie)\n")
    cat("  3 = Reia de la o tara anume\n")
    alegere_m <- trimws(citeste_input("Alegere [1/2/3] (default 1): "))
    if (alegere_m == "" || alegere_m == "1") {
      tari_multi_de_procesat <- setdiff(TARI_ANALIZA, tari_existente_multi)
    } else if (alegere_m == "3") {
      tara_start_m <- trimws(toupper(citeste_input(
        "Cod tara de la care reluam (ex: RO): ")))
      idx_m <- which(TARI_ANALIZA == tara_start_m)
      if (length(idx_m) > 0) {
        tari_multi_de_procesat <- TARI_ANALIZA[idx_m:length(TARI_ANALIZA)]
      } else {
        cat("Cod tara invalid. Continuam cu toate tarile.\n")
      }
    }
    # alegere == "2" pastreaza intact
  }
  cat("\nMultivariata: vom procesa", length(tari_multi_de_procesat), "tari:\n")
  cat("   ", paste(tari_multi_de_procesat, collapse = ", "), "\n\n")

  rezultate_multivariata <- list()
  erori_multivariata <- character()
  for (tara in tari_multi_de_procesat) {
    rez <- tryCatch(
      analiza_multivariata(tara, PANEL),
      error = function(e) {
        msg <- paste0(tara, ": ", conditionMessage(e))
        cat("EROARE GLOBALA tara", tara, ":", conditionMessage(e), "\n")
        erori_multivariata <<- c(erori_multivariata, msg)
        NULL
      })
    # Skip-all semnal: oprire totala a buclei
    if (!is.null(rez) && isTRUE(rez$skip_all)) {
      cat("Oprire totala a multivariate la cererea utilizatorului.\n")
      break
    }
    if (!is.null(rez)) rezultate_multivariata[[tara]] <- rez
  }
  REZULTATE_MULTIVARIATA <- rezultate_multivariata
  ERORI_MULTIVARIATA     <- erori_multivariata

  cat("\n========================================\n")
  cat(" Multivariata: ", length(REZULTATE_MULTIVARIATA),
      "tari procesate,", length(ERORI_MULTIVARIATA), "erori.\n")
  cat("========================================\n")

} else {
  cat("\nMultivariata sarita la cererea utilizatorului.\n")
}


# ============================================================================
# SECTIUNEA 4  -  TABELE SUMAR FINALE
# ============================================================================

cat("\n========================================\n")
cat(" PARTEA 4: TABELE SUMAR\n")
cat("========================================\n")

if (length(REZULTATE_UNIVARIATA) > 0) {
  tab_uni <- do.call(rbind, lapply(REZULTATE_UNIVARIATA, function(x) {
    data.frame(
      Tara             = x$tara,
      Lambda           = round(x$lambda, 4),
      d                = x$d,
      D                = x$D,
      Model_Manual     = x$model_manual_spec,
      Model_Auto       = x$model_auto_spec,
      Model_ETS        = x$model_ets_spec,
      LjungBox_Manual  = round(x$lb_manual, 4),
      LjungBox_Auto    = round(x$lb_auto,   4),
      LjungBox_ETS     = round(x$lb_ets,    4),
      MAPE_Manual      = round(x$mape_manual, 3),
      MAPE_Auto        = round(x$mape_auto,   3),
      MAPE_ETS         = round(x$mape_ets,    3),
      Castigator             = x$castigator,
      Castigator_Eticheta    = if (is.null(x$castigator_eticheta))
                                 NA else x$castigator_eticheta,
      Spec_Castigator        = if (is.null(x$spec_castigator))
                                 NA else x$spec_castigator,
      MAPE_Castigator        = round(x$mape_castigator, 3),
      stringsAsFactors = FALSE)
  }))
  cale_uni <- file.path(DIR_SUMAR, "tabel_univariata.csv")
  write.csv(tab_uni, cale_uni, row.names = FALSE)
  cat("Tabel univariata salvat la:", cale_uni, "\n")
} else {
  tab_uni <- NULL
  cat("Niciun rezultat univariata de salvat in aceasta sesiune.\n")
}

if (length(REZULTATE_MULTIVARIATA) > 0) {
  tab_multi <- do.call(rbind, lapply(REZULTATE_MULTIVARIATA, function(x) {
    data.frame(
      Tara                 = x$tara,
      Tip_Model            = x$tip_model,
      Lag_Optim            = x$lag_optim,
      Rang_Cointegrare     = x$rang_coint,
      Granger_IPI_to_Elec  = round(x$g_ipi_to_elec, 4),
      Granger_TDD_to_Elec  = round(x$g_tdd_to_elec, 4),
      Granger_Elec_to_IPI  = round(x$g_elec_to_ipi, 4),
      Granger_Elec_to_TDD  = round(x$g_elec_to_tdd, 4),
      Granger_IPI_to_TDD   = round(x$g_ipi_to_tdd,  4),
      Granger_TDD_to_IPI   = round(x$g_tdd_to_ipi,  4),
      MAPE_Out_of_Sample   = round(x$mape_oos,  3),
      MAPE_Naiv            = round(x$mape_naiv, 3),
      Diag_Serial_p        = round(x$serial_p, 4),
      Diag_ARCH_p          = round(x$arch_p,   4),
      Diag_JarqueBera_p    = round(x$norm_p,   4),
      stringsAsFactors = FALSE)
  }))
  cale_multi <- file.path(DIR_SUMAR, "tabel_multivariata.csv")
  write.csv(tab_multi, cale_multi, row.names = FALSE)
  cat("Tabel multivariata salvat la:", cale_multi, "\n")
} else {
  tab_multi <- NULL
  cat("Niciun rezultat multivariata de salvat in aceasta sesiune.\n")
}

if (!is.null(tab_uni) && !is.null(tab_multi)) {
  cmp <- merge(
    tab_uni[, c("Tara", "MAPE_Castigator")],
    tab_multi[, c("Tara", "MAPE_Out_of_Sample")],
    by = "Tara", all = TRUE)
  colnames(cmp) <- c("Tara", "MAPE_Univariata", "MAPE_Multivariata")
  cmp$Castigator <- with(cmp, ifelse(
    is.na(MAPE_Univariata) | is.na(MAPE_Multivariata),
    NA,
    ifelse(MAPE_Univariata < MAPE_Multivariata,
           "Univariata", "Multivariata")))
  cale_cmp <- file.path(DIR_SUMAR, "comparatie_generala.csv")
  write.csv(cmp, cale_cmp, row.names = FALSE)
  cat("Comparatie generala salvata la:", cale_cmp, "\n")
}

cale_raport <- file.path(DIR_SUMAR, "raport_executie.txt")
con <- file(cale_raport, "w", encoding = "UTF-8")
writeLines(c(
  "Raport executie analiza consum electric (sesiune interactiva)",
  paste(rep("=", 60), collapse = ""),
  paste("Data executiei:", Sys.time()),
  "",
  paste("Tari analizate (cu date complete):", length(TARI_ANALIZA)),
  paste("Univariata - succes in sesiune :", length(REZULTATE_UNIVARIATA)),
  paste("Univariata - erori in sesiune  :", length(ERORI_UNIVARIATA)),
  paste("Multivariata - succes in sesiune:", length(REZULTATE_MULTIVARIATA)),
  paste("Multivariata - erori in sesiune :", length(ERORI_MULTIVARIATA)),
  paste("Oprire ceruta in univariata:", oprire_ceruta),
  "",
  "Erori univariata:",
  if (length(ERORI_UNIVARIATA) == 0) "  (niciuna)"
    else paste("  -", ERORI_UNIVARIATA),
  "",
  "Erori multivariata:",
  if (length(ERORI_MULTIVARIATA) == 0) "  (niciuna)"
    else paste("  -", ERORI_MULTIVARIATA)
), con)
close(con)
cat("Raport executie salvat la:", cale_raport, "\n")


# ============================================================================
# SECTIUNEA 5  -  GRAFICE COMPARATIVE INTRE TARI (suprapuse)
# ----------------------------------------------------------------------------
# Pentru fiecare indicator (Elec, IPI, TDD): evolutia in timp a tuturor tarilor
# suprapusa intr-un singur grafic, fiecare tara cu o culoare proprie. Aceeasi
# culoare este folosita pentru aceeasi tara in toate graficele (paleta numita
# refolosita peste tot; RO = rosu). In plus, un grafic cu valorile previzionate
# ale consumului electric (Elec) - DOAR prognoza punctuala, fara intervale de
# incredere - citite din fisierele "prognoza_finala_2ani.csv" salvate de analiza
# univariata (deci valorile corespund exact cu cele salvate).
# Aceasta sectiune este pur aditiva; nu modifica nimic din sectiunile 0-4.
# ============================================================================

cat("\n========================================\n")
cat(" PARTEA 5: GRAFICE COMPARATIVE INTRE TARI\n")
cat("========================================\n")

tryCatch({
  if (!exists("PANEL") || !exists("TARI_ANALIZA"))
    stop("PANEL / TARI_ANALIZA indisponibile (ruleaza sectiunile anterioare).")

  # --- Paleta de culori consistenta per tara (aceeasi tara -> aceeasi culoare
  #     in toate graficele; RO = rosu, conform exemplului cerut). ---
  tari_grafic <- sort(unique(as.character(TARI_ANALIZA)))
  n_tari_g <- length(tari_grafic)
  if (n_tari_g == 0) stop("Nu exista tari in TARI_ANALIZA pentru grafice.")
  culori_baza <- grDevices::hcl.colors(max(n_tari_g, 2), palette = "Dark 3")
  palette_tari <- setNames(culori_baza[seq_len(n_tari_g)], tari_grafic)
  if ("RO" %in% tari_grafic) palette_tari[["RO"]] <- "#E41A1C"  # Romania rosu

  tema_comparativa <- theme_minimal() +
    theme(legend.position = "bottom",
          legend.text  = element_text(size = 13),
          legend.title = element_text(size = 14, face = "bold"),
          legend.key.size = grid::unit(0.9, "cm"),
          plot.title = element_text(face = "bold"))
  ghid_legenda <- guides(color = guide_legend(
    nrow = max(2, ceiling(n_tari_g / 8)), byrow = TRUE,
    override.aes = list(linewidth = 2.5)))

  # Helper local: un grafic suprapus (toate tarile) pentru un indicator din PANEL
  grafic_indicator <- function(coloana, eticheta_y, titlu, nume_fisier) {
    if (!coloana %in% names(PANEL)) {
      cat("Coloana", coloana, "lipseste din PANEL; sar graficul.\n")
      return(invisible(NULL))
    }
    df <- PANEL[, c("geo", "data", coloana)]
    names(df) <- c("geo", "data", "val")
    df <- df[!is.na(df$val) & df$geo %in% tari_grafic, ]
    df$geo <- factor(df$geo, levels = tari_grafic)
    g <- ggplot(df, aes(x = data, y = val, color = geo, group = geo)) +
      geom_line(linewidth = 0.5, na.rm = TRUE) +
      scale_color_manual(values = palette_tari, drop = FALSE) +
      labs(title = titlu, x = "Timp", y = eticheta_y, color = "Tara") +
      tema_comparativa + ghid_legenda
    afiseaza_si_salveaza_ggplot(g, file.path(DIR_SUMAR, nume_fisier),
                                latime = 14, inaltime = 8)
    cat("Salvat:", file.path(DIR_SUMAR, nume_fisier), "\n")
  }

  cat("\n--- Evolutia indicatorilor in timp (toate tarile suprapuse) ---\n")
  grafic_indicator("elec_gwh", "Consum electric (GWh)",
                   "Evolutia consumului electric - toate tarile",
                   "evolutie_Elec_toate_tarile.png")
  grafic_indicator("ipi", "Indice productie industriala (IPI)",
                   "Evolutia productiei industriale - toate tarile",
                   "evolutie_IPI_toate_tarile.png")
  grafic_indicator("tdd", "Grade-zile termice (TDD = HDD + CDD)",
                   "Evolutia gradelor-zile termice - toate tarile",
                   "evolutie_TDD_toate_tarile.png")

  # --- Grafic prognoza Elec: doar valori punctuale, citite din CSV-urile
  #     "prognoza_finala_2ani.csv" salvate de analiza univariata. ---
  cat("\n--- Prognoza consum electric (din prognoza_finala_2ani.csv) ---\n")
  citeste_prognoza_tara <- function(tara) {
    f <- file.path(DIR_UNIVARIATA, tara, "prognoza_finala_2ani.csv")
    if (!file.exists(f)) return(NULL)
    df <- tryCatch(read.csv(f, stringsAsFactors = FALSE),
                   error = function(e) NULL)
    if (is.null(df) || !all(c("Luna", "Prognoza") %in% names(df)))
      return(NULL)
    data.frame(geo = tara,
               Luna = as.Date(as.character(df$Luna)),
               Prognoza = suppressWarnings(as.numeric(df$Prognoza)),
               stringsAsFactors = FALSE)
  }
  df_prognoza <- do.call(rbind, lapply(tari_grafic, citeste_prognoza_tara))

  if (is.null(df_prognoza) || nrow(df_prognoza) == 0) {
    cat("Nu am gasit fisiere prognoza_finala_2ani.csv (ruleaza intai analiza\n")
    cat("univariata). Sar graficul de prognoza.\n")
  } else {
    df_prognoza <- df_prognoza[!is.na(df_prognoza$Prognoza) &
                                 !is.na(df_prognoza$Luna), ]
    tari_cu_prognoza <- sort(unique(df_prognoza$geo))
    df_prognoza$geo <- factor(df_prognoza$geo, levels = tari_grafic)
    g_fc <- ggplot(df_prognoza,
                   aes(x = Luna, y = Prognoza, color = geo, group = geo)) +
      geom_line(linewidth = 0.6, na.rm = TRUE) +
      scale_color_manual(values = palette_tari, drop = TRUE) +
      labs(title = "Prognoza consum electric (2 ani) - toate tarile",
           subtitle = "Valori previzionate punctuale (fara intervale de incredere)",
           x = "Luna previzionata", y = "Consum electric previzionat (GWh)",
           color = "Tara") +
      tema_comparativa + ghid_legenda
    afiseaza_si_salveaza_ggplot(g_fc,
      file.path(DIR_SUMAR, "prognoza_Elec_toate_tarile.png"),
      latime = 14, inaltime = 8)
    cat("Salvat:", file.path(DIR_SUMAR, "prognoza_Elec_toate_tarile.png"), "\n")
    cat("Tari cu prognoza in grafic:", length(tari_cu_prognoza), "->",
        paste(tari_cu_prognoza, collapse = ", "), "\n")
  }
  cat("\nGrafice comparative salvate in:", DIR_SUMAR, "\n")
}, error = function(e) {
  cat("Eroare la generarea graficelor comparative:", conditionMessage(e), "\n")
})

cat("\n========================================\n")
cat(" GATA. Toate iesirile sunt in", DIR_REZULTATE, "\n")
cat("========================================\n")

t_end <- Sys.time()
cat("\n############################################\n")
cat(" Terminat la:", format(t_end), "\n")
cat(" Durata totala:",
    round(as.numeric(difftime(t_end, t_start, units = "mins")), 1),
    "minute.\n")
cat("############################################\n")
