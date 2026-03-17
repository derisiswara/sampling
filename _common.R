# ============================================
# _common.R — Shared Setup & Functions
# ============================================

# --- Packages ---
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(survey)
  library(srvyr)
  library(sampling)
  library(gt)
  library(scales)
})

# Ensure matplotlib uses a non-interactive backend when Python chunks are rendered
Sys.setenv(MPLBACKEND = "Agg")

# --- ggplot2 Theme ---
theme_sampling <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(color = "grey40", size = 11),
    plot.caption = element_text(color = "grey50", size = 9, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    strip.text = element_text(face = "bold")
  )

theme_set(theme_sampling)

# --- Color Palette ---
sampling_colors <- c(
  "SRS"        = "#2563eb",
  "Stratified" = "#059669",
  "Cluster"    = "#d97706",
  "Systematic" = "#7c3aed",
  "PPS"        = "#dc2626",
  "Urban"      = "#2563eb",
  "Rural"      = "#059669"
)

# --- Generate Population Data ---
generate_population <- function(seed = 2024, N = 10000) {
  set.seed(seed)

  provinces <- paste0("Prov_", sprintf("%02d", 1:10))
  districts_per_prov <- 5
  districts <- paste0(
    rep(provinces, each = districts_per_prov),
    "_Kab_", sprintf("%02d", rep(1:districts_per_prov, times = 10))
  )

  pop <- tibble(
    household_id = 1:N,
    province     = sample(provinces, N, replace = TRUE,
                          prob = c(0.18, 0.14, 0.12, 0.11, 0.10,
                                   0.09, 0.08, 0.07, 0.06, 0.05)),
    district     = paste0(province, "_Kab_",
                          sprintf("%02d", sample(1:districts_per_prov, N, replace = TRUE))),
    urban_rural  = ifelse(runif(N) < ifelse(
      province %in% c("Prov_01", "Prov_02", "Prov_03"), 0.65, 0.40
    ), "Urban", "Rural"),
    region       = case_when(
      province %in% c("Prov_01", "Prov_02", "Prov_03") ~ "Jawa",
      province %in% c("Prov_04", "Prov_05")             ~ "Sumatera",
      province %in% c("Prov_06", "Prov_07")             ~ "Kalimantan",
      province %in% c("Prov_08", "Prov_09")             ~ "Sulawesi",
      TRUE                                               ~ "Indonesia Timur"
    ),
    household_size = pmin(pmax(rpois(N, lambda = ifelse(
      province %in% c("Prov_01", "Prov_02"), 3.2, 4.1
    )), 1), 12)
  )

  # Income (log-normal, varies by urban/rural & region)
  base_income <- ifelse(pop$urban_rural == "Urban", 11.5, 10.8)
  region_adj  <- case_when(
    pop$region == "Jawa"            ~  0.2,
    pop$region == "Kalimantan"      ~  0.15,
    pop$region == "Sumatera"        ~  0.1,
    pop$region == "Sulawesi"        ~ -0.05,
    TRUE                            ~ -0.15
  )
  pop$income <- round(exp(rnorm(N, mean = base_income + region_adj, sd = 0.7)))

  # Expenditure (correlated with income)
  pop$expenditure <- round(pop$income * runif(N, 0.55, 0.90) +
                             rnorm(N, 0, pop$income * 0.05))

  # Education of household head
  pop$education <- sample(
    c("SD", "SMP", "SMA", "D3", "S1", "S2+"),
    N, replace = TRUE,
    prob = c(0.15, 0.15, 0.35, 0.10, 0.20, 0.05)
  )

  # Satisfaction score (1-5, correlated with income quartile)
  inc_q <- ntile(pop$income, 4)
  pop$satisfaction <- pmin(pmax(round(
    rnorm(N, mean = 2.5 + inc_q * 0.3, sd = 0.8)
  ), 1), 5)

  # Internet access
  pop$has_internet <- rbinom(N, 1, prob = ifelse(
    pop$urban_rural == "Urban", 0.78, 0.45
  ))

  # Number of children
  pop$num_children <- pmin(rpois(N, lambda = pop$household_size * 0.3), pop$household_size - 1)

  pop
}

# --- Load or Generate Population ---
load_population <- function(data_dir = "data") {
  csv_path <- file.path(data_dir, "population.csv")
  if (file.exists(csv_path)) {
    readr::read_csv(csv_path, show_col_types = FALSE)
  } else {
    pop <- generate_population()
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
    readr::write_csv(pop, csv_path)
    pop
  }
}

# --- Helper: Sampling Summary Table ---
sampling_summary <- function(estimates, true_value, method_name = "Method") {
  tibble(
    Method    = method_name,
    Mean_Est  = mean(estimates),
    Bias      = mean(estimates) - true_value,
    Variance  = var(estimates),
    MSE       = mean((estimates - true_value)^2),
    RMSE      = sqrt(mean((estimates - true_value)^2)),
    CI_Coverage = NA_real_
  )
}

# --- Helper: Simulation Runner ---
run_simulation <- function(pop_data, sample_fn, B = 1000, seed = 42) {
  set.seed(seed)
  replicate(B, sample_fn(pop_data))
}
