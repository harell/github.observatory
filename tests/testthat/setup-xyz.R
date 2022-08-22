cli::cli_h1("Unit Tests")

local_path <- fs::path_temp("testthat")
remote_path <- "s3://tidylab/github.observatory/testthat/"

user_id <- 5642464
user_login <- "nz-stefan"
repo_id <- 340564735
repo_login <- "tidylab/microservices"


# Mock Data ---------------------------------------------------------------
query_data <- list(
    id = 280924484,
    name = "clintools",
    owner = list(login = "lilleoel", id = 68481897),
    stargazers_count = 1
)

PACKAGE <- tibble::tribble(
    ~package,   ~full_name,
    "A3",       "cran/A3",
    "aaSEA",    "cran/aaSEA",
    "AATtools", "spiritspeak/AATtools"
)

REPO <- tibble::tribble(
    ~package,   ~id,
    "A3",       236595665,
    "aaSEA",    82898383,
    "AATtools", 189174085
)

USER <- tibble::tribble(
    ~login,       ~id,
    "hadley",     4196,
    "jjallaire",  104391,
    "jcheng5",    129551,
    "harell",     7226303,
    "juliasilge", 12505835
)

FOLLOWING <- tibble::tribble(
    ~from,   ~to,
    7226303, 4196
)

TASK_VIEW <- tibble::tribble(
    ~package,    ~task_view,
    "A3",        "Spatial",
    "A3",        "SpatioTemporal",
    "aaSEA",     "SpatioTemporal",
    "AATtools",  "Spatial",
    "ADGofTest", "Tracking"
)
