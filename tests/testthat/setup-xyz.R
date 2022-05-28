print(bannerCommenter::banner("Unit Tests", numLines = 2, maxChar = 40))

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


