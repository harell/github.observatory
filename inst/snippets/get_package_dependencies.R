pkgs <- tools::package_dependencies(
    c("R6", "shiny", "R6P"),
    which =  c("Depends", "Imports"),
    recursive = FALSE
)
