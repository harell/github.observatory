# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()
if(does_not_exist("gdrive_repo")) gdrive_repo <- GDrive$new()


# Load data ---------------------------------------------------------------
USER <- gdrive_repo$read_USER()


# Update users -------------------------------------------------------------



# Teardown ----------------------------------------------------------------
gdrive_repo$overwrite_USER(tidy_users)
