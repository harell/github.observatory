#' @title S3 Sync Module
#' @export
#' @examples
#' ls(S3)
S3 <- new.env()


S3$sync_file <- function(s3, local_file, remote_file){
    local_file_metadata <- fs::file_info(local_file)
    remote_file_metadata <- s3$file_info(remote_file)

    are_files_synced <- identical(local_file_metadata$size, remote_file_metadata$size)
    if(are_files_synced) return(s3)

    is_local_exist <- !is.na(local_file_metadata$modification_time)
    is_local_ahead <- isTRUE(local_file_metadata$modification_time > remote_file_metadata$modification_time)
    is_remote_exist <- !is.na(remote_file_metadata$modification_time)
    is_remote_ahead <- isTRUE(local_file_metadata$modification_time < remote_file_metadata$modification_time)

    if(is_local_ahead | (is_local_exist & !is_remote_exist)){
        s3$file_copy(local_file, dirname(remote_file), overwrite = TRUE)

    } else if (is_remote_ahead | (is_remote_exist & !is_local_exist)) {
        s3$file_copy(remote_file, dirname(local_file), overwrite = TRUE)

    } else {
        stop("invalid option")
    }

    return(s3)
}


S3$sync_dir <- function(s3, local_dir, remote_dir){
    fs::dir_create(local_dir)

    local_files <- fs::dir_ls(local_dir)
    remote_files <- s3$dir_ls(remote_dir)

    files <- dplyr::bind_rows(
        tibble::tibble(local = as.character(local_files)) |> dplyr::mutate(remote = s3$path(remote_dir, basename(local))),
        tibble::tibble(remote = remote_files) |> dplyr::mutate(local = as.character(fs::path(local_dir, basename(remote))))
    ) |> dplyr::distinct()

    for(k in seq_len(nrow(files)))
        S3$sync_file(s3, files[k, "local"][[1]], files[k, "remote"][[1]])

    return(s3)
}
