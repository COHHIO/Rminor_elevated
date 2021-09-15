accessor_create <- function(.x) rlang::new_function(args = 
                      rlang::pairlist2(
                        path = rlang::expr(!!.x),
                        dropbox_folder = file.path("RminorElevated"),
                        ... = ,
                      ),
                    body = base::quote({
                      if (file.info(path)$mtime < lubridate::floor_date(Sys.time(), "day"))
                        rdrop2::drop_download(file.path(dropbox_folder, basename(path)), local_path = path)
                      UU::file_fn(path)(path, ...)
                      
                    }))

create_accessors <- function(path = "data") {
  files <- UU::list.files2(path)
  if (any(purrr::map_lgl(files, ~file.info(.x)$mtime < lubridate::floor_date(Sys.time(), "day")))) {
    db_files <- rdrop2::drop_dir("RminorElevated") |> 
      dplyr::mutate(client_modified = suppressMessages(lubridate::as_datetime(client_modified, tz = Sys.timezone())),
                    file_time = file.info(file.path(path, name))$mtime,
                    needs_update = file_time < client_modified) 
    
    files_to_download <- dplyr::filter(db_files, !name %in% basename(files) | needs_update)
    
    if (nrow(files_to_download)) {
      if (!dir.exists(path))
        UU::mkpath(path)
      slider::slide(files_to_download, ~rdrop2::drop_download(.x$path_display, file.path(path, .x$name), overwrite = TRUE))
    }
    files <- UU::list.files2(path)
  }
  purrr::map(files, accessor_create)
}

#' @title Authorize Dropbox
#'
#' @param db_token \code{(character)} path to Dropbox token saved as RDS
#' @export
db_auth <- function(db_token = file.path("inst","auth","db_token.rds")) {
  if (!file.exists(db_token)) {
    token <- rdrop2::drop_auth(key = Sys.getenv("db_key"), secret = Sys.getenv("db_secret"), cache = FALSE)
    if (!dir.exists(dirname(db_token)))
      UU::mkpath(dirname(db_token))
    saveRDS(token, db_token)
  } else {
    rdrop2::drop_auth(rdstoken = db_token)
  }
  
}