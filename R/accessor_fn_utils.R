accessor = function(x = as.character(match.call()[[1]]),
                    path = "data/db",
                    ...) {
  .file <-
    list.files(path,
               pattern = paste0("^", x, "\\."),
               full.names = TRUE)
  if (length(.file) > 1)
    rlang::warn(paste0(length(.file), " files found. Loading the first."))
  
  UU::file_fn(.file[[1]])(.file[[1]])
}

create_accessors <- function(path = "data") {
  files <- list.files(path, full.names = TRUE) |> 
    {\(x) {rlang::set_names(x, stringr::str_extract(basename(x), ".*(?=\\.)"))}}()
  purrr::map(files, ~rlang::new_function(args = 
                                           rlang::pairlist2(
                                             path = .x,
                                             ... = ,
                                           ),
                                         body = base::quote({
                                           UU::file_fn(path)(path, ...)
                                         })))
}