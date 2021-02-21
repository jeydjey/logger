#' Class to provide logging capabilities
#'
#'   
logger <- R6::R6Class("logger", 
  public = list(
    #' @description initialize a new logger with \code{logger$new("file_path")}
    #' @param file file path to log file that already exists or should be created
    #' @examples 
    #' ##create a new log
    #' log1 <- logger$new("log1.log")
    initialize = function(file) {
      stopifnot(is.character(file), length(file)==1)
      private$.file <- file
    },
    #' @description add a log entry to the logging file
    #' @param log_lvl character string for the log level to record the log entry for
    #' @param log_msg log message to record
    #' @param ... custom named values to record in the log
    #' @param echo Overriding echo = T argument
    #' @examples
    #' ## add a log record
    #' log1$add(log_lvl = "debug", log_msg = "some information")
    add = function(log_lvl, log_msg, ..., echo = T) {
      #add a logging entry with sanitized character strings for ndjson-format
      dots <- list(...)
      if (any(unlist(lapply(dots, length)) > 1)) 
        base::warning("Each custom log field should be of length one, or else the log entry will be shortened to max length of 400 characters")
      if (length(log_msg) > 1)
        log_msg <- lapply(log_msg, private$.sanitize)
      dots <- lapply(dots, private$.sanitize)
      log_df <- data.frame(timestamp = Sys.time(), log_lvl = log_lvl, log_msg = as.character(log_msg), dots, stringsAsFactors = F)
      private$.stream_out(log_df, echo = echo)
    },
    #' @description add a log entry with "warn" as the \code{log_lvl}
    #' @param log_msg log message to record
    #' @param ... custom named values to record in the log
    #' @param echo Overriding echo = T argument
    warn = function(log_msg, ..., echo = T) {
      #add a logging entry of type 'warn'
      self$add("warn", log_msg, ..., echo = echo)
    },
    #' @description add a log entry with "info" as the \code{log_lvl}
    #' @param log_msg log message to record
    #' @param ... custom named values to record in the log
    #' @param echo Overriding echo = T argument
    info = function(log_msg, ..., echo = T) {
      #add a logging entry of type 'info'
      self$add("info", log_msg, ..., echo = echo)
    },
    #' @description add a log entry with "err" as the \code{log_lvl}
    #' @param log_msg log message to record
    #' @param ... custom named values to record in the log
    #' @param echo Overriding echo = T argument
    err = function(log_msg, ..., echo = T) {
      #add a logging entry of type 'err'
      self$add("err", log_msg, ..., echo = echo)
    },
    #' @description return the log as a tibble
    return_log =  function() {
      #returns the written log desanitized and as a tibble
      dplyr::mutate(private$.stream_in(),
                    dplyr::across(where(is.character), .fns = ~private$.sanitize_(.x, T)))
    }
  ),
  private = list(
    .file = NULL,
    .con = NULL,
    .sanitizer = list(`\\{` = "__LEFTBRACE__", 
                      `\\}` = "__RIGHTBRACE__", 
                      `"` = "__DBLQUOTE__", 
                      `'` = "__SLQUOTE__",
                      `,` = "__COMMA__", 
                      `\r` = "__CR__", 
                      `\n` = "__LF__"),
    .cst_sanitizer = NULL,
    .sanitize = function(text) {
      #handling of string sanitation for entries with length >1
      if(is.list(text)) {
        if(length(text)>1) {
          text <- lapply(private$.sanitize)
          return(text)
        } else {
          string <- text[[1]]
        } 
      } else { 
        string <- text
      }
      
      if(is.numeric(string))
        return(text)
      
      string <- as.character(string)
      string <- paste(string, collapse = "\n")
      if(stringi::stri_length(string)>400) 
        string <- stringi::stri_sub(string, to = 400)
      string <- private$.sanitize_(string, F)
      if(is.list(text) && length(text)==1) {
        text[[1]] <- string
      } else { 
        text <- string
      }
      
      return(text)
    },
    .sanitize_ = function(string, desanitize = F) {
      #actual sanitizer/desanitizer method for strings
      sanitizer <- if(is.null(private$.cst_sanitizer)) private$.sanitizer else private$.cst_sanitizer
      for(k in names(sanitizer)) {
        if (!desanitize) {
          string <- gsub(k, sanitizer[k], string)
        }
        else {
          string <- gsub(sanitizer[k], k, string)
        }
      }
      string
    },
    .stream_out = function(df, echo = T) {
      #write to file
      val <- jsonlite:::asJSON(df, collapse = F)
      write(val, file = private$.file, append = T)
      if(echo) cat(val)
    },
    .stream_in = function() {
      #read in file
      val_in <- readLines(private$.file)
      val_in <- lapply(val_in, jsonlite::fromJSON)
      dplyr::bind_rows(val_in)
    }
  ),
  active = list(
    #' @field file Set or get the file path of the logger
    file = function(value) {
      #return path or set new path
      if(missing(value)) {
        private$.file
      } else {
        stopifnot(is.character(value), length(value)==1)
        old.file <- private$.file
        private$.file <- value
        print(paste0("Location of logger file changed from '", old.file, "' to '", value, "'."))
      }
    },
    #' @field custom_sanitizer get, replace or add custom sanitizer rules to the default sanitizer
    custom_sanitizer = function(value, add = T) {
      #add or replace custom sanitizer entries to the default sanitizer fields
      if(missing(value)) {
        private$.cst_sanitizer
      } else {
        if(is.null(private$.cst_sanitizer) || !add) {
          private$.cst_sanitizer <- modifyList(private$.sanitizer, value)
        } else {
          private$.cst_sanitizer <- modifyList(private$.cst_sanitizer, value)
        }
      }
    },
    #' @field default_sanitizer get the default sanitizer rules
    default_sanitizer = function(value) {
      #return the default sanitizer
      if(missing(value)) {
        private$.sanitizer
      } else {
        stop("default_sanitizer is read only!", call. = F)
      }
    }
  ),
)


