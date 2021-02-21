#' Class to provide logging capabilities
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords logger
#' @return Object of \code{\link{R6Class}} with methods for logging events to a log file.
#' @format \code{\link{R6Class}} object.
#' @examples
#' my_log <- logger$new("log1.log")
#' my_log$info("This is an information")
#' @field file stores the file path
#' @field custom_sanitizer stores the customer sanitizer list which cleanes logging strings for ndjson format.
#' @field default_sanitizer stores the default sanitizer list which cleanes logging strings for ndjson format.
#' #' @section Methods:
#' \describe{
#'   \item{\code{new(file)}}{This method is used to create object of this class with \code{file} as path of the logging object it writes to.}
#'
#'   \item{\code{add(log_lvl, log_msg, ..., echo = T)}}{This method adds a logging entry to the logfile with wichever \code{log_lvl} defined. \code{...} can be filled with custom named fields to log}
#'   \item{\code{warn(log_msg, ..., echo = T)}}{This method adds a logging entry to the logfile with "warn" as defined \code{log_lvl}.}
#'   \item{\code{info(log_msg, ..., echo = T)}}{This method adds a logging entry to the logfile with "info" as defined \code{log_lvl}.}
#'   \item{\code{err(log_msg, ..., echo = T)}}{This method adds a logging entry to the logfile with "err" as defined \code{log_lvl}.}
#'   \item{\code{return_log()}}{This method returns the log in the form of a tibble.}
#'   \item{\code{file(value)}}{This method sets a new path for the logging file.}
#'   \item{\code{custom_sanitizer(value, add = T)}}{This method adds or replaces custom sanitizer fields.}
#'   
logger <- R6::R6Class("logger", 
  public = list(
    initialize = function(file) {
      stopifnot(is.character(file), length(file)==1)
      private$.file <- file
    },
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
    warn = function(log_msg, ..., echo = T) {
      #add a logging entry of type 'warn'
      self$add("warn", log_msg, ..., echo = echo)
    },
    info = function(log_msg, ..., echo = T) {
      #add a logging entry of type 'info'
      self$add("info", log_msg, ..., echo = echo)
    },
    err = function(log_msg, ..., echo = T) {
      #add a logging entry of type 'err'
      self$add("err", log_msg, ..., echo = echo)
    },
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


