Logger <- R6::R6Class("Logger", 
  public = list(
    initialize = function(file) {
      stopifnot(is.character(file), length(file)==1)
      private$.file <- file
    },
    add = function(log_lvl, log_msg, ..., echo = T) {
      dots <- list(...)
      if (any(unlist(lapply(dots, length)) > 1)) 
        base::warning("Each custom log field should be of length one, or else the log entry will be shortened to max length of 400 characters")
      if (length(log_msg) > 1)
        log_msg <- lapply(log_msg, private$sanitize)
      dots <- lapply(dots, private$sanitize)
      log_df <- data.frame(timestamp = Sys.time(), log_lvl = log_lvl, log_msg = as.character(log_msg), dots, stringsAsFactors = F)
      private$.stream_out(log_df, echo = echo)
    },
    warn = function(log_msg, ..., echo = T) {
      self$add("warn", log_msg, ..., echo = echo)
    },
    info = function(log_msg, ..., echo = T) {
      self$add("info", log_msg, ..., echo = echo)
    },
    err = function(log_msg, ..., echo = T) {
      self$add("err", log_msg, ..., echo = echo)
    },
    return_log =  function() {
      private$.stream_in()
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
    sanitize = function(text, desanitize = F) {
      
      if(is.list(text)) {
        if(length(text)>1) {
          text <- lapply(sanitize)
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
      sanitizer <- if(is.null(private$.cst_sanitizer)) private$.sanitizer else private$.cst_sanitizer
      for(k in names(sanitizer)) {
        if (!desanitize) {
          string <- gsub(k, sanitizer[k], string)
        }
        else {
          string <- gsub(sanitizer[k], k, string)
        }
      }
      if(is.list(text) && length(text)==1) {
        text[[1]] <- string
      } else { 
        text <- string
      }
      
      return(text)
    },
    .stream_out = function(df, echo = T) {
      val <- jsonlite:::asJSON(df, collapse = F)
      write(val, file = private$.file, append = T)
      if(echo) cat(val)
    },
    .stream_in = function() {
      val_in <- readLines(private$.file)
      val_in <- lapply(val_in, jsonlite::fromJSON)
      dplyr::bind_rows(val_in)
    }
  ),
  active = list(
    file = function(value) {
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
      if(missing(value)) {
        private$.sanitizer
      } else {
        stop("default_sanitizer is read only!", call. = F)
      }
    }
  ),
)


