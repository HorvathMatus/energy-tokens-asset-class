### Functions for reading data


parse_date_any <- function(v) {
  # If already Date
  if (inherits(v, "Date")) return(v)
  
  # If POSIXct/POSIXt
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) return(as.Date(v))
  
  # Otherwise treat as character
  s <- as.character(v)
  s <- str_remove(s, "\\s*UTC$")  # remove trailing " UTC" if present
  
  # Try multiple formats robustly
  dt <- suppressWarnings(parse_date_time(
    s,
    orders = c("Ymd HMS", "Ymd HM", "Ymd", "Y-m-d H:M:S", "Y-m-d H:M", "Y-m-d"),
    tz = "UTC"
  ))
  
  as.Date(dt)
}

parse_datetime_any <- function(v, tz = "UTC") {
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) return(v)
  s <- as.character(v)
  s <- str_remove(s, "\\s*UTC$") # if some files have trailing "UTC"
  suppressWarnings(parse_date_time(
    s,
    orders = c("Ymd HMS", "Ymd HM", "Ymd", "Y-m-d H:M:S", "Y-m-d H:M", "Y-m-d"),
    tz = tz
  ))
}

read_coingecko <- function(f) {
  x <- read_csv(f, show_col_types = FALSE)
  
  # Allow "date" / "DATE" etc.
  date_name <- names(x)[tolower(names(x)) == "date"][1]
  if (is.na(date_name)) stop("No 'Date' column in: ", f)
  
  if (date_name != "Date") x <- x %>% rename(Date = all_of(date_name))
  
  x <- x %>%
    mutate(Date = parse_date_any(Date)) %>%
    distinct(Date, .keep_all = TRUE)
  
  # Optional: if exactly 2 cols (Date + one series), rename series to filename stem
  if (ncol(x) == 2) {
    series_col <- setdiff(names(x), "Date")[1]
    stem <- tools::file_path_sans_ext(basename(f))
    if (series_col != stem) {
      x <- x %>% rename(!!stem := all_of(series_col))
    }
  }
  
  x
}

read_firstrate <- function(f, tz = "UTC") {
  # Detect header: if first line contains letters (DateTime/Open/Close), treat as header
  first_line <- readLines(f, n = 1, warn = FALSE)
  has_header <- str_detect(first_line, "[A-Za-z]")
  
  x <- read_csv(
    f,
    col_names = has_header,
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  )
  
  # If no header, assign standard names based on column count
  if (!has_header) {
    if (ncol(x) == 7) {
      names(x) <- c("DateTime", "Open", "High", "Low", "Close", "Volume", "OpenInterest")
    } else if (ncol(x) == 6) {
      names(x) <- c("DateTime", "Open", "High", "Low", "Close", "Volume")
    } else {
      stop("Unexpected number of columns (", ncol(x), ") in file: ", f)
    }
  } else {
    # If header exists, standardize names a bit and locate datetime/close columns
    names(x) <- names(x) %>%
      str_trim() %>%
      str_replace_all("\\s+", "_") %>%
      str_replace_all("[^A-Za-z0-9_]", "") %>%
      str_to_lower()
    
    dt_col <- names(x)[str_detect(names(x), "datetime|date_time|^date$|^datetime")][1]
    close_col <- names(x)[str_detect(names(x), "close")][1]
    
    if (is.na(dt_col)) stop("Couldn't find DateTime column in: ", f)
    if (is.na(close_col)) stop("Couldn't find Close column in: ", f)
    
    x <- x %>% rename(DateTime = all_of(dt_col), Close = all_of(close_col))
  }
  
  # Parse DateTime and keep only DateTime + Close
  x <- x %>%
    mutate(DateTime = parse_datetime_any(DateTime, tz = tz)) %>%
    filter(!is.na(DateTime)) %>%
    select(DateTime, Close) %>%
    distinct(DateTime, .keep_all = TRUE)
  
  # Rename Close -> filename stem
  stem <- tools::file_path_sans_ext(basename(f))
  x %>% rename(!!stem := Close)
}