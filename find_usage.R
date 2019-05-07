
library(tidyverse)

# ------ Script options ---------

script_args <- commandArgs(trailingOnly = TRUE)

target_pkg <- if(is.na(script_args[1])) "ggplot2" else script_args[1]
foreign_pkg <- if(is.na(script_args[2])) "ggplot2" else script_args[2]
output_dir <- if(is.na(script_args[3])) "data" else script_args[3]

if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# best results when using --with-keep.source
# remotes::install_github(
#   "cran/ggspatial", 
#   dependencies = TRUE, 
#   upgrade = "never", 
#   INSTALL_opts = "--with-keep.source"
# )

# ---------- library ish functions ---------

appendable_list <- function(size = 29L) {
  env <- new.env(parent = emptyenv(), size = size)
  class(env) <- "appendable_list"
  env$size <- 0
  env$append <- function(x, name = "") {
    env$size <- env$size + 1
    key <- as.character(env$size)
    env[[paste0("name_", key)]] <- name
    env[[paste0("obj_", key)]] <- x
    env
  }
  
  env
}

as.list.appendable_list <- function(x, ...) {
  size <- x$size
  set_names(
    map(seq_len(size), ~x[[paste0("obj_", .)]]),
    map_chr(seq_len(size), ~x[[paste0("name_", .)]])
  )
}

# l <- appendable_list()
# l$append("I'm a thing")$append("I'm another thing!")
# as.list(l)

add_calls <- function(x, pred, value = identity, lst = appendable_list()) {
  if (is.atomic(x) || is.name(x)) {
    invisible(NULL)
  } else if (is.call(x)) {
    if (isTRUE(pred(x[[1]]))) {
      lst$append(value(x))
    }
    walk(x, add_calls, pred = pred, value = value, lst = lst)
  } else if (is.pairlist(x)) {
    walk(x, add_calls, pred = pred, value = value, lst = lst)
  } else {
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
  
  lst
}

calls_in_function <- function(x, pred, value = identity) {
  as.list(
    add_calls(
      body(x), 
      pred = rlang::as_function(pred), 
      value = rlang::as_function(value)
    )
  )
}

# calls <- add_calls(
#   quote(ggplot2::theme(fish(), line = ggplot2::element_blank())), 
#   function(x) try(identical(eval(x), ggplot2::theme), silent = TRUE)
# )
# as.list(calls)
# calls_in_function(add_calls, ~try(identical(eval(.), walk)))

srcref_as_tibble <- function(x) {
  if(is.null(x)) {
    tibble(
      src_file = NA_character_, 
      src_line_start = NA_integer_,
      src_line_end = NA_integer_
    )
  } else {
    src_file <- attr(x, "srcfile") %||% emptyenv()
    tibble(
      src_file = get0("filename", envir = src_file, inherits = FALSE) %||% NA_character_,
      src_line_start = x[1],
      src_line_end = x[3]
    )
  }
}

namespace_as_tibble <- function(ns) {
  exports <- getNamespaceExports(ns)
  
  tibble(
    object_name = str_subset(names(ns), "__[a-zA-Z0-9]+__", negate = TRUE),
    object = map(object_name, ~ns[[.x]]),
    object_type = map_chr(object, typeof),
    object_class = map(object, class),
    exported = object_name %in% exports,
    index = seq_along(object_name),
    src_ref = map(object, ~srcref_as_tibble(attr(.x, "srcref")))
  ) %>%
    unnest(src_ref) %>%
    mutate(src_file = str_extract(src_file, "R/.*$"))
}

# ------- Load target package ---------

pkg_objects <- namespace_as_tibble(getNamespace("ggplot2")) %>%
  mutate(target_pkg = target_pkg)

match_pkg_object <- function(x) {
  which(map_lgl(pkg_objects$object, identical, x))[1]
}

is_pkg_object <- function(x) {
  !is.na(match_pkg_object(x))
}

match_subclass <- function(cls) {
  which(map_lgl(pkg_objects$object_class, ~any(cls %in% .x[1])))[1]
}

# -------- Find ggplot2 function references --------

foreign_pkg_df <- namespace_as_tibble(loadNamespace(foreign_pkg)) %>%
  mutate(foreign_pkg = !!foreign_pkg) %>%
  select(foreign_pkg, everything())

foreign_function_refs <- foreign_pkg_df %>%
  filter(object_type == "closure") %>%
  mutate(
    references_list = map(
      object, 
      calls_in_function,
      pred = ~try(is_pkg_object(eval(.)), silent = TRUE),
      value = ~tibble(
        index = match_pkg_object(eval(.[[1]])),
        call_text = format(.) %>% paste(collapse = "\n")
      )
    ),
    references_df = map(references_list, bind_rows),
    foreign_object_class = map_chr(object_class, paste, collapse = " / ")
  ) %>%
  select(
    foreign_pkg,
    foreign_object = object_name,
    foreign_object_class = object_class,
    foreign_src_file = src_file, 
    foreign_src_line_start = src_line_start,
    foreign_src_line_end = src_line_end,
    references_df
  ) %>%
  unnest(references_df)

foreign_ggproto_objects <- foreign_pkg_df %>%
  select(
    foreign_pkg,
    foreign_object = object_name, 
    foreign_object_class = object_class,
    foreign_src_file = src_file, 
    foreign_src_line_start = src_line_start,
    foreign_src_line_end = src_line_end,
  ) %>%
  filter(map_lgl(foreign_object_class, ~"ggproto" %in% .x)) %>%
  mutate(
    index = map_int(foreign_object_class, match_subclass),
    foreign_object_class = map_chr(foreign_object_class, paste, collapse = " / ")
  )

# ---------- write output -------------

pkg_objs <- pkg_objects %>%
  mutate(object_class = map_chr(object_class, paste, collapse = " / ")) %>%
  select(target_pkg, object_name, index, object_class, src_file, src_line_start, src_line_end) %>%
  write_csv(file.path(output_dir, paste0(target_pkg, "__objects.csv")))

function_refs <- pkg_objects %>%
  select(target_pkg, object_name, index) %>%
  inner_join(foreign_function_refs, by = "index") %>%
  write_csv(file.path(output_dir, paste0(target_pkg, "-", foreign_pkg, "-function_refs.csv")))

ggproto_refs <- pkg_objects %>%
  select(object_name, index) %>%
  inner_join(foreign_ggproto_objects, by = "index") %>%
  write_csv(file.path(output_dir, paste0(target_pkg, "-", foreign_pkg, "-ggproto_refs.csv")))
