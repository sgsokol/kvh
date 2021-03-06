# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Parse file in KVH format
#'
#' Returns a list with names formed form kvh keys and values formed from kvh values
#' If a kvh value has sub-keys, it is returned as a nested list. Otherwise it is
#' returned as a character string.
#'
#' @param fn character kvh file name.
#' @param comment_str character optional comment string (default empty ""). If non empty, the comment
#'   string itself and everything following it on the line is ignored. Note that
#'   lines are first appended if end lines are escaped and then a search for a
#'   comment string is done.
#' @param strip_white logical optional control of white spaces on both ends of keys and values (default FALSE)
#' @param skip_blank logical optional control of lines composed of only white characters after a possible stripping of a comment (default FALSE)
#' @param split_str character optional string by which a value string can be splitted in several strings (default: empty string, i.e. no splitting)
#' @param follow_url logical optional control of recursive kvh reading and parsing. If set to TRUE and a value starts with 'file://' then the path following this prefix will be passed as argument 'fn' to another 'kvh_read()' call. The list returned by this last call will be affected to the corresponding key instead of the value 'file://...'. If a circular reference to some file is detected, a warning is emmited and the faulty value 'file://...' will be left without change. The rest of the file is proceeded as usual. If a path is relative one (i.e. not strating with `/` neither 'C:/' or alike on windows paltform) then its meant relative to the location of the parent kvh file, not the current working directory.
#' @export
kvh_read <- function(fn, comment_str = "", strip_white = FALSE, skip_blank = FALSE, split_str = "", follow_url = FALSE) {
    .Call(`_kvh_kvh_read`, fn, comment_str, strip_white, skip_blank, split_str, follow_url)
}

