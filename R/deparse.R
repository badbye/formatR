#' Deparse Code
#'
#' \code{\link{deparse2}} only parse a single code block, while
#' \code{\link{deparse0}} can parse multiple code blocks.
#' More specificly, given a chunk of code, \code{\link{deparse0}} split it into
#' pieces(This feature is using the \code{\link{highr:::group_rc}} function). In which each
#' piece of code is a code block.
#' Then it calls \code{\link{deparse2}} to format all the pieces.
#'
#' @param exp A single expression
#' @param max.width The maxmium width. Will try to make the length of each line
#' less than the max.width. It will throw a waring once one line is longer.
#' @param force.cut Remove the indent to make the lines(that are longer than
#' max.width) shorter.
#' @param default.indent Number of spaces in the default indent setting
#' @return A string of format R code
#' @examples
#' x = 'old.priors <- db.query(paste("select prior_id from pfts_priors where pft_id =", parent.pft.id, ";"), con = con)'
#' deparse2(x)
#' @export
deparse2 <- function(exp,
                     max.width = getOption('width', 80),
                     force.cut = getOption('force.cut', FALSE),
                     default.indent = getOption('default.indent', 2), ...){
  chars = strsplit(trimws(exp), '')[[1]]
  positions = find_cutoff(chars)
  cutoffs = positions$index
  indents = positions$indent
  lines = c(); next_indent = ''
  while(any(cutoffs > max.width)){
    cut.index = ifelse(any(cutoffs < max.width),
                       which.max(cutoffs[cutoffs < max.width]),
                       1)
    cut.position = cutoffs[cut.index]
    # append new line
    lines = c(lines, paste0(c(next_indent, chars[1:cut.position]), collapse = ''))
    next_indent_len = indents[cut.index]
    cutoffs = cutoffs[-(1:cut.index)] - cut.position
    if (cutoffs[1] + next_indent_len > max.width){
      # next_indent_len = default.indent
      if (force.cut){
        next_indent_len = default.indent
      }else{
        warning('The value of `max.width` is too small, width overflowed')
      }
    }
    # cutoffs = cutoffs + next_indent_len
    next_indent = rep(' ', next_indent_len)  # next indent
    chars = chars[-(1:cut.position)]
    indents = indents[-(1:cut.index)]
  }
  lines = c(lines, paste0(c(next_indent, chars), collapse = ''))
  res = paste(lines, collapse = '\n')
  res
}

#' Find the possible positions for cutoff
#'
#' The positions to cutoff could be after comma or left bracket.
#' The pipe operator `%>%` is also supported.
#' Should use the function: sourcetools::tokenize_string
find_cutoff <- function(chars, default.indent = 2){
  # find all the positions can be cut off
  current_line = ''; nlength = 0;
  quotes = c(); bracket = c(); indent = c()
  cutoff = list(index=c(), indent=c());
  for (char in chars){
    nlength = nlength + 1
    current_line = paste(current_line, char, sep = '')
    if (char %in% c('"', "'")){
      if (length(quotes) >= 1 && tail(quotes, 1) == char){
        quotes = pop(quotes)
      }else{
        quotes = c(quotes, char)
      }
    }else if (length(quotes) == 0){
      if (char %in% c(',', '(', '{')){
        # postion to indent & cutoff
        if (char == '(') {bracket = c(bracket, char); indent = c(indent, nlength)}
        cutoff$index = c(cutoff$index, nlength)
        cutoff$indent = c(cutoff$indent, ifelse(length(indent) > 0, tail(indent, 1), default.indent))
      }else if (char == ')'){
        bracket = pop(bracket)
        indent = pop(indent)
      }
    }
  }
  # add support for magrittr '%>%'
  pipe <- gregexpr('%>%', paste0(chars, collapse = ''))[[1]]
  if (all(pipe > 0)){
    cutoff$index = c(cutoff$index, as.numeric(pipe))
    cutoff$indent = c(cutoff$indent, rep(default.indent, length(pipe)))
    cutoff$indent = order(cutoff$index)
    cutoff$index = sort(cutoff$index)
  }
  cutoff
}

#' Remove the last element of a vector
pop <- function(vec){
  if (length(vec) > 0) vec = vec[-length(vec)]
  vec
}

#' @export
#' @rdname deparse2
deparse1 <- function(x, ...){
  code = highr:::group_src(x)
  res = sapply(code, deparse2, ...)
  paste(res, collapse = '\n')
}

#' @export
#' @rdname deparse2
deparse0 <- function(x, ...){
  x = paste0(base::deparse(x), collapse='\n')
  res = deparse1(x)
  lines = strsplit(res, '\n')[[1]]
  lines
}

