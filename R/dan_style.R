#' dan_style: Dan Manter's custom R code formatting style
#'
#' This is the main exported style function for the DanRStyle package.
#' It assembles all custom transformers:
#' - pipe-at-start rewriting
#' - soft alignment of `=`
#' - tidyverse indentation
#' - operator spacing
#' - aligned comments
#' - wrapped comments
#' - multi-line argument breaking
#' - git-aware styling
#'
#' @export
dan_style <- function(scope = "tokens", strict = FALSE) {
  
  # Base tidyverse style as foundation
  style <- styler::tidyverse_style(
    scope  = scope,
    strict = strict
  )
  
  style$token$transformers <- list(
    pipe_transformer       = pipe_rewriter,
    operator_spacing       = operator_spacing_transformer,
    soft_align             = soft_align_equals,
    comment_alignment      = align_comments_transformer,
    comment_wrapping       = wrap_comments_transformer,
    argument_breaking      = break_arguments_transformer
  )
  
  # Git-aware styling (only changed lines)
  style$line$use_raw_indention <- TRUE
  
  style
}