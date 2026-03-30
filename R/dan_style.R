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
  
  # Replace / extend specific transformers
  style$token$transformers <- list(
    # 1. Pipe-at-start rewriting
    pipe_rewriter,
    
    # 2. Operator spacing
    operator_spacing_transformer,
    
    # 3. Soft alignment of `=`
    soft_align_equals,
    
    # 4. Comment alignment + wrapping
    align_comments_transformer,
    wrap_comments_transformer,
    
    # 5. Multi-line argument breaking
    break_arguments_transformer
  )
  
  # Git-aware styling (only changed lines)
  style$line$use_raw_indention <- TRUE
  
  style
}