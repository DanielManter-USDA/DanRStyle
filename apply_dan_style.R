# RStudio Addin: Apply Dan's Style
# --------------------------------
# This provides a one-click Addin that applies DanRStyle::dan_style()
# to the currently active R script or selected text.

#' Apply Dan's custom style to the active RStudio document
#'
#' This function is bound to the RStudio Addin "Apply Dan's Style".
#' It styles either the selected text or the entire document using
#' DanRStyle::dan_style().
#'
#' @export
apply_dan_style <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("RStudio API is not available. This Addin only works inside RStudio.")
  }
  
  ctx <- rstudioapi::getActiveDocumentContext()
  
  # If user has selected text, style only that
  if (nchar(ctx$selection[[1]]$text) > 0) {
    styled <- styler::style_text(
      ctx$selection[[1]]$text,
      style = DanRStyle::dan_style
    )
    
    rstudioapi::modifyRange(
      ctx$selection[[1]]$range,
      paste0(styled, collapse = "\n")
    )
    
  } else {
    # Otherwise style the entire document
    styled <- styler::style_text(
      ctx$contents,
      style = DanRStyle::dan_style
    )
    
    rstudioapi::setDocumentContents(
      paste0(styled, collapse = "\n"),
      id = ctx$id
    )
  }
}