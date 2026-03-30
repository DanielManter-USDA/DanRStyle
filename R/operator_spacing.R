# Operator spacing rules for DanRStyle
# ------------------------------------

#' Apply tidyverse-style spacing around operators
#'
#' Ensures consistent spacing around assignment, arithmetic, logical,
#' and comparison operators. Does not modify spacing inside strings,
#' roxygen comments, or function call parentheses.
operator_spacing_transformer <- function(pd_flat) {
  
  # Operators that should have spaces around them
  spaced_ops <- c(
    "EQ_ASSIGN",      # =
    "LEFT_ASSIGN",    # <-
    "RIGHT_ASSIGN",   # ->
    "PLUS", "MINUS",  # + -
    "STAR", "SLASH",  # * /
    "EQ", "NE",       # == !=
    "GE", "LE", "GT", "LT"  # >= <= > <
  )
  
  op_idx <- which(pd_flat$token %in% spaced_ops)
  
  if (length(op_idx) == 0) {
    return(pd_flat)
  }
  
  for (i in op_idx) {
    
    # Skip if inside parentheses with no spacing desired
    # Example: mean(x, na.rm=TRUE) should become na.rm = TRUE
    # but we avoid adding spaces around parentheses themselves
    if (pd_flat$token[i] %in% c("EQ_ASSIGN")) {
      # Always enforce spaces around '='
      pd_flat$lag_spaces[i] <- 1
      pd_flat$lead_spaces[i] <- 1
      next
    }
    
    # General rule: enforce one space before and after operator
    pd_flat$lag_spaces[i]  <- 1
    pd_flat$lead_spaces[i] <- 1
  }
  
  pd_flat
}


#' Soft alignment of equals signs inside function calls and lists
#'
#' This aligns named arguments like:
#'   tibble(
#'     x   = 1,
#'     long = 2
#'   )
#'
#' but does NOT enforce strict alignment across unrelated blocks.
soft_align_equals <- function(pd_flat) {
  
  eq_idx <- which(pd_flat$token == "EQ_ASSIGN")
  
  if (length(eq_idx) == 0) {
    return(pd_flat)
  }
  
  # Group by line number
  lines <- split(eq_idx, pd_flat$line1[eq_idx])
  
  for (line in lines) {
    # Determine the longest LHS on this line
    lhs_lengths <- sapply(line, function(i) {
      max(pd_flat$col1[pd_flat$line1 == pd_flat$line1[i] &
                         pd_flat$col2 < pd_flat$col1[i]])
    })
    
    target_col <- max(lhs_lengths) + 2
    
    for (i in line) {
      current_col <- pd_flat$col1[i]
      pd_flat$lag_spaces[i] <- max(target_col - current_col, 1)
    }
  }
  
  pd_flat
}