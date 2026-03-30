#' break_arguments_transformer: Multi-line argument breaking
#'
#' Ensures that function calls with multiple arguments are formatted
#' with one argument per line, preserving indentation and comments.
#'
#' This transformer operates on the styler parse table (`pd`) and is
#' designed to be used inside the DanRStyle custom style list.
#'
#' @keywords internal
break_arguments_transformer <- function(pd) {
  
  # Only operate on function calls with parentheses
  is_call <- pd$token == "SYMBOL_FUNCTION_CALL"
  if (!any(is_call)) return(pd)
  
  # Identify opening and closing parentheses for each call
  open_paren  <- which(pd$token == "'('")
  close_paren <- which(pd$token == "')'")
  
  # Nothing to do if no parentheses
  if (length(open_paren) == 0 || length(close_paren) == 0) {
    return(pd)
  }
  
  # Helper: break arguments for a single call
  break_one_call <- function(pd, open_idx, close_idx) {
    
    # Extract the argument region
    arg_region <- seq(open_idx + 1, close_idx - 1)
    
    # If no arguments, nothing to break
    if (length(arg_region) == 0) return(pd)
    
    # Detect commas inside the argument region
    commas <- arg_region[pd$token[arg_region] == "','"]
    
    # If no commas, it's a single-argument call → leave unchanged
    if (length(commas) == 0) return(pd)
    
    # Determine indentation level
    indent_level <- pd$col1[open_idx] + 2
    
    # Insert newlines after each comma
    for (i in commas) {
      pd$newlines[i] <- 1
      pd$spaces[i]   <- 0
    }
    
    # Indent each argument line
    for (i in arg_region) {
      if (pd$newlines[i] > 0) {
        pd$spaces[i] <- indent_level
      }
    }
    
    pd
  }
  
  # Apply to all calls
  for (op in open_paren) {
    # Find matching closing paren
    cp <- close_paren[close_paren > op][1]
    if (!is.na(cp)) {
      pd <- break_one_call(pd, op, cp)
    }
  }
  
  pd
}