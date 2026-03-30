# Comment alignment and wrapping rules for DanRStyle
# --------------------------------------------------

#' Align comments to a consistent column
#'
#' This transformer aligns trailing comments so that blocks of code have
#' comments starting at the same column. It only aligns comments that appear
#' after code on the same line (not full-line comments).
align_comments_transformer <- function(pd_flat) {
  
  # Identify comment tokens
  comment_idx <- which(pd_flat$token == "COMMENT")
  
  if (length(comment_idx) == 0) {
    return(pd_flat)
  }
  
  # Determine the target alignment column
  # We align comments to 2 spaces after the longest code segment
  code_lengths <- sapply(comment_idx, function(i) {
    # Length of text before the comment on the same line
    line_idx <- pd_flat$line1[i]
    max(pd_flat$col2[pd_flat$line1 == line_idx & pd_flat$token != "COMMENT"], na.rm = TRUE)
  })
  
  target_col <- max(code_lengths, na.rm = TRUE) + 2
  
  # Apply alignment
  for (i in seq_along(comment_idx)) {
    idx <- comment_idx[i]
    current_col <- pd_flat$col1[idx]
    needed_spaces <- max(target_col - current_col, 1)
    pd_flat$lag_spaces[idx] <- needed_spaces
  }
  
  pd_flat
}


#' Wrap long comments at 80 characters
#'
#' This transformer wraps comments that exceed 80 characters. It preserves
#' indentation and does not modify code lines.
wrap_comments_transformer <- function(pd_flat, width = 80) {
  
  comment_idx <- which(pd_flat$token == "COMMENT")
  
  if (length(comment_idx) == 0) {
    return(pd_flat)
  }
  
  for (i in comment_idx) {
    text <- pd_flat$text[i]
    
    # Skip roxygen comments
    if (startsWith(text, "#'")) next
    
    # Remove leading "# " for wrapping
    stripped <- sub("^#\\s?", "", text)
    
    if (nchar(stripped) <= width) next
    
    # Wrap the comment text
    wrapped <- strwrap(stripped, width = width)
    
    # Rebuild wrapped comment lines with "# "
    wrapped <- paste0("# ", wrapped)
    
    # Replace the original token with the first wrapped line
    pd_flat$text[i] <- wrapped[1]
    
    # Insert additional wrapped lines as new rows
    if (length(wrapped) > 1) {
      new_rows <- pd_flat[rep(i, length(wrapped) - 1), ]
      new_rows$text <- wrapped[-1]
      new_rows$lag_newlines <- 1
      pd_flat <- rbind(
        pd_flat[1:i, ],
        new_rows,
        pd_flat[(i + 1):nrow(pd_flat), ]
      )
    }
  }
  
  pd_flat
}