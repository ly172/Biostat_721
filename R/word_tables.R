# Native, editable Word tables with the same three horizontal rules as the PDF.
# Called only by the Word branch of project_report.Rmd; uses base R.
word_table <- function(data, widths) {
  xml_escape <- function(x) {
    x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  widths <- as.integer(round(widths * 20)) # Word measures widths in twentieths of a point.
  cell <- function(value, column, header = FALSE) {
    border <- if (header) '<w:tcBorders><w:bottom w:val="single" w:sz="4" w:color="000000"/></w:tcBorders>' else ""
    paste0('<w:tc><w:tcPr><w:tcW w:w="', widths[column], '" w:type="dxa"/>', border,
           '</w:tcPr><w:p><w:pPr><w:spacing w:before="0" w:after="0" w:line="260" w:lineRule="exact"/>',
           '<w:ind w:firstLine="0"/><w:jc w:val="', if (column == ncol(data)) 'right' else 'left',
           '"/></w:pPr><w:r><w:rPr><w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman"/>',
           '<w:sz w:val="20"/></w:rPr><w:t xml:space="preserve">', xml_escape(value),
           '</w:t></w:r></w:p></w:tc>')
  }
  row <- function(values, header = FALSE) {
    paste0('<w:tr><w:trPr><w:cantSplit/>', if (header) '<w:tblHeader/>' else '',
           '</w:trPr>', paste(vapply(seq_along(values), function(j) cell(values[j], j, header), ""), collapse = ''), '</w:tr>')
  }
  borders <- paste0('<w:tblBorders>',
    '<w:top w:val="single" w:sz="7" w:color="000000"/>',
    '<w:left w:val="nil"/><w:bottom w:val="single" w:sz="7" w:color="000000"/>',
    '<w:right w:val="nil"/><w:insideH w:val="nil"/><w:insideV w:val="nil"/></w:tblBorders>')
  xml <- paste0('<w:tbl><w:tblPr><w:tblW w:w="', sum(widths), '" w:type="dxa"/>',
    '<w:jc w:val="center"/><w:tblLayout w:type="fixed"/>', borders,
    '<w:tblCellMar><w:top w:w="0" w:type="dxa"/><w:left w:w="100" w:type="dxa"/>',
    '<w:bottom w:w="0" w:type="dxa"/><w:right w:w="100" w:type="dxa"/></w:tblCellMar></w:tblPr>',
    '<w:tblGrid>', paste0('<w:gridCol w:w="', widths, '"/>', collapse = ''), '</w:tblGrid>',
    row(names(data), TRUE), paste(vapply(seq_len(nrow(data)), function(i) row(unlist(data[i, ], use.names = FALSE)), ""), collapse = ''), '</w:tbl>')
  cat('\n```{=openxml}\n', xml, '\n```\n\n', sep = '')
}

word_paragraph <- function(text) {
  cat('\n\n', text, '\n\n', sep = '')
}
