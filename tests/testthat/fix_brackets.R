fixBrackets = function(file){
  x=readLines(file)
  y=gsub("\\[", "[", x, fixed = TRUE)
  y=gsub("\\]", "]", y, fixed = TRUE)
  writeLines(y, file)
}
fixBrackets("README.md")