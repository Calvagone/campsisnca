fixBrackets = function(file){
  x=readLines(file)
  x=gsub("\\[", "[", x, fixed=TRUE)
  x=gsub("\\]", "]", x, fixed=TRUE)
  x=gsub("\\*", "*", x, fixed=TRUE)
  writeLines(x, file)
}
fixBrackets("README.md")