# Fix global variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}
