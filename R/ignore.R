#Due to some problem on R, and detect imports
#that are inside of assertions, lets do this trick
#https://github.com/r-lib/rcmdcheck/issues/207

ignore_unused_imports <- function() {
  sfnetworks::sfnetwork
  sf::st_as_sf
}