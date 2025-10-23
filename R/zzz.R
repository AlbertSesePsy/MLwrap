.onAttach <- function(libname, pkgname) {
  version <- read.dcf(
    file = system.file("DESCRIPTION", package = pkgname),
    fields = "Version"
  )
  packageStartupMessage("
*****************************************************************************

ooo        ooooo ooooo
 88.       .888   888
 888b     d 888   888         oooo oooo    ooo oooo d8b  .oooo.   oo.ooooo.
 8 Y88. .P  888   888           88.  88.  .8    888 8P  P  )88b   888   88b
 8   888    888   888            88..]88..8     888      .oP 888   888   888
 8    Y     888   888       o     888  888      888     d8(  888   888   888
o8o        o888o o888ooooood8      8    8      d888b     Y888  8o  888bod8P
                                                                   888
                                                                  o888o

*****************************************************************************

      MLwrap v", version, ": **Start simple, scale smart**
  ")
}

utils::globalVariables(c(
  ".", "x", "y", ".pred", "error", "value", "variable",
  "Class", "Importance", "Probability", "data_set", "label",
  "type", "density", "se", "S1", "ST", "prob_pred", "prob_observed",
  "truth", "estimate", "prob_estimate", "search_res", "sym",
  "tidy_object", "all_of", ".pred_class", "importance", "Variable", "StDev", "val_color", "iter",
  "Feature"
))
