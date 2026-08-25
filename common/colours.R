# Brand colours. _brand.yml is the source of brand truth; ggplot cannot read
# it, so these mirror it by hand.

colors <- list(
  brand = c(
    white  = "#FFFFFF",
    black  = "#000000",
    blue   = "#004986",  # base (digital primary)
    forest = "#115740",  # base (deep green)
    green  = "#89A84F",  # base
    red    = "#AB2328",  # base
    orange = "#E87722",  # action
    yellow = "#F6BE00",  # action
    sky    = "#CCDAE6",  # grounding (pale blue)
    grey   = "#D8D8D8"   # grounding (light grey)
  ),

  # Subsets of `brand` by usage category, handy for scale_*_manual().
  brand_base = c(
    blue = "#004986", forest = "#115740", green = "#89A84F", red = "#AB2328"
  ),

  brand_action = c(
    orange = "#E87722", yellow = "#F6BE00"
  ),

  brand_grounding = c(
    sky = "#CCDAE6", grey = "#D8D8D8"
  )
)
