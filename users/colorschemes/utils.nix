with builtins;
{
  transparentify = color:
    "#99${substring 1 (stringLength color - 1) color}";

  # TODO: Implemet a version that makes a color brighter
  # Possible algo
  # - conver RGB to HSL
  # - adjust luminosity (L)
  # - convert back to RGB
  brighter = color:
    color;

  # TODO: Implemet a version that makes a color dimmer
  dimmer = color:
    color;
}
