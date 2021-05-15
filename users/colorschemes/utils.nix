with builtins;
{
  transparentify = color:
    "#99${substring 1 (stringLength color - 1) color}";

  brighter = color:
    color;

  dimmer = color:
    color;
}
