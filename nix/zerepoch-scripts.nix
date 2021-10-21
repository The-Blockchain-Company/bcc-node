############################################################################
# The zerepoch-example scripts as nix-outputs
############################################################################

{ runCommand
, zerepoch-builder
}:

runCommand "zerepoch-example-scripts" { }
  ''
  mkdir -p $out
  cd $out
  ${zerepoch-builder}/bin/zerepoch-example
  ls -alR .
  ''
