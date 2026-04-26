{
  lib,
  runCommand,
  sources,
}:

runCommand "tabler-icons-webfont-${sources.tabler-icons-webfont.version}"
  {
    inherit (sources.tabler-icons-webfont) src;
    meta = {
      description = "Tabler Icons webfont assets";
      homepage = "https://tabler.io/icons";
      license = lib.licenses.mit;
      maintainers = with lib.maintainers; [ codgician ];
    };
  }
  ''
    mkdir -p $out
    tar -xzf $src --strip-components=1 -C $out
  ''
