{
  lib,
  runCommand,
  sources,
}:

runCommand "lucide-static-${sources.lucide-static.version}"
  {
    inherit (sources.lucide-static) src;
    meta = {
      description = "Lucide icon library - static assets (icon font and SVGs)";
      homepage = "https://lucide.dev/";
      license = lib.licenses.isc;
      maintainers = with lib.maintainers; [ codgician ];
    };
  }
  ''
    mkdir -p $out
    tar -xzf $src --strip-components=1 -C $out
  ''
