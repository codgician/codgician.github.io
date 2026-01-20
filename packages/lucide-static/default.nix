{
  lib,
  fetchzip,
}:

let
  version = "0.544.0";
in
fetchzip {
  pname = "lucide-static";
  inherit version;

  url = "https://registry.npmjs.org/lucide-static/-/lucide-static-${version}.tgz";
  hash = "sha256-8+MABl6ToG4e3SM7VEzoDoHsCY52gtlMVW9EuOk5fd0=";
  stripRoot = true;

  meta = {
    description = "Lucide icon library - static assets (icon font and SVGs)";
    homepage = "https://lucide.dev/";
    license = lib.licenses.isc;
    maintainers = with lib.maintainers; [ codgician ];
  };
}
