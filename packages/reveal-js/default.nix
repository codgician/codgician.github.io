{
  lib,
  fetchzip,
}:

let
  version = "5.2.1";
in
fetchzip {
  pname = "reveal-js";
  inherit version;

  url = "https://registry.npmjs.org/reveal.js/-/reveal.js-${version}.tgz";
  hash = "sha256-zvqQZqO7wyitV5fTVnzwgQbtZaXUgY2UypMmVTdb96M=";
  stripRoot = true;

  meta = {
    description = "The HTML Presentation Framework";
    homepage = "https://revealjs.com/";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ codgician ];
  };
}
