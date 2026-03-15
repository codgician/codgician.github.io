{
  lib,
  runCommand,
  sources,
}:

runCommand "reveal-js-${sources.reveal-js.version}" {
  inherit (sources.reveal-js) src;
  meta = {
    description = "The HTML Presentation Framework";
    homepage = "https://revealjs.com/";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ codgician ];
  };
} ''
  mkdir -p $out
  tar -xzf $src --strip-components=1 -C $out
''
