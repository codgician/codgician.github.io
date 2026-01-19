(function () {
  const toggle = document.getElementById("theme-toggle");
  const html = document.documentElement;

  // Load saved theme or detect system preference
  const saved = localStorage.getItem("theme");
  if (saved) {
    html.setAttribute("data-theme", saved);
  }

  // Update toggle button icon
  function updateToggle() {
    const isDark =
      html.getAttribute("data-theme") === "dark" ||
      (!html.getAttribute("data-theme") &&
        window.matchMedia("(prefers-color-scheme: dark)").matches);
    if (toggle) {
      const icon = toggle.querySelector("i");
      if (icon) {
        icon.className = isDark ? "icon-sun" : "icon-moon";
      }
    }
  }

  updateToggle();

  if (toggle) {
    toggle.addEventListener("click", function () {
      const current = html.getAttribute("data-theme");
      const next = current === "dark" ? "light" : "dark";
      html.setAttribute("data-theme", next);
      localStorage.setItem("theme", next);
      updateToggle();
    });
  }
})();
