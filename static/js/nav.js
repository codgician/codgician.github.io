(function () {
  const nav = document.querySelector(".nav");
  if (!nav) return;

  // ==========================================================================
  // Active nav link detection
  // ==========================================================================
  const navLinks = nav.querySelectorAll(".nav-links a");
  const currentPath = window.location.pathname;

  navLinks.forEach((link) => {
    // Use link.pathname to get the resolved absolute path
    const linkPath = new URL(link.href).pathname;
    // Check if current path starts with link path (but not just the language root)
    // e.g., /en/posts/... matches /en/posts/, /en/slides/... matches /en/slides/
    const isLangRoot = /^\/[a-z]{2}\/$/.test(linkPath);
    if (linkPath && !isLangRoot && currentPath.startsWith(linkPath)) {
      link.classList.add("active");
    }
  });

  let lastScrollY = window.scrollY;
  let ticking = false;

  // Threshold to prevent hiding on small scrolls
  const scrollThreshold = 10;

  function updateNav() {
    const currentScrollY = window.scrollY;
    const scrollDelta = currentScrollY - lastScrollY;

    // Only act on significant scroll changes
    if (Math.abs(scrollDelta) < scrollThreshold) {
      ticking = false;
      return;
    }

    // At top of page: always show nav
    if (currentScrollY <= 0) {
      nav.classList.remove("nav--hidden");
    }
    // Scrolling down: hide nav
    else if (scrollDelta > 0) {
      nav.classList.add("nav--hidden");
    }
    // Scrolling up: show nav
    else {
      nav.classList.remove("nav--hidden");
    }

    lastScrollY = currentScrollY;
    ticking = false;
  }

  function onScroll() {
    if (!ticking) {
      window.requestAnimationFrame(updateNav);
      ticking = true;
    }
  }

  window.addEventListener("scroll", onScroll, { passive: true });
})();
