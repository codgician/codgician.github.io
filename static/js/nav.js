(function () {
  const nav = document.querySelector(".nav");
  if (!nav) return;

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
