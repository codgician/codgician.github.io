/**
 * TOC (Table of Contents) functionality
 * - Scroll spy: highlights current section in TOC
 * - Toggle: show/hide TOC via navbar button
 * - Smooth scroll: clicking TOC links scrolls smoothly
 *
 * State logic:
 * - Desktop (>= 1280px): TOC expanded by default
 * - Mobile (< 1280px): TOC collapsed by default
 * - Initial state determined by viewport at page load
 * - State persists across resize (user preference preserved)
 */
(function () {
  const tocDesktop = document.querySelector(".toc");
  const tocMobile = document.querySelector(".toc-mobile");
  const tocToggle = document.getElementById("toc-toggle");
  const tocNav = document.querySelector(".toc-nav");
  const tocMobileNav = document.querySelector(".toc-mobile-nav");

  // Exit early if no TOC elements
  if (!tocDesktop && !tocMobile) return;

  const prefersReducedMotion = window.matchMedia(
    "(prefers-reduced-motion: reduce)"
  ).matches;

  // ==========================================================================
  // Helpers
  // ==========================================================================

  function getNavOffset() {
    return (document.querySelector(".nav")?.offsetHeight || 56) + 16;
  }

  function scrollToElement(el) {
    const targetPosition =
      el.getBoundingClientRect().top + window.scrollY - getNavOffset();
    window.scrollTo({
      top: targetPosition,
      behavior: prefersReducedMotion ? "auto" : "smooth",
    });
  }

  // Media query for mobile/desktop breakpoint (matches CSS $bp-xl: 1280px)
  const mobileQuery = window.matchMedia("(max-width: 1279px)");
  const isMobile = () => mobileQuery.matches;

  // ==========================================================================
  // TOC Visibility State
  // ==========================================================================

  // Initialize based on viewport at page load:
  // - Desktop: expanded (true)
  // - Mobile: collapsed (false)
  let tocVisible = !isMobile();

  // Apply visibility state to DOM
  function applyTocState() {
    if (tocDesktop) {
      tocDesktop.classList.toggle("toc--hidden", !tocVisible);
    }
    if (tocMobile) {
      tocMobile.open = tocVisible;
    }
  }

  // Set initial state
  applyTocState();

  // Toggle TOC visibility
  function toggleToc() {
    tocVisible = !tocVisible;
    applyTocState();
  }

  // Sync state when mobile TOC is toggled directly (user clicks summary)
  if (tocMobile) {
    tocMobile.addEventListener("toggle", () => {
      // Only sync if state actually differs (prevents loops)
      if (tocMobile.open !== tocVisible) {
        tocVisible = tocMobile.open;
        applyTocState();
      }
    });
  }

  // ==========================================================================
  // Desktop TOC: Scroll fade indicator
  // ==========================================================================

  function updateScrollFade() {
    if (!tocDesktop) return;
    const hasOverflow = tocDesktop.scrollHeight > tocDesktop.clientHeight;
    const isAtBottom =
      tocDesktop.scrollHeight - tocDesktop.scrollTop <=
      tocDesktop.clientHeight + 2;
    tocDesktop.classList.toggle("toc--has-overflow", hasOverflow);
    tocDesktop.classList.toggle("toc--scrolled-bottom", isAtBottom);
  }

  if (tocDesktop) {
    tocDesktop.addEventListener("scroll", updateScrollFade, { passive: true });
    requestAnimationFrame(updateScrollFade);
  }

  // ==========================================================================
  // TOC Toggle Button (navbar)
  // ==========================================================================

  if (tocToggle) {
    tocToggle.addEventListener("click", () => {
      toggleToc();

      // On mobile, scroll to TOC and highlight when opening
      if (isMobile() && tocMobile && tocVisible) {
        const tocPosition =
          tocMobile.getBoundingClientRect().top +
          window.scrollY -
          getNavOffset();
        window.scrollTo({
          top: tocPosition,
          behavior: prefersReducedMotion ? "auto" : "smooth",
        });

        tocMobile.classList.add("toc-mobile--highlight");
        setTimeout(() => {
          tocMobile.classList.remove("toc-mobile--highlight");
        }, 1000);
      }
    });
  }

  // ==========================================================================
  // Mobile TOC: smooth scroll on link click
  // ==========================================================================

  if (tocMobileNav) {
    tocMobileNav.querySelectorAll("a[href^='#']").forEach((link) => {
      link.addEventListener("click", (e) => {
        const targetId = link.getAttribute("href").slice(1);
        const target = document.getElementById(targetId);
        if (target) {
          e.preventDefault();
          scrollToElement(target);
          history.pushState(null, "", `#${targetId}`);
        }
      });
    });
  }

  // ==========================================================================
  // Scroll Spy (Desktop TOC)
  // ==========================================================================

  if (!tocNav) return;

  const tocLinks = tocNav.querySelectorAll("a[href^='#']");
  if (tocLinks.length === 0) return;

  const headingIds = Array.from(tocLinks).map((link) =>
    link.getAttribute("href").slice(1)
  );
  const headings = headingIds
    .map((id) => document.getElementById(id))
    .filter((el) => el !== null);

  if (headings.length === 0) return;

  let currentActiveId = null;

  function findLastPassedHeading(scrollTop) {
    let lastPassed = null;
    for (const heading of headings) {
      if (heading.offsetTop <= scrollTop + 100) {
        lastPassed = heading;
      } else {
        break;
      }
    }
    return lastPassed;
  }

  function setActiveLink(id) {
    if (id === currentActiveId) return;

    tocLinks.forEach((link) => link.classList.remove("toc-active"));

    if (id) {
      const escapedId = CSS.escape ? CSS.escape(id) : id.replace(/["\\]/g, "\\$&");
      const activeLink = tocNav.querySelector(`a[href="#${escapedId}"]`);
      if (activeLink) {
        activeLink.classList.add("toc-active");
      }
    }

    currentActiveId = id;
  }

  // IntersectionObserver for scroll spy
  if (window.IntersectionObserver) {
    const observer = new IntersectionObserver(
      (entries) => {
        const visibleEntries = entries.filter((entry) => entry.isIntersecting);

        if (visibleEntries.length > 0) {
          visibleEntries.sort(
            (a, b) => a.boundingClientRect.top - b.boundingClientRect.top
          );
          setActiveLink(visibleEntries[0].target.id);
        } else {
          const lastPassedHeading = findLastPassedHeading(window.scrollY);
          if (lastPassedHeading) {
            setActiveLink(lastPassedHeading.id);
          }
        }
      },
      {
        rootMargin: "-80px 0px -50% 0px",
        threshold: 0,
      }
    );

    headings.forEach((heading) => observer.observe(heading));
  }

  // Desktop TOC: smooth scroll on link click
  tocLinks.forEach((link) => {
    link.addEventListener("click", (e) => {
      const targetId = link.getAttribute("href").slice(1);
      const target = document.getElementById(targetId);
      if (target) {
        e.preventDefault();
        scrollToElement(target);
        history.pushState(null, "", `#${targetId}`);
        setActiveLink(targetId);
      }
    });
  });

  // Set initial active state
  const initialHeading = findLastPassedHeading(window.scrollY);
  if (initialHeading) {
    setActiveLink(initialHeading.id);
  } else if (headings.length > 0) {
    setActiveLink(headings[0].id);
  }
})();
