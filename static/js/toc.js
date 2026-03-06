/**
 * TOC Scroll Spy & Toggle
 * - Highlights current section in table of contents
 * - Toggle button: hides/shows TOC on desktop, scrolls to TOC on mobile
 * - Smooth expand/collapse animation for mobile TOC
 * Uses IntersectionObserver for performance
 * Compatible with modern browsers (2020+)
 */
(function () {
  const tocDesktop = document.querySelector(".toc");
  const tocMobile = document.querySelector(".toc-mobile");
  const tocToggle = document.getElementById("toc-toggle");
  const tocNav = document.querySelector(".toc-nav");
  const tocMobileNav = document.querySelector(".toc-mobile-nav");

  // Respect reduced motion preference
  const prefersReducedMotion = window.matchMedia(
    "(prefers-reduced-motion: reduce)"
  ).matches;

  // ==========================================================================
  // Shared helpers
  // ==========================================================================

  // Get nav bar offset for scroll calculations (nav height + padding)
  function getNavOffset() {
    return (document.querySelector(".nav")?.offsetHeight || 56) + 16;
  }

  // Update scroll fade indicator on desktop TOC
  // Only shows fade when content overflows and not scrolled to bottom
  function updateScrollFade() {
    if (!tocDesktop) return;
    const hasOverflow = tocDesktop.scrollHeight > tocDesktop.clientHeight;
    const isAtBottom =
      tocDesktop.scrollHeight - tocDesktop.scrollTop <= tocDesktop.clientHeight + 2;
    tocDesktop.classList.toggle("toc--has-overflow", hasOverflow);
    tocDesktop.classList.toggle("toc--scrolled-bottom", isAtBottom);
  }

  // Listen for scroll on desktop TOC
  if (tocDesktop) {
    tocDesktop.addEventListener("scroll", updateScrollFade, { passive: true });
    // Initial check (after layout settles)
    requestAnimationFrame(updateScrollFade);
  }

  // ==========================================================================
  // Shared TOC visibility state (synced between mobile and desktop)
  // ==========================================================================
  let tocVisible = true;
  let isUpdating = false; // Guard against re-entrancy from toggle events

  // Apply visibility state to both TOCs
  function setTocVisible(visible) {
    if (isUpdating) return;
    isUpdating = true;

    tocVisible = visible;

    // Update desktop TOC
    if (tocDesktop) {
      tocDesktop.classList.toggle("toc--hidden", !visible);
    }

    // Update mobile TOC (this triggers 'toggle' event, guarded above)
    if (tocMobile) {
      tocMobile.open = visible;
    }


    isUpdating = false;
  }

  // Sync state when mobile TOC is toggled directly (user clicks summary)
  if (tocMobile) {
    tocMobile.addEventListener("toggle", () => {
      if (isUpdating) return; // Ignore events triggered by setTocVisible
      setTocVisible(tocMobile.open);
    });
  }

  // ==========================================================================
  // TOC Toggle Button (navbar)
  // ==========================================================================
  if (tocToggle) {
    // Use matchMedia for consistent breakpoint with CSS
    const mobileQuery = window.matchMedia("(max-width: 1279px)");
    const isMobile = () => mobileQuery.matches;

    tocToggle.addEventListener("click", () => {
      // Toggle shared visibility state
      setTocVisible(!tocVisible);

      // On mobile, also scroll to TOC and highlight
      if (isMobile() && tocMobile && tocVisible) {
        const tocPosition =
          tocMobile.getBoundingClientRect().top + window.scrollY - getNavOffset();

        window.scrollTo({
          top: tocPosition,
          behavior: prefersReducedMotion ? "auto" : "smooth",
        });

        // Add highlight animation
        tocMobile.classList.add("toc-mobile--highlight");
        setTimeout(() => {
          tocMobile.classList.remove("toc-mobile--highlight");
        }, 1000);
      }
    });
  }

  // ==========================================================================
  // Shared helper: scroll to element with nav offset
  // ==========================================================================
  function scrollToElement(el) {
    const targetPosition =
      el.getBoundingClientRect().top + window.scrollY - getNavOffset();

    window.scrollTo({
      top: targetPosition,
      behavior: prefersReducedMotion ? "auto" : "smooth",
    });
  }

  // ==========================================================================
  // Mobile TOC: smooth scroll on link click
  // ==========================================================================
  if (tocMobileNav) {
    const mobileLinks = tocMobileNav.querySelectorAll("a[href^='#']");
    mobileLinks.forEach((link) => {
      link.addEventListener("click", (e) => {
        const targetId = link.getAttribute("href").slice(1);
        const target = document.getElementById(targetId);

        if (target) {
          e.preventDefault();
          scrollToElement(target);

          // Update URL hash without jumping
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

  // Get all heading IDs from the TOC links
  const headingIds = Array.from(tocLinks).map((link) =>
    link.getAttribute("href").slice(1)
  );

  // Find corresponding heading elements in the document
  const headings = headingIds
    .map((id) => document.getElementById(id))
    .filter((el) => el !== null);

  if (headings.length === 0) return;

  // Find the last heading that has been scrolled past
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

  // Track which heading is currently "active"
  let currentActiveId = null;

  function setActiveLink(id) {
    if (id === currentActiveId) return;

    // Remove active class from all links
    tocLinks.forEach((link) => link.classList.remove("toc-active"));

    // Add active class to matching link
    if (id) {
      // Use CSS.escape for proper selector escaping (handles ., :, [, etc.)
      const escapedId = CSS.escape ? CSS.escape(id) : id.replace(/["\\]/g, "\\$&");
      const activeLink = tocNav.querySelector(`a[href="#${escapedId}"]`);
      if (activeLink) {
        activeLink.classList.add("toc-active");
      }
    }

    currentActiveId = id;
  }

  // Create IntersectionObserver to track heading visibility
  if (window.IntersectionObserver) {
    const observer = new IntersectionObserver(
      (entries) => {
        const visibleEntries = entries.filter((entry) => entry.isIntersecting);

        if (visibleEntries.length > 0) {
          // Sort by position in document (topmost first)
          visibleEntries.sort(
            (a, b) => a.boundingClientRect.top - b.boundingClientRect.top
          );
          setActiveLink(visibleEntries[0].target.id);
        } else {
          // When scrolling up past all headings, find the last one we passed
          const lastPassedHeading = findLastPassedHeading(window.scrollY);
          if (lastPassedHeading) {
            setActiveLink(lastPassedHeading.id);
          }
        }
      },
      {
        // Observe from just below the nav bar to the middle of the viewport
        rootMargin: "-80px 0px -50% 0px",
        threshold: 0,
      }
    );

    // Observe all headings
    headings.forEach((heading) => observer.observe(heading));
  }

  // Handle click on TOC links - smooth scroll
  tocLinks.forEach((link) => {
    link.addEventListener("click", (e) => {
      const targetId = link.getAttribute("href").slice(1);
      const target = document.getElementById(targetId);

      if (target) {
        e.preventDefault();
        scrollToElement(target);

        // Update URL hash without jumping
        history.pushState(null, "", `#${targetId}`);

        // Set active immediately for responsive feel
        setActiveLink(targetId);
      }
    });
  });

  // Set initial active state based on scroll position
  const initialHeading = findLastPassedHeading(window.scrollY);
  if (initialHeading) {
    setActiveLink(initialHeading.id);
  } else if (headings.length > 0) {
    // If at top, highlight first heading
    setActiveLink(headings[0].id);
  }
})();
