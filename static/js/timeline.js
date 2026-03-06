/**
 * Timeline Scroll Spy & Navigation
 * - Active year in timeline becomes large (acts as title)
 * - Scrolls to first post of year when clicking
 * - Syncs with scroll position
 */
(function () {
  const timeline = document.querySelector(".timeline");
  if (!timeline) return;

  const timelineNav = document.querySelector(".timeline-nav");
  const timelineContent = document.querySelector(".timeline-content");
  const yearLinks = timelineNav?.querySelectorAll(".timeline-year");
  const posts = document.querySelectorAll(".timeline-post");

  // Guard: require all critical elements
  if (!timelineNav || !timelineContent || !yearLinks || yearLinks.length === 0 || posts.length === 0) return;

  // Build year -> first post mapping and year -> link mapping
  const yearToFirstPost = new Map();
  const yearToLink = new Map();
  const validYears = new Set();

  posts.forEach((post) => {
    const year = post.dataset.year;
    if (year && !yearToFirstPost.has(year)) {
      yearToFirstPost.set(year, post);
    }
  });

  yearLinks.forEach((link) => {
    const year = link.dataset.year;
    if (year) {
      yearToLink.set(year, link);
      validYears.add(year);
    }
  });

  const prefersReducedMotion = window.matchMedia(
    "(prefers-reduced-motion: reduce)"
  ).matches;

  // Cache nav element for offset calculations
  const navElement = document.querySelector(".nav");

  // ==========================================================================
  // Helpers
  // ==========================================================================

  function getScrollContainer() {
    return window.innerWidth >= 768 ? timelineContent : window;
  }

  function getScrollOffset() {
    if (window.innerWidth < 768) {
      return (navElement?.offsetHeight || 44) + (timelineNav?.offsetHeight || 40) + 16;
    }
    return 16;
  }

  function scrollToPost(post) {
    const container = getScrollContainer();
    
    if (container === window) {
      const targetY = post.getBoundingClientRect().top + window.scrollY - getScrollOffset();
      window.scrollTo({
        top: targetY,
        behavior: prefersReducedMotion ? "auto" : "smooth",
      });
    } else {
      container.scrollTo({
        top: post.offsetTop - getScrollOffset(),
        behavior: prefersReducedMotion ? "auto" : "smooth",
      });
    }
  }

  // ==========================================================================
  // Active year tracking
  // ==========================================================================

  let currentActiveYear = null;

  function setActiveYear(year) {
    if (year === currentActiveYear) return;

    yearLinks.forEach((link) => link.classList.remove("timeline-year--active"));
    
    if (year) {
      // Use precomputed map instead of querySelector with interpolated string
      const activeLink = yearToLink.get(year);
      if (activeLink) {
        activeLink.classList.add("timeline-year--active");

        // On mobile, scroll active year into view
        if (window.innerWidth < 768) {
          activeLink.scrollIntoView({
            behavior: prefersReducedMotion ? "auto" : "smooth",
            inline: "center",
            block: "nearest",
          });
        }
      }
    }

    currentActiveYear = year;
  }

  function findActiveYear() {
    const container = getScrollContainer();
    const scrollTop = container === window 
      ? window.scrollY 
      : container.scrollTop;
    const offset = getScrollOffset() + 50;

    let activeYear = null;
    
    for (const post of posts) {
      const postTop = container === window
        ? post.getBoundingClientRect().top + window.scrollY
        : post.offsetTop;
      
      if (postTop <= scrollTop + offset) {
        activeYear = post.dataset.year;
      } else {
        break;
      }
    }

    return activeYear || posts[0]?.dataset.year;
  }

  // ==========================================================================
  // Year navigation (DRY helper)
  // ==========================================================================

  function goToYear(year, updateHash = false) {
    const firstPost = yearToFirstPost.get(year);
    if (!firstPost) return false;

    scrollToPost(firstPost);
    if (updateHash) {
      history.pushState(null, "", `#${year}`);
    }
    setActiveYear(year);
    return true;
  }

  // ==========================================================================
  // Scroll handling
  // ==========================================================================

  let scrollTimeout;
  function handleScroll() {
    if (scrollTimeout) return;
    
    scrollTimeout = setTimeout(() => {
      scrollTimeout = null;
      setActiveYear(findActiveYear());
    }, 50);
  }

  function setupScrollListener() {
    const container = getScrollContainer();
    if (container === window) {
      window.addEventListener("scroll", handleScroll, { passive: true });
    } else {
      container.addEventListener("scroll", handleScroll, { passive: true });
    }
  }

  let resizeTimeout;
  window.addEventListener("resize", () => {
    if (resizeTimeout) clearTimeout(resizeTimeout);
    resizeTimeout = setTimeout(() => {
      window.removeEventListener("scroll", handleScroll);
      timelineContent?.removeEventListener("scroll", handleScroll);
      setupScrollListener();
      setActiveYear(findActiveYear());
    }, 200);
  });

  setupScrollListener();

  // ==========================================================================
  // Click handlers
  // ==========================================================================

  yearLinks.forEach((link) => {
    link.addEventListener("click", (e) => {
      e.preventDefault();
      const year = link.dataset.year;
      goToYear(year, true);
    });
  });

  // ==========================================================================
  // Initial state & hash handling
  // ==========================================================================

  function getYearFromHash() {
    const hash = window.location.hash.slice(1);
    // Use precomputed validYears Set
    if (validYears.has(hash)) {
      return hash;
    }
    // Support legacy #year-XXXX format
    if (hash.startsWith("year-")) {
      const year = hash.replace("year-", "");
      if (validYears.has(year)) return year;
    }
    return null;
  }

  const initialYear = getYearFromHash();
  if (initialYear) {
    setTimeout(() => goToYear(initialYear), 100);
  } else {
    setActiveYear(findActiveYear());
  }

  window.addEventListener("hashchange", () => {
    const year = getYearFromHash();
    if (year) {
      goToYear(year);
    }
  });
})();
