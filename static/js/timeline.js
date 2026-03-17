/**
 * Timeline Scroll Spy & Navigation
 * - Active year in timeline becomes large (acts as title)
 * - Scrolls to first post of year when clicking
 * - Syncs with scroll position
 */
(function () {
  const timeline = document.querySelector(".timeline");
  if (!timeline) return;

  // Scope queries to timeline root for component isolation
  const timelineNav = timeline.querySelector(".timeline-nav");
  const timelineContent = timeline.querySelector(".timeline-content");
  const yearLinks = timelineNav?.querySelectorAll(".timeline-year");
  const posts = timeline.querySelectorAll(".timeline-post");

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

  const prefersReducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  const mobileQuery = window.matchMedia("(max-width: 767px)");

  // Cache nav element for offset calculations (outside timeline scope)
  const navElement = document.querySelector(".nav");

  // ==========================================================================
  // Helpers - Centralized breakpoint and visibility
  // ==========================================================================

  function isMobile() {
    return mobileQuery.matches;
  }

  function isPostVisible(post) {
    return !post.hidden;
  }

  function setPostVisible(post, visible) {
    post.hidden = !visible;
  }

  function getVisiblePosts() {
    return Array.from(posts).filter(isPostVisible);
  }

  function getScrollContainer() {
    return isMobile() ? window : timelineContent;
  }

  function getScrollOffset() {
    if (isMobile()) {
      // Only include nav height if it's not hidden (CSS changes top position when hidden)
      const navHeight = navElement?.classList.contains("nav--hidden") ? 0 : (navElement?.offsetHeight || 44);
      return navHeight + (timelineNav?.offsetHeight || 40) + 16;
    }
    return 16;
  }

  // ==========================================================================
  // Scroll position helpers - Unified coordinate calculations
  // ==========================================================================

  function getScrollTop(container) {
    return container === window ? window.scrollY : container.scrollTop;
  }

  function getPostTopInContainer(post, container) {
    if (container === window) {
      return post.getBoundingClientRect().top + window.scrollY;
    }
    const containerRect = container.getBoundingClientRect();
    const postRect = post.getBoundingClientRect();
    return postRect.top - containerRect.top + container.scrollTop;
  }

  function getPostBottomInContainer(post, container) {
    return getPostTopInContainer(post, container) + post.offsetHeight;
  }

  function scrollToPost(post) {
    const container = getScrollContainer();
    const targetTop = getPostTopInContainer(post, container) - getScrollOffset();

    container.scrollTo({
      top: targetTop,
      behavior: prefersReducedMotion ? "auto" : "smooth",
    });
  }

  // ==========================================================================
  // Active year tracking
  // ==========================================================================

  let currentActiveYear = null;

  function setActiveYear(year) {
    if (year === currentActiveYear) return;

    yearLinks.forEach((link) => link.classList.remove("timeline-year--active"));

    if (year) {
      const activeLink = yearToLink.get(year);
      if (activeLink) {
        activeLink.classList.add("timeline-year--active");

        // On mobile, scroll active year into view
        if (isMobile()) {
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
    const scrollTop = getScrollTop(container);
    const offset = getScrollOffset();

    const visiblePosts = getVisiblePosts();
    if (visiblePosts.length === 0) return null;

    // Find the last post that has scrolled past (its bottom is above viewport top)
    // This makes the year highlight when the previous year's content disappears
    let activeYear = visiblePosts[0]?.dataset.year;

    for (let i = 0; i < visiblePosts.length; i++) {
      const post = visiblePosts[i];
      const postBottom = getPostBottomInContainer(post, container);

      if (postBottom <= scrollTop + offset) {
        // Use next post's year, or current if it's the last post
        activeYear = visiblePosts[i + 1]?.dataset.year ?? post.dataset.year;
      } else {
        break;
      }
    }

    return activeYear;
  }

  // ==========================================================================
  // Year navigation
  // ==========================================================================

  function getFirstVisiblePostForYear(year) {
    return getVisiblePosts().find((post) => post.dataset.year === year);
  }

  function goToYear(year, updateHash = false) {
    // When filtering, only navigate to visible posts; otherwise allow fallback to any post
    const visiblePost = getFirstVisiblePostForYear(year);
    const firstPost = currentTagFilter ? visiblePost : (visiblePost || yearToFirstPost.get(year));
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
  let currentScrollContainer = null;

  function handleScroll() {
    if (scrollTimeout) return;

    scrollTimeout = setTimeout(() => {
      scrollTimeout = null;
      setActiveYear(findActiveYear());
    }, 50);
  }

  function bindScrollListener() {
    const nextContainer = getScrollContainer();
    if (nextContainer === currentScrollContainer) return;

    if (currentScrollContainer) {
      currentScrollContainer.removeEventListener("scroll", handleScroll);
    }

    nextContainer.addEventListener("scroll", handleScroll, { passive: true });
    currentScrollContainer = nextContainer;
  }

  let resizeTimeout;
  window.addEventListener("resize", () => {
    if (resizeTimeout) clearTimeout(resizeTimeout);
    resizeTimeout = setTimeout(() => {
      bindScrollListener();
      setActiveYear(findActiveYear());
    }, 200);
  });

  bindScrollListener();

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
  // Hash handling
  // ==========================================================================

  function getYearFromHash() {
    const hash = window.location.hash.slice(1);
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

  window.addEventListener("hashchange", () => {
    const year = getYearFromHash();
    if (year) {
      goToYear(year);
    }
  });

  // ==========================================================================
  // Tag filtering
  // ==========================================================================

  const tagFilter = document.getElementById("tag-filter");
  const tagFilterLabel = tagFilter?.querySelector(".tag-filter-label");
  const tagFilterClear = tagFilter?.querySelector(".tag-filter-clear");

  // Track current tag filter state for navigation decisions
  let currentTagFilter = null;

  // Unified year-start and first-visible class management
  function updateGroupingClasses() {
    const visiblePosts = getVisiblePosts();

    // Clear all grouping classes first
    posts.forEach((post) => {
      post.classList.remove("timeline-post--year-start", "timeline-post--first-visible");
    });

    // Apply classes to visible posts
    let lastVisibleYear = null;
    visiblePosts.forEach((post, index) => {
      const isYearStart = index > 0 && post.dataset.year !== lastVisibleYear;
      post.classList.toggle("timeline-post--year-start", isYearStart);
      post.classList.toggle("timeline-post--first-visible", index === 0);
      lastVisibleYear = post.dataset.year;
    });
  }

  function getTagFromUrl() {
    const params = new URLSearchParams(window.location.search);
    return params.get("tag");
  }

  function updateTagFilterUI(tag) {
    if (!tagFilter) return;

    if (tag) {
      tagFilter.style.display = "inline-flex";
      if (tagFilterLabel) tagFilterLabel.textContent = `#${tag}`;
    } else {
      tagFilter.style.display = "none";
    }
  }

  function clearTagFilter() {
    const url = new URL(window.location);
    url.searchParams.delete("tag");
    history.pushState(null, "", url);
    filterByTag(null);
  }

  function filterByTag(tag) {
    currentTagFilter = tag && tag.length > 0 ? tag : null;
    const hasFilter = !!currentTagFilter;
    const visibleYears = new Set();

    updateTagFilterUI(tag);

    // Show/hide posts based on tag
    posts.forEach((post) => {
      const postTags = (post.dataset.tags || "").split(",").filter(Boolean);
      const matches = !hasFilter || postTags.includes(tag);
      setPostVisible(post, matches);
      if (matches && post.dataset.year) {
        visibleYears.add(post.dataset.year);
      }
    });

    // Update year visibility
    yearLinks.forEach((link) => {
      const year = link.dataset.year;
      link.hidden = hasFilter && !visibleYears.has(year);
    });

    // Update grouping classes and active year
    updateGroupingClasses();
    setActiveYear(findActiveYear());
  }

  // Clear filter button
  if (tagFilterClear) {
    tagFilterClear.addEventListener("click", clearTagFilter);
  }

  // Listen for popstate (back/forward navigation)
  window.addEventListener("popstate", () => {
    filterByTag(getTagFromUrl());
  });

  // ==========================================================================
  // Initialization - Apply tag filter first, then hash navigation
  // ==========================================================================

  const initialTag = getTagFromUrl();
  if (initialTag) {
    filterByTag(initialTag);
  }
  // No else needed: inline script in template already set year-start classes

  const initialYear = getYearFromHash();
  if (initialYear) {
    // Use requestAnimationFrame to ensure layout is complete
    requestAnimationFrame(() => goToYear(initialYear));
  } else {
    setActiveYear(findActiveYear());
  }
})();
