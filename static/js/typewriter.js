/**
 * Typewriter effect module
 * Usage: Add data-typewriter="text1|text2|text3" to any element
 */
(function () {
  "use strict";

  var DEFAULTS = {
    typeSpeed: 80,
    deleteSpeed: 40,
    pauseDelay: 2000,
  };

  function init(element) {
    var texts = (element.dataset.typewriter || "").split("|").filter(Boolean);
    if (!texts.length) return;

    var speed = parseInt(element.dataset.typeSpeed, 10) || DEFAULTS.typeSpeed;
    var deleteSpeed = parseInt(element.dataset.deleteSpeed, 10) || DEFAULTS.deleteSpeed;
    var pause = parseInt(element.dataset.pauseDelay, 10) || DEFAULTS.pauseDelay;
    var loop = texts.length > 1;

    element.textContent = "";
    element.classList.add("typewriter");

    var textIndex = 0;
    var charIndex = 0;
    var isDeleting = false;

    function tick() {
      var current = texts[textIndex];

      if (isDeleting) {
        charIndex--;
      } else {
        charIndex++;
      }
      element.textContent = current.substring(0, charIndex);

      var delay = isDeleting ? deleteSpeed : speed;

      if (!isDeleting && charIndex === current.length) {
        if (!loop) return;
        delay = pause;
        isDeleting = true;
      } else if (isDeleting && charIndex === 0) {
        isDeleting = false;
        textIndex = (textIndex + 1) % texts.length;
      }

      setTimeout(tick, delay);
    }

    setTimeout(tick, 500);
  }

  function onReady(fn) {
    if (document.readyState !== "loading") fn();
    else document.addEventListener("DOMContentLoaded", fn);
  }

  onReady(function () {
    document.querySelectorAll("[data-typewriter]").forEach(init);
  });
})();
