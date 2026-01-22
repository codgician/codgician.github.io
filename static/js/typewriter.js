/**
 * Typewriter effect module
 * Usage: Add data-typewriter="text1|text2|text3" to any element
 * 
 * Behavior:
 * - First phrase is always the first one defined
 * - Subsequent phrases cycle randomly (shuffled)
 */
(function () {
  "use strict";

  var DEFAULTS = {
    typeSpeed: 80,
    deleteSpeed: 40,
    pauseDelay: 2000,
  };

  /**
   * Fisher-Yates shuffle algorithm
   */
  function shuffle(array) {
    var result = array.slice();
    for (var i = result.length - 1; i > 0; i--) {
      var j = Math.floor(Math.random() * (i + 1));
      var temp = result[i];
      result[i] = result[j];
      result[j] = temp;
    }
    return result;
  }

  function init(element) {
    var texts = (element.dataset.typewriter || "").split("|").filter(Boolean);
    if (!texts.length) return;

    var speed = parseInt(element.dataset.typeSpeed, 10) || DEFAULTS.typeSpeed;
    var deleteSpeed = parseInt(element.dataset.deleteSpeed, 10) || DEFAULTS.deleteSpeed;
    var pause = parseInt(element.dataset.pauseDelay, 10) || DEFAULTS.pauseDelay;
    var loop = texts.length > 1;

    element.textContent = "";
    element.classList.add("typewriter");

    var firstPhrase = texts[0];
    var otherPhrases = texts.slice(1);
    var phraseQueue = [firstPhrase].concat(shuffle(otherPhrases));

    var queueIndex = 0;
    var charIndex = 0;
    var isDeleting = false;

    function tick() {
      var current = phraseQueue[queueIndex];

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
        queueIndex++;
        
        if (queueIndex >= phraseQueue.length) {
          phraseQueue = [firstPhrase].concat(shuffle(otherPhrases));
          queueIndex = 0;
        }
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
