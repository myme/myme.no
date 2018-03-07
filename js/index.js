(function() {
  'use strict';

  function init() {
    if (location.pathname === '/') {
      var article = document.querySelector('article');
      var nextHeader = document.querySelector('article + h1');
      var readMore = document.createElement('a');

      article.parentElement.insertBefore(readMore, nextHeader);

      article.classList.add('collapsed');

      readMore.title = 'Click to expand';
      readMore.href = '#';
      readMore.innerHTML = '<i class="fas fa-chevron-down"></i>';
      readMore.classList.add('expand');
      readMore.addEventListener('click', function(event) {
        event.preventDefault();
        article.classList.remove('collapsed');
      });
    }
  }

  document.addEventListener('DOMContentLoaded', function() {
    init();
  });
})();
