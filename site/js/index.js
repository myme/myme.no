document.addEventListener('DOMContentLoaded', () => {
  'use strict';

  const root = document.querySelector(':root');
  const schemeMedia = window.matchMedia('(prefers-color-scheme: dark)');
  const themeBtn = document.querySelector('button.theme');
  const themeIcon = themeBtn.querySelector('i');

  themeBtn.classList.remove('hidden');

  function setThemeUIState() {
    const themeState = localStorage.getItem('theme') || 'adjust';
    const icon = {
      light: 'sun',
      dark: 'moon',
      adjust: 'adjust',
    }[themeState];
    themeIcon.className = `fas fa-${icon}`;
    if (themeState === 'adjust') {
      delete root.dataset.scheme;
    } else {
      root.dataset.scheme = themeState;
    }
  }

  function setThemeExplicitly() {
    const themeOrder = schemeMedia.matches
      ? ['adjust', 'light', 'dark']
      : ['adjust', 'dark', 'light'];

    const storedTheme = localStorage.getItem('theme');
    const themeState = themeOrder.includes(storedTheme) ? storedTheme : 'adjust';
    const nextState = (() => {
      let current;
      do {
        current = themeOrder.shift();
        themeOrder.push(current);
      } while (current !== themeState);
      return themeOrder.shift();
    })();

    localStorage.setItem('theme', nextState);
    setThemeUIState();
  }

  const sleep = (timeout) => new Promise(resolve => {
    setTimeout(resolve, timeout);
  });

  /**
   * Invoke `callback` with transitions disabled.
   *
   * Attempts to compute the time a transition on <body> will take and
   * re-enables transitions after that time. It does so by hackisly inspecting
   * the transitionDelays set on the body element.
   */
  async function withoutTransitions(callback) {
    try {
      const duration = Math.max(
        ...getComputedStyle(document.body)
          .transitionDuration
          .split(',')
          .map((x) => parseFloat(x) * (x.match(/s$/) ? 1000 : 1)));
      document.body.className = '';
      callback();
      await sleep(duration);
    } finally {
      document.body.className = 'transitions';
    }
  }

  schemeMedia.addEventListener('change', () => { setThemeUIState(); });

  withoutTransitions(setThemeUIState);

  themeBtn.addEventListener('click', () => { setThemeExplicitly(); });
});
