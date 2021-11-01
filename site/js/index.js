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

  schemeMedia.addEventListener('change', () => { setThemeUIState(); });

  document.body.className = '';
  setThemeUIState();
  document.body.className = 'transitions';

  themeBtn.addEventListener('click', () => { setThemeExplicitly(); });
});
