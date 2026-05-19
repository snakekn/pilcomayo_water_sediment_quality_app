// i18n.js — Language toggle for RiverRemedy Shiny app
// Handles instant DOM-swap for static UI strings and notifies the server for reactive outputs.

var I18N = {};

/**
 * Switch the app language.
 * @param {string} lang - "en" or "es"
 */
function setLang(lang) {
  // Update toggle button active state
  document.querySelectorAll('.lang-btn').forEach(function(btn) {
    btn.classList.toggle('lang-active', btn.id === 'lang-btn-' + lang);
  });

  // Swap text content of all elements tagged with data-i18n
  document.querySelectorAll('[data-i18n]').forEach(function(el) {
    var key = el.getAttribute('data-i18n');
    if (I18N[lang] && I18N[lang][key] !== undefined) {
      el.textContent = I18N[lang][key];
    }
  });

  // Notify the Shiny server so reactive outputs (renderUI, notifications) update
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue('lang', lang, { priority: 'event' });
  }
}

// Receive the full translation dictionary from the server (sent once on session start)
if (window.Shiny) {
  Shiny.addCustomMessageHandler('i18n_dict', function(dict) {
    I18N = dict;
    // Apply current button state (default EN on fresh session)
    var activeLang = document.querySelector('.lang-btn.lang-active');
    var lang = activeLang ? activeLang.id.replace('lang-btn-', '') : 'en';
    setLang(lang);
  });
}
