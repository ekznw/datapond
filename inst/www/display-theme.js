(function () {
  'use strict';

  var storageKey = 'datapond.display.theme';
  var mediaQuery = window.matchMedia ?
    window.matchMedia('(prefers-color-scheme: dark)') : null;
  var mode = 'system';

  try {
    var storedMode = window.localStorage.getItem(storageKey);
    if (storedMode === 'light' || storedMode === 'dark' || storedMode === 'bordo') {
      mode = storedMode;
    }
  } catch (error) {
    // The selector still works when browser storage is unavailable.
  }

  function resolvedTheme() {
    if (mode === 'dark' || mode === 'bordo') return 'dark';
    if (mode === 'light') return 'light';
    return mediaQuery && mediaQuery.matches ? 'dark' : 'light';
  }

  function applyTheme() {
    document.documentElement.setAttribute('data-datapond-theme', resolvedTheme());
    document.documentElement.setAttribute(
      'data-datapond-palette',
      mode === 'bordo' ? 'bordo' : 'earth'
    );
    var control = document.getElementById('display_theme_mode');
    if (control) control.value = mode;
  }

  // Apply before the page is painted, including on refresh.
  applyTheme();
  document.addEventListener('DOMContentLoaded', applyTheme);
  document.addEventListener('change', function (event) {
    if (event.target.id !== 'display_theme_mode') return;
    mode = event.target.value;
    if (mode !== 'light' && mode !== 'dark' && mode !== 'bordo') mode = 'system';
    applyTheme();
    try {
      if (mode === 'system') {
        window.localStorage.removeItem(storageKey);
      } else {
        window.localStorage.setItem(storageKey, mode);
      }
    } catch (error) {
      // Storage is optional; do not interrupt the current session.
    }
  });

  function handleSystemThemeChange() {
    if (mode === 'system') applyTheme();
  }
  if (mediaQuery) {
    if (mediaQuery.addEventListener) {
      mediaQuery.addEventListener('change', handleSystemThemeChange);
    } else if (mediaQuery.addListener) {
      mediaQuery.addListener(handleSystemThemeChange);
    }
  }

  window.addEventListener('storage', function (event) {
    if (event.key !== storageKey && event.key !== null) return;
    mode = event.newValue === 'dark' || event.newValue === 'light' || event.newValue === 'bordo' ?
      event.newValue : 'system';
    applyTheme();
  });
})();

(function () {
  'use strict';

  var storageKey = 'datapond.interface.sound';
  var enabled = true;
  var pendingStartup = false;
  var fadeTimer = null;
  var fadeDelay = null;

  try {
    enabled = window.localStorage.getItem(storageKey) !== 'off';
  } catch (error) {
    // Browser storage is optional; sound can still be controlled this session.
  }

  function audioElement() {
    return document.getElementById('datapond_finfoot_audio');
  }

  function clearTimers() {
    if (fadeDelay !== null) window.clearTimeout(fadeDelay);
    if (fadeTimer !== null) window.clearInterval(fadeTimer);
    fadeDelay = null;
    fadeTimer = null;
  }

  function stopCue(audio) {
    clearTimers();
    if (!audio) return;
    audio.pause();
    audio.volume = 0.18;
    try {
      audio.currentTime = 0;
    } catch (error) {
      // Metadata may not be ready yet; play() will begin at the start anyway.
    }
  }

  function fadeAndStop(audio, fadeDuration) {
    var startedAt = Date.now();
    var startingVolume = audio.volume;

    fadeTimer = window.setInterval(function () {
      var progress = Math.min(1, (Date.now() - startedAt) / fadeDuration);
      audio.volume = Math.max(0, startingVolume * (1 - progress));
      if (progress >= 1) stopCue(audio);
    }, 50);
  }

  function scheduleFade(audio, message) {
    var isStartup = message && message.reason === 'startup';
    var holdDuration = isStartup ? 9000 : 1900;
    var fadeDuration = isStartup ? 2000 : 1100;

    clearTimers();
    fadeDelay = window.setTimeout(function () {
      fadeDelay = null;
      fadeAndStop(audio, fadeDuration);
    }, holdDuration);
  }

  function playCue(message) {
    if (!enabled) return;

    var audio = audioElement();
    if (!audio) return;

    stopCue(audio);
    audio.volume = 0.18;
    var playback = audio.play();

    if (playback && typeof playback.then === 'function') {
      playback.then(function () {
        pendingStartup = false;
        scheduleFade(audio, message);
      }).catch(function () {
        if (message && message.reason === 'startup') pendingStartup = true;
      });
    } else {
      pendingStartup = false;
      scheduleFade(audio, message);
    }
  }

  function applyStoredPreference() {
    var control = document.getElementById('enable_finfoot_sound');
    if (control) control.checked = enabled;
  }

  applyStoredPreference();
  document.addEventListener('DOMContentLoaded', applyStoredPreference);
  document.addEventListener('shiny:bound', function (event) {
    if (event.target && event.target.id === 'enable_finfoot_sound') {
      applyStoredPreference();
    }
  });

  document.addEventListener('change', function (event) {
    if (event.target.id !== 'enable_finfoot_sound') return;
    enabled = Boolean(event.target.checked);
    pendingStartup = false;
    try {
      window.localStorage.setItem(storageKey, enabled ? 'on' : 'off');
    } catch (error) {
      // Keep the in-memory preference when browser storage is unavailable.
    }
    if (!enabled) stopCue(audioElement());
  });

  function retryStartupCue(event) {
    if (event.target && event.target.id === 'enable_finfoot_sound') return;
    if (pendingStartup && enabled) playCue({ reason: 'startup' });
  }

  // Browsers commonly block startup audio. The pending cue is retried inside
  // the first genuine user gesture, when playback is permitted.
  document.addEventListener('pointerdown', retryStartupCue, true);
  document.addEventListener('keydown', retryStartupCue, true);

  Shiny.addCustomMessageHandler('datapond-play-finfoot', playCue);
})();
