"use strict";

exports.initTooltip = function() {
  $('#gen-link.tooltipped').tooltip({delay: 50, position: "bottom", tooltip: "Generate a bookmarkable/shareable link"});
}

exports.initModals = function() {
  $('.modal').modal();
}

exports.clipboard = function(text) {
  return function() {
    return copyTextToClipboard(text);
  }
}

exports.showToast = function(text) {
  return function() {
    Materialize.toast(text, 2000);
  }
}

function copyTextToClipboard(text) {
  var textArea = document.createElement("textarea");

  // Place in top-left corner of screen regardless of scroll position.
  textArea.style.position = 'fixed';
  textArea.style.top = 0;
  textArea.style.left = 0;

  // Ensure it has a small width and height. Setting to 1px / 1em
  // doesn't work as this gives a negative w/h on some browsers.
  textArea.style.width = '2em';
  textArea.style.height = '2em';

  // We don't need padding, reducing the size if it does flash render.
  textArea.style.padding = 0;

  // Clean up any borders.
  textArea.style.border = 'none';
  textArea.style.outline = 'none';
  textArea.style.boxShadow = 'none';

  // Avoid flash of white box if rendered for any reason.
  textArea.style.background = 'transparent';

  textArea.value = text;
  document.body.appendChild(textArea);
  textArea.select();

  var successful;
  try {
    successful = document.execCommand('copy');
  } catch (err) {
    successful = false;
  }

  document.body.removeChild(textArea);
  return successful;
}
