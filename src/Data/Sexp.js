'use strict';

exports._replaceDoubleQuotes = function(s) {
  return s.replace(/"/g, '\\"');
};
