'use strict';

exports._escape = function(s) {
  return s.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
};
