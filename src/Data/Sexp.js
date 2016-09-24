'use strict';

exports._escape = function(s) {
  return s.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
};

exports._fromString = function(ctors) {
  return function(s) {
    var i = 0;

    var sexp = function() {
      for (;;) {
        switch (s.charAt(i)) {
          case ' ':
          case '\t':
          case '\r':
          case '\n':
            ++i;
            continue;
          case '(':
            ++i;
            return list();
          case '"':
            ++i;
            return string();
          default:
            return ctors.nothing;
        }
      }
    };

    var list = function() {
      var elements = ctors.nil;
      for (;;) {
        if (s.charAt(i) === ')') {
          ++i;
          break;
        }
        var element = sexp();
        if ('value0' in element) {
          elements = ctors.cons(element.value0)(elements);
        } else {
          return ctors.nothing;
        }
      }
      return ctors.just(ctors.list(ctors.reverse(elements)));
    };

    var string = function() {
      var value = '';
      loop: for (;;) {
        var c = s.charAt(i);
        switch (c) {
          case '\\':
            value += s.charAt(i + 1);
            i += 2;
            break;
          case '"':
            ++i;
            break loop;
          case '':
            return ctors.nothing;
          default:
            ++i;
            value += c;
        }
      }
      return ctors.just(ctors.atom(value));
    };

    return sexp();
  };
};
