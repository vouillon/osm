// This program was compiled from OCaml by js_of_ocaml 1.3
function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlWrappedString (msg));
}
function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[4], msg);
}
function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}
function caml_str_repeat(n, s) {
  if (!n) { return ""; }
  if (n & 1) { return caml_str_repeat(n - 1, s) + s; }
  var r = caml_str_repeat(n >> 1, s);
  return r + r;
}
function MlString(param) {
  if (param != null) {
    this.bytes = this.fullBytes = param;
    this.last = this.len = param.length;
  }
}
MlString.prototype = {
  string:null,
  bytes:null,
  fullBytes:null,
  array:null,
  len:null,
  last:0,
  toJsString:function() {
    return this.string = decodeURIComponent (escape(this.getFullBytes()));
  },
  toBytes:function() {
    if (this.string != null)
      var b = unescape (encodeURIComponent (this.string));
    else {
      var b = "", a = this.array, l = a.length;
      for (var i = 0; i < l; i ++) b += String.fromCharCode (a[i]);
    }
    this.bytes = this.fullBytes = b;
    this.last = this.len = b.length;
    return b;
  },
  getBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return b;
  },
  getFullBytes:function() {
    var b = this.fullBytes;
    if (b !== null) return b;
    b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    this.fullBytes = b;
    return b;
  },
  toArray:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    var a = [], l = this.last;
    for (var i = 0; i < l; i++) a[i] = b.charCodeAt(i);
    for (l = this.len; i < l; i++) a[i] = 0;
    this.string = this.bytes = this.fullBytes = null;
    this.last = this.len;
    this.array = a;
    return a;
  },
  getArray:function() {
    var a = this.array;
    if (!a) a = this.toArray();
    return a;
  },
  getLen:function() {
    var len = this.len;
    if (len !== null) return len;
    this.toBytes();
    return this.len;
  },
  toString:function() { var s = this.string; return s?s:this.toJsString(); },
  valueOf:function() { var s = this.string; return s?s:this.toJsString(); },
  blitToArray:function(i1, a2, i2, l) {
    var a1 = this.array;
    if (a1) {
      if (i2 <= i1) {
        for (var i = 0; i < l; i++) a2[i2 + i] = a1[i1 + i];
      } else {
        for (var i = l - 1; i >= 0; i--) a2[i2 + i] = a1[i1 + i];
      }
    } else {
      var b = this.bytes;
      if (b == null) b = this.toBytes();
      var l1 = this.last - i1;
      if (l <= l1)
        for (var i = 0; i < l; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
      else {
        for (var i = 0; i < l1; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
        for (; i < l; i++) a2 [i2 + i] = 0;
      }
    }
  },
  get:function (i) {
    var a = this.array;
    if (a) return a[i];
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return (i<this.last)?b.charCodeAt(i):0;
  },
  safeGet:function (i) {
    if (!this.len) this.toBytes();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    return this.get(i);
  },
  set:function (i, c) {
    var a = this.array;
    if (!a) {
      if (this.last == i) {
        this.bytes += String.fromCharCode (c & 0xff);
        this.last ++;
        return 0;
      }
      a = this.toArray();
    } else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    a[i] = c & 0xff;
    return 0;
  },
  safeSet:function (i, c) {
    if (this.len == null) this.toBytes ();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    this.set(i, c);
  },
  fill:function (ofs, len, c) {
    if (ofs >= this.last && this.last && c == 0) return;
    var a = this.array;
    if (!a) a = this.toArray();
    else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    var l = ofs + len;
    for (var i = ofs; i < l; i++) a[i] = c;
  },
  compare:function (s2) {
    if (this.string != null && s2.string != null) {
      if (this.string < s2.string) return -1;
      if (this.string > s2.string) return 1;
      return 0;
    }
    var b1 = this.getFullBytes ();
    var b2 = s2.getFullBytes ();
    if (b1 < b2) return -1;
    if (b1 > b2) return 1;
    return 0;
  },
  equal:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string == s2.string;
    return this.getFullBytes () == s2.getFullBytes ();
  },
  lessThan:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string < s2.string;
    return this.getFullBytes () < s2.getFullBytes ();
  },
  lessEqual:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string <= s2.string;
    return this.getFullBytes () <= s2.getFullBytes ();
  }
}
function MlWrappedString (s) { this.string = s; }
MlWrappedString.prototype = new MlString();
function MlMakeString (l) { this.bytes = ""; this.len = l; }
MlMakeString.prototype = new MlString ();
function caml_array_get (array, index) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  return array[index+1];
}
function caml_array_set (array, index, newval) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}
function caml_blit_string(s1, i1, s2, i2, len) {
  if (len === 0) return;
  if (i2 === s2.last && s2.bytes != null) {
    var b = s1.bytes;
    if (b == null) b = s1.toBytes ();
    if (i1 > 0 || s1.last > len) b = b.slice(i1, i1 + len);
    s2.bytes += b;
    s2.last += b.length;
    return;
  }
  var a = s2.array;
  if (!a) a = s2.toArray(); else { s2.bytes = s2.string = null; }
  s1.blitToArray (i1, a, i2, len);
}
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  var n = f.length;
  var d = n - args.length;
  if (d == 0)
    return f.apply(null, args);
  else if (d < 0)
    return caml_call_gen(f.apply(null, args.slice(0,n)), args.slice(n));
  else
    return function (x){ return caml_call_gen(f, args.concat([x])); };
}
function caml_classify_float (x) {
  if (isFinite (x)) {
    if (Math.abs(x) >= 2.2250738585072014e-308) return 0;
    if (x != 0) return 1;
    return 2;
  }
  return isNaN(x)?4:3;
}
function caml_create_string(len) {
  if (len < 0) caml_invalid_argument("String.create");
  return new MlMakeString(len);
}
function caml_fill_string(s, i, l, c) { s.fill (i, l, c); }
function caml_parse_format (fmt) {
  fmt = fmt.toString ();
  var len = fmt.length;
  if (len > 31) caml_invalid_argument("format_int: format too long");
  var f =
    { justify:'+', signstyle:'-', filler:' ', alternate:false,
      base:0, signedconv:false, width:0, uppercase:false,
      sign:1, prec:-1, conv:'f' };
  for (var i = 0; i < len; i++) {
    var c = fmt.charAt(i);
    switch (c) {
    case '-':
      f.justify = '-'; break;
    case '+': case ' ':
      f.signstyle = c; break;
    case '0':
      f.filler = '0'; break;
    case '#':
      f.alternate = true; break;
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      f.width = 0;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.width = f.width * 10 + c; i++
      }
      i--;
     break;
    case '.':
      f.prec = 0;
      i++;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.prec = f.prec * 10 + c; i++
      }
      i--;
    case 'd': case 'i':
      f.signedconv = true; /* fallthrough */
    case 'u':
      f.base = 10; break;
    case 'x':
      f.base = 16; break;
    case 'X':
      f.base = 16; f.uppercase = true; break;
    case 'o':
      f.base = 8; break;
    case 'e': case 'f': case 'g':
      f.signedconv = true; f.conv = c; break;
    case 'E': case 'F': case 'G':
      f.signedconv = true; f.uppercase = true;
      f.conv = c.toLowerCase (); break;
    }
  }
  return f;
}
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  if (f.signedconv && (f.sign < 0 || f.signstyle != '-')) len++;
  if (f.alternate) {
    if (f.base == 8) len += 1;
    if (f.base == 16) len += 2;
  }
  var buffer = "";
  if (f.justify == '+' && f.filler == ' ')
    for (var i = len; i < f.width; i++) buffer += ' ';
  if (f.signedconv) {
    if (f.sign < 0) buffer += '-';
    else if (f.signstyle != '-') buffer += f.signstyle;
  }
  if (f.alternate && f.base == 8) buffer += '0';
  if (f.alternate && f.base == 16) buffer += "0x";
  if (f.justify == '+' && f.filler == '0')
    for (var i = len; i < f.width; i++) buffer += '0';
  buffer += rawbuffer;
  if (f.justify == '-')
    for (var i = len; i < f.width; i++) buffer += ' ';
  return new MlWrappedString (buffer);
}
function caml_format_float (fmt, x) {
  var s, f = caml_parse_format(fmt);
  var prec = (f.prec < 0)?6:f.prec;
  if (x < 0) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(prec);
      var i = s.length;
      if (s.charAt(i - 3) == 'e')
        s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
      break;
    case 'f':
      s = x.toFixed(prec); break;
    case 'g':
      prec = prec?prec:1;
      s = x.toExponential(prec - 1);
      var j = s.indexOf('e');
      var exp = +s.slice(j + 1);
      if (exp < -4 || x.toFixed(0).length > prec) {
        var i = j - 1; while (s.charAt(i) == '0') i--;
        if (s.charAt(i) == '.') i--;
        s = s.slice(0, i + 1) + s.slice(j);
        i = s.length;
        if (s.charAt(i - 3) == 'e')
          s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
        break;
      } else {
        var p = prec;
        if (exp < 0) { p -= exp + 1; s = x.toFixed(p); }
        else while (s = x.toFixed(p), s.length > prec + 1) p--;
        if (p) {
          var i = s.length - 1; while (s.charAt(i) == '0') i--;
          if (s.charAt(i) == '.') i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
    }
  return caml_finish_formatting(f, s);
}
function caml_format_int(fmt, i) {
  if (fmt.toString() == "%d") return new MlWrappedString(""+i);
  var f = caml_parse_format(fmt);
  if (i < 0) { if (f.signedconv) { f.sign = -1; i = -i; } else i >>>= 0; }
  var s = i.toString(f.base);
  if (f.prec >= 0) {
    f.filler = ' ';
    var n = f.prec - s.length;
    if (n > 0) s = caml_str_repeat (n, '0') + s;
  }
  return caml_finish_formatting(f, s);
}
function caml_int64_compare(x,y) {
  var x3 = x[3] << 16;
  var y3 = y[3] << 16;
  if (x3 > y3) return 1;
  if (x3 < y3) return -1;
  if (x[2] > y[2]) return 1;
  if (x[2] < y[2]) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  return 0;
}
function caml_int_compare (a, b) {
  if (a < b) return (-1); if (a == b) return 0; return 1;
}
function caml_compare_val (a, b, total) {
  var stack = [];
  for(;;) {
    if (!(total && a === b)) {
      if (a instanceof MlString) {
        if (b instanceof MlString) {
            if (a != b) {
		var x = a.compare(b);
		if (x != 0) return x;
	    }
        } else
          return 1;
      } else if (a instanceof Array && a[0] === (a[0]|0)) {
        var ta = a[0];
        if (ta === 250) {
          a = a[1];
          continue;
        } else if (b instanceof Array && b[0] === (b[0]|0)) {
          var tb = b[0];
          if (tb === 250) {
            b = b[1];
            continue;
          } else if (ta != tb) {
            return (ta < tb)?-1:1;
          } else {
            switch (ta) {
            case 248: {
		var x = caml_int_compare(a[2], b[2]);
		if (x != 0) return x;
		break;
	    }
            case 255: {
		var x = caml_int64_compare(a, b);
		if (x != 0) return x;
		break;
	    }
            default:
              if (a.length != b.length) return (a.length < b.length)?-1:1;
              if (a.length > 1) stack.push(a, b, 1);
            }
          }
        } else
          return 1;
      } else if (b instanceof MlString ||
                 (b instanceof Array && b[0] === (b[0]|0))) {
        return -1;
      } else {
        if (a < b) return -1;
        if (a > b) return 1;
        if (total && a != b) {
          if (a == a) return 1;
          if (b == b) return -1;
        }
      }
    }
    if (stack.length == 0) return 0;
    var i = stack.pop();
    b = stack.pop();
    a = stack.pop();
    if (i + 1 < a.length) stack.push(a, b, i + 1);
    a = a[i];
    b = b[i];
  }
}
function caml_compare (a, b) { return caml_compare_val (a, b, true); }
function caml_greaterequal (x, y) { return +(caml_compare(x,y,false) >= 0); }
function caml_int64_is_negative(x) {
  return (x[3] << 16) < 0;
}
function caml_int64_neg (x) {
  var y1 = - x[1];
  var y2 = - x[2] + (y1 >> 24);
  var y3 = - x[3] + (y2 >> 24);
  return [255, y1 & 0xffffff, y2 & 0xffffff, y3 & 0xffff];
}
function caml_int64_of_int32 (x) {
  return [255, x & 0xffffff, (x >> 24) & 0xffffff, (x >> 31) & 0xffff]
}
function caml_int64_ucompare(x,y) {
  if (x[3] > y[3]) return 1;
  if (x[3] < y[3]) return -1;
  if (x[2] > y[2]) return 1;
  if (x[2] < y[2]) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  return 0;
}
function caml_int64_lsl1 (x) {
  x[3] = (x[3] << 1) | (x[2] >> 23);
  x[2] = ((x[2] << 1) | (x[1] >> 23)) & 0xffffff;
  x[1] = (x[1] << 1) & 0xffffff;
}
function caml_int64_lsr1 (x) {
  x[1] = ((x[1] >>> 1) | (x[2] << 23)) & 0xffffff;
  x[2] = ((x[2] >>> 1) | (x[3] << 23)) & 0xffffff;
  x[3] = x[3] >>> 1;
}
function caml_int64_sub (x, y) {
  var z1 = x[1] - y[1];
  var z2 = x[2] - y[2] + (z1 >> 24);
  var z3 = x[3] - y[3] + (z2 >> 24);
  return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}
function caml_int64_udivmod (x, y) {
  var offset = 0;
  var modulus = x.slice ();
  var divisor = y.slice ();
  var quotient = [255, 0, 0, 0];
  while (caml_int64_ucompare (modulus, divisor) > 0) {
    offset++;
    caml_int64_lsl1 (divisor);
  }
  while (offset >= 0) {
    offset --;
    caml_int64_lsl1 (quotient);
    if (caml_int64_ucompare (modulus, divisor) >= 0) {
      quotient[1] ++;
      modulus = caml_int64_sub (modulus, divisor);
    }
    caml_int64_lsr1 (divisor);
  }
  return [0,quotient, modulus];
}
function caml_int64_to_int32 (x) {
  return x[1] | (x[2] << 24);
}
function caml_int64_is_zero(x) {
  return (x[3]|x[2]|x[1]) == 0;
}
function caml_int64_format (fmt, x) {
  var f = caml_parse_format(fmt);
  if (f.signedconv && caml_int64_is_negative(x)) {
    f.sign = -1; x = caml_int64_neg(x);
  }
  var buffer = "";
  var wbase = caml_int64_of_int32(f.base);
  var cvtbl = "0123456789abcdef";
  do {
    var p = caml_int64_udivmod(x, wbase);
    x = p[1];
    buffer = cvtbl.charAt(caml_int64_to_int32(p[2])) + buffer;
  } while (! caml_int64_is_zero(x));
  if (f.prec >= 0) {
    f.filler = ' ';
    var n = f.prec - buffer.length;
    if (n > 0) buffer = caml_str_repeat (n, '0') + buffer;
  }
  return caml_finish_formatting(f, buffer);
}
function caml_parse_sign_and_base (s) {
  var i = 0, base = 10, sign = s.get(0) == 45?(i++,-1):1;
  if (s.get(i) == 48)
    switch (s.get(i + 1)) {
    case 120: case 88: base = 16; i += 2; break;
    case 111: case 79: base =  8; i += 2; break;
    case  98: case 66: base =  2; i += 2; break;
    }
  return [i, sign, base];
}
function caml_parse_digit(c) {
  if (c >= 48 && c <= 57)  return c - 48;
  if (c >= 65 && c <= 90)  return c - 55;
  if (c >= 97 && c <= 122) return c - 87;
  return -1;
}
var caml_global_data = [0];
function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}
function caml_int_of_string (s) {
  var r = caml_parse_sign_and_base (s);
  var i = r[0], sign = r[1], base = r[2];
  var threshold = -1 >>> 0;
  var c = s.get(i);
  var d = caml_parse_digit(c);
  if (d < 0 || d >= base) caml_failwith("int_of_string");
  var res = d;
  for (;;) {
    i++;
    c = s.get(i);
    if (c == 95) continue;
    d = caml_parse_digit(c);
    if (d < 0 || d >= base) break;
    res = base * res + d;
    if (res > threshold) caml_failwith("int_of_string");
  }
  if (i != s.getLen()) caml_failwith("int_of_string");
  res = sign * res;
  if ((res | 0) != res) caml_failwith("int_of_string");
  return res;
}
function caml_is_printable(c) { return +(c > 31 && c < 127); }
function caml_js_from_array(a) { return a.slice(1); }
function caml_js_get_console () {
  var c = this.console?this.console:{};
  var m = ["log", "debug", "info", "warn", "error", "assert", "dir", "dirxml",
           "trace", "group", "groupCollapsed", "groupEnd", "time", "timeEnd"];
  function f () {}
  for (var i = 0; i < m.length; i++) if (!c[m[i]]) c[m[i]]=f;
  return c;
}
function caml_js_pure_expr (f) { return f(); }
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[undefined];
    return caml_call_gen(f, args);
  }
}
function caml_lessequal (x, y) { return +(caml_compare(x,y,false) <= 0); }
function caml_lessthan (x, y) { return +(caml_compare(x,y,false) < 0); }
function caml_make_vect (len, init) {
  var b = [0]; for (var i = 1; i <= len; i++) b[i] = init; return b;
}
function caml_ml_flush () { return 0; }
function caml_ml_open_descriptor_out () { return 0; }
function caml_ml_out_channels_list () { return 0; }
function caml_ml_output () { return 0; }
function caml_mul(x,y) {
  return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0;
}
function caml_register_global (n, v) { caml_global_data[n + 1] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
function caml_sys_get_config () {
  return [0, new MlWrappedString("Unix"), 32, 0];
}
(function()
   {function _hX_(_rM_,_rN_,_rO_,_rP_,_rQ_,_rR_,_rS_)
     {return _rM_.length==6
              ?_rM_(_rN_,_rO_,_rP_,_rQ_,_rR_,_rS_)
              :caml_call_gen(_rM_,[_rN_,_rO_,_rP_,_rQ_,_rR_,_rS_]);}
    function _lc_(_rG_,_rH_,_rI_,_rJ_,_rK_,_rL_)
     {return _rG_.length==5
              ?_rG_(_rH_,_rI_,_rJ_,_rK_,_rL_)
              :caml_call_gen(_rG_,[_rH_,_rI_,_rJ_,_rK_,_rL_]);}
    function _gZ_(_rB_,_rC_,_rD_,_rE_,_rF_)
     {return _rB_.length==4
              ?_rB_(_rC_,_rD_,_rE_,_rF_)
              :caml_call_gen(_rB_,[_rC_,_rD_,_rE_,_rF_]);}
    function _di_(_rx_,_ry_,_rz_,_rA_)
     {return _rx_.length==3
              ?_rx_(_ry_,_rz_,_rA_)
              :caml_call_gen(_rx_,[_ry_,_rz_,_rA_]);}
    function _dN_(_ru_,_rv_,_rw_)
     {return _ru_.length==2?_ru_(_rv_,_rw_):caml_call_gen(_ru_,[_rv_,_rw_]);}
    function _bd_(_rs_,_rt_)
     {return _rs_.length==1?_rs_(_rt_):caml_call_gen(_rs_,[_rt_]);}
    var
     _a_=[0,new MlString("Failure")],
     _b_=[0,new MlString("Invalid_argument")],
     _c_=[0,new MlString("Not_found")],
     _d_=[0,new MlString("Assert_failure")],
     _e_=new MlString("");
    caml_register_global(6,_c_);
    caml_register_global(5,[0,new MlString("Division_by_zero")]);
    caml_register_global(3,_b_);
    caml_register_global(2,_a_);
    var
     _aS_=new MlString("output"),
     _aR_=new MlString("%.12g"),
     _aQ_=new MlString("."),
     _aP_=new MlString("%d"),
     _aO_=new MlString("true"),
     _aN_=new MlString("false"),
     _aM_=new MlString("Pervasives.do_at_exit"),
     _aL_=new MlString("\\b"),
     _aK_=new MlString("\\t"),
     _aJ_=new MlString("\\n"),
     _aI_=new MlString("\\r"),
     _aH_=new MlString("\\\\"),
     _aG_=new MlString("\\'"),
     _aF_=new MlString(""),
     _aE_=new MlString("String.blit"),
     _aD_=new MlString("String.sub"),
     _aC_=new MlString("Buffer.add_substring"),
     _aB_=new MlString("Buffer.add: cannot grow buffer"),
     _aA_=new MlString(""),
     _az_=new MlString(""),
     _ay_=new MlString("\""),
     _ax_=new MlString("\""),
     _aw_=new MlString("'"),
     _av_=new MlString("'"),
     _au_=new MlString("."),
     _at_=new MlString("printf: bad positional specification (0)."),
     _as_=new MlString("%_"),
     _ar_=[0,new MlString("printf.ml"),144,8],
     _aq_=new MlString("''"),
     _ap_=new MlString("Printf: premature end of format string ``"),
     _ao_=new MlString("''"),
     _an_=new MlString(" in format string ``"),
     _am_=new MlString(", at char number "),
     _al_=new MlString("Printf: bad conversion %"),
     _ak_=new MlString("Sformat.index_of_int: negative argument "),
     _aj_=new MlString("bad box format"),
     _ai_=new MlString("bad box name ho"),
     _ah_=new MlString("bad tag name specification"),
     _ag_=new MlString("bad tag name specification"),
     _af_=new MlString(""),
     _ae_=new MlString(""),
     _ad_=new MlString("bad integer specification"),
     _ac_=new MlString("bad format"),
     _ab_=new MlString(" (%c)."),
     _aa_=new MlString("%c"),
     _$_=
      new
       MlString
       ("Format.fprintf: %s ``%s'', giving up at character number %d%s"),
     ___=[3,0,3],
     _Z_=new MlString("."),
     _Y_=new MlString(">"),
     _X_=new MlString("</"),
     _W_=new MlString(">"),
     _V_=new MlString("<"),
     _U_=new MlString("\n"),
     _T_=new MlString("Format.Empty_queue"),
     _S_=[0,new MlString("")],
     _R_=new MlString("canvas"),
     _Q_=new MlString("p"),
     _P_=new MlString("div"),
     _O_=new MlString("Dom_html.Canvas_not_available"),
     _N_=new MlString("%.1f fps - %.f ms"),
     _M_=[0,new MlString("lines.ml"),214,52],
     _L_=new MlString("OES_element_index_uint"),
     _K_=new MlString("scale"),
     _J_=new MlString("transform"),
     _I_=[254,2,0,0,0,2,0,-50,0,1],
     _H_=new MlString("vertices: %d"),
     _G_=new MlString("edges: %d"),
     _F_=[0,0],
     _E_=new MlString("p0"),
     _D_=[0,32],
     _C_=new MlString("p1"),
     _B_=[0,64],
     _A_=new MlString("p2"),
     _z_=[0,96],
     _y_=new MlString("p3"),
     _x_=new MlString("w"),
     _w_=new MlString("edge"),
     _v_=new MlString("side"),
     _u_=new MlString("color"),
     _t_=new MlString("style1"),
     _s_=[0,16],
     _r_=new MlString("style2"),
     _q_=new MlString("experimental-webgl"),
     _p_=new MlString("elements: %d"),
     _o_=new MlString("absolute"),
     _n_=new MlString("20px"),
     _m_=new MlString("20px"),
     _l_=new MlString("0.9em"),
     _k_=new MlString("red"),
     _j_=
      new
       MlString
       ("\n  precision highp float;\n\n  uniform vec2 scale;\n  uniform mat3 transform;\n\n  attribute vec2 p0, p1, p2, p3; // the two extremities of the line\n  attribute vec2 n;\n  attribute float edge;   // source or target\n  attribute float side;   // which side of the line\n\n  attribute float w;          // line width\n  attribute lowp vec4 color;  // line color\n  attribute float style1, style2; // line style\n\n  varying mediump vec4 dx;\n  varying mediump vec2 dy_hw;\n  varying lowp vec4 col;\n\n  float determinant (mat3 m) {\n    return m[0][0] * m[1][1] - m[0][1] * m[1][0];\n  }\n\n  void main () {\n    vec2 q1 = (transform * vec3(p1, 1)).xy;\n    vec2 q2 = (transform * vec3(p2, 1)).xy;\n    float hw = w * sqrt(determinant(transform)) * 0.5;\n\n    vec2 dq = q2 - q1;\n    vec2 t1 = normalize(dq);\n    vec2 n1 = vec2(-t1.y, t1.x);\n\n    float border = hw + 1.;\n\n    float dy = (hw + 1.) * side;\n\n    vec4 d1 = vec4(t1, - dot(t1, q1), 0);\n    vec4 d2 = vec4(0, 0, length(dq), 0) - d1;\n    vec4 d3 = d1;\n    vec4 d4 = d2;\n\n    if (style1 > 0.) {\n      // Line cap\n      q1 = q1 + (border * side) * n1 - border * t1;\n      d1.z = (style1 == 3.) ? (d1.z+border): d1.z;\n      d3.z = (style1 == 1.) ? (d3.z-border): d3.z;\n    } else {\n      // Line join\n      vec2 q0 = (transform * vec3(p0, 1)).xy;\n      vec2 t0 = normalize(q1 - q0);\n      vec2 n0 = vec2(-t0.y, t0.x);\n      vec2 n = (n0 + n1) / (1. + dot(n0, n1));\n\nd3 = (style1 == -3.) ? vec4(-normalize(n), dot(normalize(n), q1), 0)  :  vec4(0.);\n\n      q1 = q1 + (border * side) * n;\n      d1 = (style1 == -2.) ? d1 : vec4(0.);\n//      d3 = vec4(0.);\n    }\n\n    if (style2 > 0.) {\n      // Line cap\n      q2 = q2 + (border * side) * n1 + border * t1;\n      d2.z = (style2 == 3.) ? (d2.z+border): d2.z;\n      d4.z = (style2 == 1.) ? (d4.z-border): d4.z;\n    } else {\n      // Line join\n      vec2 q3 = (transform * vec3(p3, 1)).xy;\n      vec2 t2 = normalize(q3 - q2);\n      vec2 n2 = vec2(-t2.y, t2.x);\n      vec2 n = (n1 + n2) / (1. + dot(n1, n2));\n  // |n| = sqrt(2/(1. + dot(n1, n2)))\n  // miter if miter_limit^2*(1. + dot(n1, n2)) < 2\n\nd4 = (style2 == -3.) ? vec4(-normalize(n), + dot(normalize(n), q2), 0)  :  vec4(0.);\n\n      q2 = q2 + (border * side) * n;\n      d2 = (style2 == -2.) ? d2 : vec4(0.);\n    }\n\n    vec2 q = edge > 0. ? q1 : q2;\n\n    dy_hw = vec2(dy, hw);\n    col = color;\n\n    dx = vec4(q, 1, 0) * mat4(d1,d2,d3,d4);\n\n    gl_Position = vec4(q * scale + vec2(-1., -1.), 0., 1.);\n  }\n"),
     _i_=
      new
       MlString
       ("\n  #define FAST 0\n  #define BLUR 0.6\n\n  precision mediump float;\n\n  varying mediump vec4 dx;\n  varying mediump vec2 dy_hw;\n  varying lowp vec4 col;\n\n  void main() {\n    float dy = dy_hw.x; float half_width = dy_hw.y;\n\n    vec2 ds = min (dx.xz, dx.yw);\n\n    float dx2 = min (0., ds.x);\n    float d = sqrt(dx2 * dx2 + dy * dy);\n\n    d = max(d, -ds.y);\n\n    vec3 dists = vec3(half_width - d,- half_width - d, half_width + ds.y);\n\n#if FAST\n    vec3 alphas = clamp(dists * BLUR + 0.5, 0., 1.);\n#else\n    vec3 alphas = smoothstep(0., 1., dists * BLUR + 0.5);\n#endif\n\n    float alpha = /* alphas.z * */ (alphas.x - alphas.y);\n\n    gl_FragColor = col * alpha;\n  }\n");
    function _h_(_f_){throw [0,_a_,_f_];}
    function _aT_(_g_){throw [0,_b_,_g_];}
    var _a1_=(1<<31)-1|0;
    function _a0_(_aU_,_aW_)
     {var
       _aV_=_aU_.getLen(),
       _aX_=_aW_.getLen(),
       _aY_=caml_create_string(_aV_+_aX_|0);
      caml_blit_string(_aU_,0,_aY_,0,_aV_);
      caml_blit_string(_aW_,0,_aY_,_aV_,_aX_);
      return _aY_;}
    function _a2_(_aZ_){return caml_format_int(_aP_,_aZ_);}
    var
     _ba_=caml_ml_open_descriptor_out(1),
     _a$_=caml_ml_open_descriptor_out(2);
    function _bb_(_a6_)
     {var _a3_=caml_ml_out_channels_list(0);
      for(;;)
       {if(_a3_){var _a4_=_a3_[2];try {}catch(_a5_){}var _a3_=_a4_;continue;}
        return 0;}}
    var _bc_=[0,_bb_];
    function _bg_(_a__,_a9_,_a7_,_a8_)
     {if(0<=_a7_&&0<=_a8_&&!((_a9_.getLen()-_a8_|0)<_a7_))
       return caml_ml_output(_a__,_a9_,_a7_,_a8_);
      return _aT_(_aS_);}
    function _bf_(_be_){return _bd_(_bc_[1],0);}
    caml_register_named_value(_aM_,_bf_);
    function _bv_(_bi_)
     {var _bh_=0,_bj_=_bi_;
      for(;;)
       {if(_bj_){var _bl_=_bj_[2],_bk_=_bh_+1|0,_bh_=_bk_,_bj_=_bl_;continue;}
        return _bh_;}}
    function _bw_(_bm_)
     {var _bn_=_bm_,_bo_=0;
      for(;;)
       {if(_bn_)
         {var _bp_=_bn_[2],_bq_=[0,_bn_[1],_bo_],_bn_=_bp_,_bo_=_bq_;
          continue;}
        return _bo_;}}
    function _bx_(_bt_,_br_)
     {var _bs_=_br_;
      for(;;)
       {if(_bs_){var _bu_=_bs_[2];_bd_(_bt_,_bs_[1]);var _bs_=_bu_;continue;}
        return 0;}}
    function _bK_(_by_,_bA_)
     {var _bz_=caml_create_string(_by_);
      caml_fill_string(_bz_,0,_by_,_bA_);
      return _bz_;}
    function _bL_(_bD_,_bB_,_bC_)
     {if(0<=_bB_&&0<=_bC_&&!((_bD_.getLen()-_bC_|0)<_bB_))
       {var _bE_=caml_create_string(_bC_);
        caml_blit_string(_bD_,_bB_,_bE_,0,_bC_);
        return _bE_;}
      return _aT_(_aD_);}
    function _bM_(_bH_,_bG_,_bJ_,_bI_,_bF_)
     {if
       (0<=
        _bF_&&
        0<=
        _bG_&&
        !((_bH_.getLen()-_bF_|0)<_bG_)&&
        0<=
        _bI_&&
        !((_bJ_.getLen()-_bF_|0)<_bI_))
       return caml_blit_string(_bH_,_bG_,_bJ_,_bI_,_bF_);
      return _aT_(_aE_);}
    var
     _bN_=caml_sys_get_config(0)[2],
     _bO_=caml_mul(_bN_/8|0,(1<<(_bN_-10|0))-1|0)-1|0;
    function _cc_(_bP_)
     {var
       _bQ_=1<=_bP_?_bP_:1,
       _bR_=_bO_<_bQ_?_bO_:_bQ_,
       _bS_=caml_create_string(_bR_);
      return [0,_bS_,0,_bR_,_bS_];}
    function _cd_(_bT_){return _bL_(_bT_[1],0,_bT_[2]);}
    function _b0_(_bU_,_bW_)
     {var _bV_=[0,_bU_[3]];
      for(;;)
       {if(_bV_[1]<(_bU_[2]+_bW_|0)){_bV_[1]=2*_bV_[1]|0;continue;}
        if(_bO_<_bV_[1])if((_bU_[2]+_bW_|0)<=_bO_)_bV_[1]=_bO_;else _h_(_aB_);
        var _bX_=caml_create_string(_bV_[1]);
        _bM_(_bU_[1],0,_bX_,0,_bU_[2]);
        _bU_[1]=_bX_;
        _bU_[3]=_bV_[1];
        return 0;}}
    function _ce_(_bY_,_b1_)
     {var _bZ_=_bY_[2];
      if(_bY_[3]<=_bZ_)_b0_(_bY_,1);
      _bY_[1].safeSet(_bZ_,_b1_);
      _bY_[2]=_bZ_+1|0;
      return 0;}
    function _cg_(_b8_,_b7_,_b2_,_b5_)
     {var _b3_=_b2_<0?1:0;
      if(_b3_)
       var _b4_=_b3_;
      else
       {var _b6_=_b5_<0?1:0,_b4_=_b6_?_b6_:(_b7_.getLen()-_b5_|0)<_b2_?1:0;}
      if(_b4_)_aT_(_aC_);
      var _b9_=_b8_[2]+_b5_|0;
      if(_b8_[3]<_b9_)_b0_(_b8_,_b5_);
      _bM_(_b7_,_b2_,_b8_[1],_b8_[2],_b5_);
      _b8_[2]=_b9_;
      return 0;}
    function _cf_(_ca_,_b__)
     {var _b$_=_b__.getLen(),_cb_=_ca_[2]+_b$_|0;
      if(_ca_[3]<_cb_)_b0_(_ca_,_b$_);
      _bM_(_b__,0,_ca_[1],_ca_[2],_b$_);
      _ca_[2]=_cb_;
      return 0;}
    function _ck_(_ch_){return 0<=_ch_?_ch_:_h_(_a0_(_ak_,_a2_(_ch_)));}
    function _cl_(_ci_,_cj_){return _ck_(_ci_+_cj_|0);}
    var _cm_=_bd_(_cl_,1);
    function _cr_(_cp_,_co_,_cn_){return _bL_(_cp_,_co_,_cn_);}
    function _cx_(_cq_){return _cr_(_cq_,0,_cq_.getLen());}
    function _cz_(_cs_,_ct_,_cv_)
     {var
       _cu_=_a0_(_an_,_a0_(_cs_,_ao_)),
       _cw_=_a0_(_am_,_a0_(_a2_(_ct_),_cu_));
      return _aT_(_a0_(_al_,_a0_(_bK_(1,_cv_),_cw_)));}
    function _do_(_cy_,_cB_,_cA_){return _cz_(_cx_(_cy_),_cB_,_cA_);}
    function _dp_(_cC_){return _aT_(_a0_(_ap_,_a0_(_cx_(_cC_),_aq_)));}
    function _cW_(_cD_,_cL_,_cN_,_cP_)
     {function _cK_(_cE_)
       {if((_cD_.safeGet(_cE_)-48|0)<0||9<(_cD_.safeGet(_cE_)-48|0))
         return _cE_;
        var _cF_=_cE_+1|0;
        for(;;)
         {var _cG_=_cD_.safeGet(_cF_);
          if(48<=_cG_)
           {if(!(58<=_cG_)){var _cI_=_cF_+1|0,_cF_=_cI_;continue;}var _cH_=0;}
          else
           if(36===_cG_){var _cJ_=_cF_+1|0,_cH_=1;}else var _cH_=0;
          if(!_cH_)var _cJ_=_cE_;
          return _cJ_;}}
      var _cM_=_cK_(_cL_+1|0),_cO_=_cc_((_cN_-_cM_|0)+10|0);
      _ce_(_cO_,37);
      var _cQ_=_cM_,_cR_=_bw_(_cP_);
      for(;;)
       {if(_cQ_<=_cN_)
         {var _cS_=_cD_.safeGet(_cQ_);
          if(42===_cS_)
           {if(_cR_)
             {var _cT_=_cR_[2];
              _cf_(_cO_,_a2_(_cR_[1]));
              var _cU_=_cK_(_cQ_+1|0),_cQ_=_cU_,_cR_=_cT_;
              continue;}
            throw [0,_d_,_ar_];}
          _ce_(_cO_,_cS_);
          var _cV_=_cQ_+1|0,_cQ_=_cV_;
          continue;}
        return _cd_(_cO_);}}
    function _fm_(_c2_,_c0_,_cZ_,_cY_,_cX_)
     {var _c1_=_cW_(_c0_,_cZ_,_cY_,_cX_);
      if(78!==_c2_&&110!==_c2_)return _c1_;
      _c1_.safeSet(_c1_.getLen()-1|0,117);
      return _c1_;}
    function _dq_(_c9_,_dh_,_dm_,_c3_,_dl_)
     {var _c4_=_c3_.getLen();
      function _dj_(_c5_,_dg_)
       {var _c6_=40===_c5_?41:125;
        function _df_(_c7_)
         {var _c8_=_c7_;
          for(;;)
           {if(_c4_<=_c8_)return _bd_(_c9_,_c3_);
            if(37===_c3_.safeGet(_c8_))
             {var _c__=_c8_+1|0;
              if(_c4_<=_c__)
               var _c$_=_bd_(_c9_,_c3_);
              else
               {var _da_=_c3_.safeGet(_c__),_db_=_da_-40|0;
                if(_db_<0||1<_db_)
                 {var _dc_=_db_-83|0;
                  if(_dc_<0||2<_dc_)
                   var _dd_=1;
                  else
                   switch(_dc_)
                    {case 1:var _dd_=1;break;
                     case 2:var _de_=1,_dd_=0;break;
                     default:var _de_=0,_dd_=0;}
                  if(_dd_){var _c$_=_df_(_c__+1|0),_de_=2;}}
                else
                 var _de_=0===_db_?0:1;
                switch(_de_)
                 {case 1:
                   var _c$_=_da_===_c6_?_c__+1|0:_di_(_dh_,_c3_,_dg_,_da_);
                   break;
                  case 2:break;
                  default:var _c$_=_df_(_dj_(_da_,_c__+1|0)+1|0);}}
              return _c$_;}
            var _dk_=_c8_+1|0,_c8_=_dk_;
            continue;}}
        return _df_(_dg_);}
      return _dj_(_dm_,_dl_);}
    function _dQ_(_dn_){return _di_(_dq_,_dp_,_do_,_dn_);}
    function _d6_(_dr_,_dC_,_dM_)
     {var _ds_=_dr_.getLen()-1|0;
      function _dO_(_dt_)
       {var _du_=_dt_;
        a:
        for(;;)
         {if(_du_<_ds_)
           {if(37===_dr_.safeGet(_du_))
             {var _dv_=0,_dw_=_du_+1|0;
              for(;;)
               {if(_ds_<_dw_)
                 var _dx_=_dp_(_dr_);
                else
                 {var _dy_=_dr_.safeGet(_dw_);
                  if(58<=_dy_)
                   {if(95===_dy_)
                     {var _dA_=_dw_+1|0,_dz_=1,_dv_=_dz_,_dw_=_dA_;continue;}}
                  else
                   if(32<=_dy_)
                    switch(_dy_-32|0)
                     {case 1:
                      case 2:
                      case 4:
                      case 5:
                      case 6:
                      case 7:
                      case 8:
                      case 9:
                      case 12:
                      case 15:break;
                      case 0:
                      case 3:
                      case 11:
                      case 13:var _dB_=_dw_+1|0,_dw_=_dB_;continue;
                      case 10:
                       var _dD_=_di_(_dC_,_dv_,_dw_,105),_dw_=_dD_;continue;
                      default:var _dE_=_dw_+1|0,_dw_=_dE_;continue;}
                  var _dF_=_dw_;
                  c:
                  for(;;)
                   {if(_ds_<_dF_)
                     var _dG_=_dp_(_dr_);
                    else
                     {var _dH_=_dr_.safeGet(_dF_);
                      if(126<=_dH_)
                       var _dI_=0;
                      else
                       switch(_dH_)
                        {case 78:
                         case 88:
                         case 100:
                         case 105:
                         case 111:
                         case 117:
                         case 120:var _dG_=_di_(_dC_,_dv_,_dF_,105),_dI_=1;break;
                         case 69:
                         case 70:
                         case 71:
                         case 101:
                         case 102:
                         case 103:var _dG_=_di_(_dC_,_dv_,_dF_,102),_dI_=1;break;
                         case 33:
                         case 37:
                         case 44:
                         case 64:var _dG_=_dF_+1|0,_dI_=1;break;
                         case 83:
                         case 91:
                         case 115:var _dG_=_di_(_dC_,_dv_,_dF_,115),_dI_=1;break;
                         case 97:
                         case 114:
                         case 116:var _dG_=_di_(_dC_,_dv_,_dF_,_dH_),_dI_=1;break;
                         case 76:
                         case 108:
                         case 110:
                          var _dJ_=_dF_+1|0;
                          if(_ds_<_dJ_)
                           {var _dG_=_di_(_dC_,_dv_,_dF_,105),_dI_=1;}
                          else
                           {var _dK_=_dr_.safeGet(_dJ_)-88|0;
                            if(_dK_<0||32<_dK_)
                             var _dL_=1;
                            else
                             switch(_dK_)
                              {case 0:
                               case 12:
                               case 17:
                               case 23:
                               case 29:
                               case 32:
                                var
                                 _dG_=_dN_(_dM_,_di_(_dC_,_dv_,_dF_,_dH_),105),
                                 _dI_=1,
                                 _dL_=0;
                                break;
                               default:var _dL_=1;}
                            if(_dL_){var _dG_=_di_(_dC_,_dv_,_dF_,105),_dI_=1;}}
                          break;
                         case 67:
                         case 99:var _dG_=_di_(_dC_,_dv_,_dF_,99),_dI_=1;break;
                         case 66:
                         case 98:var _dG_=_di_(_dC_,_dv_,_dF_,66),_dI_=1;break;
                         case 41:
                         case 125:var _dG_=_di_(_dC_,_dv_,_dF_,_dH_),_dI_=1;break;
                         case 40:
                          var _dG_=_dO_(_di_(_dC_,_dv_,_dF_,_dH_)),_dI_=1;break;
                         case 123:
                          var
                           _dP_=_di_(_dC_,_dv_,_dF_,_dH_),
                           _dR_=_di_(_dQ_,_dH_,_dr_,_dP_),
                           _dS_=_dP_;
                          for(;;)
                           {if(_dS_<(_dR_-2|0))
                             {var _dT_=_dN_(_dM_,_dS_,_dr_.safeGet(_dS_)),_dS_=_dT_;
                              continue;}
                            var _dU_=_dR_-1|0,_dF_=_dU_;
                            continue c;}
                         default:var _dI_=0;}
                      if(!_dI_)var _dG_=_do_(_dr_,_dF_,_dH_);}
                    var _dx_=_dG_;
                    break;}}
                var _du_=_dx_;
                continue a;}}
            var _dV_=_du_+1|0,_du_=_dV_;
            continue;}
          return _du_;}}
      _dO_(0);
      return 0;}
    function _d8_(_d7_)
     {var _dW_=[0,0,0,0];
      function _d5_(_d1_,_d2_,_dX_)
       {var _dY_=41!==_dX_?1:0,_dZ_=_dY_?125!==_dX_?1:0:_dY_;
        if(_dZ_)
         {var _d0_=97===_dX_?2:1;
          if(114===_dX_)_dW_[3]=_dW_[3]+1|0;
          if(_d1_)_dW_[2]=_dW_[2]+_d0_|0;else _dW_[1]=_dW_[1]+_d0_|0;}
        return _d2_+1|0;}
      _d6_(_d7_,_d5_,function(_d3_,_d4_){return _d3_+1|0;});
      return _dW_[1];}
    function _hM_(_ek_,_d9_)
     {var _d__=_d8_(_d9_);
      if(_d__<0||6<_d__)
       {var
         _em_=
          function(_d$_,_ef_)
           {if(_d__<=_d$_)
             {var
               _ea_=caml_make_vect(_d__,0),
               _ed_=
                function(_eb_,_ec_)
                 {return caml_array_set(_ea_,(_d__-_eb_|0)-1|0,_ec_);},
               _ee_=0,
               _eg_=_ef_;
              for(;;)
               {if(_eg_)
                 {var _eh_=_eg_[2],_ei_=_eg_[1];
                  if(_eh_)
                   {_ed_(_ee_,_ei_);
                    var _ej_=_ee_+1|0,_ee_=_ej_,_eg_=_eh_;
                    continue;}
                  _ed_(_ee_,_ei_);}
                return _dN_(_ek_,_d9_,_ea_);}}
            return function(_el_){return _em_(_d$_+1|0,[0,_el_,_ef_]);};};
        return _em_(0,0);}
      switch(_d__)
       {case 1:
         return function(_eo_)
          {var _en_=caml_make_vect(1,0);
           caml_array_set(_en_,0,_eo_);
           return _dN_(_ek_,_d9_,_en_);};
        case 2:
         return function(_eq_,_er_)
          {var _ep_=caml_make_vect(2,0);
           caml_array_set(_ep_,0,_eq_);
           caml_array_set(_ep_,1,_er_);
           return _dN_(_ek_,_d9_,_ep_);};
        case 3:
         return function(_et_,_eu_,_ev_)
          {var _es_=caml_make_vect(3,0);
           caml_array_set(_es_,0,_et_);
           caml_array_set(_es_,1,_eu_);
           caml_array_set(_es_,2,_ev_);
           return _dN_(_ek_,_d9_,_es_);};
        case 4:
         return function(_ex_,_ey_,_ez_,_eA_)
          {var _ew_=caml_make_vect(4,0);
           caml_array_set(_ew_,0,_ex_);
           caml_array_set(_ew_,1,_ey_);
           caml_array_set(_ew_,2,_ez_);
           caml_array_set(_ew_,3,_eA_);
           return _dN_(_ek_,_d9_,_ew_);};
        case 5:
         return function(_eC_,_eD_,_eE_,_eF_,_eG_)
          {var _eB_=caml_make_vect(5,0);
           caml_array_set(_eB_,0,_eC_);
           caml_array_set(_eB_,1,_eD_);
           caml_array_set(_eB_,2,_eE_);
           caml_array_set(_eB_,3,_eF_);
           caml_array_set(_eB_,4,_eG_);
           return _dN_(_ek_,_d9_,_eB_);};
        case 6:
         return function(_eI_,_eJ_,_eK_,_eL_,_eM_,_eN_)
          {var _eH_=caml_make_vect(6,0);
           caml_array_set(_eH_,0,_eI_);
           caml_array_set(_eH_,1,_eJ_);
           caml_array_set(_eH_,2,_eK_);
           caml_array_set(_eH_,3,_eL_);
           caml_array_set(_eH_,4,_eM_);
           caml_array_set(_eH_,5,_eN_);
           return _dN_(_ek_,_d9_,_eH_);};
        default:return _dN_(_ek_,_d9_,[0]);}}
    function _fi_(_eO_,_eR_,_eP_)
     {var _eQ_=_eO_.safeGet(_eP_);
      if((_eQ_-48|0)<0||9<(_eQ_-48|0))return _dN_(_eR_,0,_eP_);
      var _eS_=_eQ_-48|0,_eT_=_eP_+1|0;
      for(;;)
       {var _eU_=_eO_.safeGet(_eT_);
        if(48<=_eU_)
         {if(!(58<=_eU_))
           {var
             _eX_=_eT_+1|0,
             _eW_=(10*_eS_|0)+(_eU_-48|0)|0,
             _eS_=_eW_,
             _eT_=_eX_;
            continue;}
          var _eV_=0;}
        else
         if(36===_eU_)
          if(0===_eS_)
           {var _eY_=_h_(_at_),_eV_=1;}
          else
           {var _eY_=_dN_(_eR_,[0,_ck_(_eS_-1|0)],_eT_+1|0),_eV_=1;}
         else
          var _eV_=0;
        if(!_eV_)var _eY_=_dN_(_eR_,0,_eP_);
        return _eY_;}}
    function _fd_(_eZ_,_e0_){return _eZ_?_e0_:_bd_(_cm_,_e0_);}
    function _e3_(_e1_,_e2_){return _e1_?_e1_[1]:_e2_;}
    function _he_(_e9_,_e6_,_g4_,_fn_,_fq_,_gY_,_g1_,_gC_,_gB_)
     {function _e$_(_e5_,_e4_){return caml_array_get(_e6_,_e3_(_e5_,_e4_));}
      function _ff_(_fh_,_fa_,_fc_,_e7_)
       {var _e8_=_e7_;
        for(;;)
         {var _e__=_e9_.safeGet(_e8_)-32|0;
          if(!(_e__<0||25<_e__))
           switch(_e__)
            {case 1:
             case 2:
             case 4:
             case 5:
             case 6:
             case 7:
             case 8:
             case 9:
             case 12:
             case 15:break;
             case 10:
              return _fi_
                      (_e9_,
                       function(_fb_,_fg_)
                        {var _fe_=[0,_e$_(_fb_,_fa_),_fc_];
                         return _ff_(_fh_,_fd_(_fb_,_fa_),_fe_,_fg_);},
                       _e8_+1|0);
             default:var _fj_=_e8_+1|0,_e8_=_fj_;continue;}
          var _fk_=_e9_.safeGet(_e8_);
          if(124<=_fk_)
           var _fl_=0;
          else
           switch(_fk_)
            {case 78:
             case 88:
             case 100:
             case 105:
             case 111:
             case 117:
             case 120:
              var
               _fo_=_e$_(_fh_,_fa_),
               _fp_=caml_format_int(_fm_(_fk_,_e9_,_fn_,_e8_,_fc_),_fo_),
               _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_fp_,_e8_+1|0),
               _fl_=1;
              break;
             case 69:
             case 71:
             case 101:
             case 102:
             case 103:
              var
               _fs_=_e$_(_fh_,_fa_),
               _ft_=caml_format_float(_cW_(_e9_,_fn_,_e8_,_fc_),_fs_),
               _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_ft_,_e8_+1|0),
               _fl_=1;
              break;
             case 76:
             case 108:
             case 110:
              var _fu_=_e9_.safeGet(_e8_+1|0)-88|0;
              if(_fu_<0||32<_fu_)
               var _fv_=1;
              else
               switch(_fu_)
                {case 0:
                 case 12:
                 case 17:
                 case 23:
                 case 29:
                 case 32:
                  var _fw_=_e8_+1|0,_fx_=_fk_-108|0;
                  if(_fx_<0||2<_fx_)
                   var _fy_=0;
                  else
                   {switch(_fx_)
                     {case 1:var _fy_=0,_fz_=0;break;
                      case 2:
                       var
                        _fA_=_e$_(_fh_,_fa_),
                        _fB_=caml_format_int(_cW_(_e9_,_fn_,_fw_,_fc_),_fA_),
                        _fz_=1;
                       break;
                      default:
                       var
                        _fC_=_e$_(_fh_,_fa_),
                        _fB_=caml_format_int(_cW_(_e9_,_fn_,_fw_,_fc_),_fC_),
                        _fz_=1;}
                    if(_fz_){var _fD_=_fB_,_fy_=1;}}
                  if(!_fy_)
                   {var
                     _fE_=_e$_(_fh_,_fa_),
                     _fD_=caml_int64_format(_cW_(_e9_,_fn_,_fw_,_fc_),_fE_);}
                  var
                   _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_fD_,_fw_+1|0),
                   _fl_=1,
                   _fv_=0;
                  break;
                 default:var _fv_=1;}
              if(_fv_)
               {var
                 _fF_=_e$_(_fh_,_fa_),
                 _fG_=caml_format_int(_fm_(110,_e9_,_fn_,_e8_,_fc_),_fF_),
                 _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_fG_,_e8_+1|0),
                 _fl_=1;}
              break;
             case 37:
             case 64:
              var _fr_=_di_(_fq_,_fa_,_bK_(1,_fk_),_e8_+1|0),_fl_=1;break;
             case 83:
             case 115:
              var _fH_=_e$_(_fh_,_fa_);
              if(115===_fk_)
               var _fI_=_fH_;
              else
               {var _fJ_=[0,0],_fK_=0,_fL_=_fH_.getLen()-1|0;
                if(!(_fL_<_fK_))
                 {var _fM_=_fK_;
                  for(;;)
                   {var
                     _fN_=_fH_.safeGet(_fM_),
                     _fO_=
                      14<=_fN_
                       ?34===_fN_?1:92===_fN_?1:0
                       :11<=_fN_?13<=_fN_?1:0:8<=_fN_?1:0,
                     _fP_=_fO_?2:caml_is_printable(_fN_)?1:4;
                    _fJ_[1]=_fJ_[1]+_fP_|0;
                    var _fQ_=_fM_+1|0;
                    if(_fL_!==_fM_){var _fM_=_fQ_;continue;}
                    break;}}
                if(_fJ_[1]===_fH_.getLen())
                 var _fR_=_fH_;
                else
                 {var _fS_=caml_create_string(_fJ_[1]);
                  _fJ_[1]=0;
                  var _fT_=0,_fU_=_fH_.getLen()-1|0;
                  if(!(_fU_<_fT_))
                   {var _fV_=_fT_;
                    for(;;)
                     {var _fW_=_fH_.safeGet(_fV_),_fX_=_fW_-34|0;
                      if(_fX_<0||58<_fX_)
                       if(-20<=_fX_)
                        var _fY_=1;
                       else
                        {switch(_fX_+34|0)
                          {case 8:
                            _fS_.safeSet(_fJ_[1],92);
                            _fJ_[1]+=1;
                            _fS_.safeSet(_fJ_[1],98);
                            var _fZ_=1;
                            break;
                           case 9:
                            _fS_.safeSet(_fJ_[1],92);
                            _fJ_[1]+=1;
                            _fS_.safeSet(_fJ_[1],116);
                            var _fZ_=1;
                            break;
                           case 10:
                            _fS_.safeSet(_fJ_[1],92);
                            _fJ_[1]+=1;
                            _fS_.safeSet(_fJ_[1],110);
                            var _fZ_=1;
                            break;
                           case 13:
                            _fS_.safeSet(_fJ_[1],92);
                            _fJ_[1]+=1;
                            _fS_.safeSet(_fJ_[1],114);
                            var _fZ_=1;
                            break;
                           default:var _fY_=1,_fZ_=0;}
                         if(_fZ_)var _fY_=0;}
                      else
                       var
                        _fY_=
                         (_fX_-1|0)<0||56<(_fX_-1|0)
                          ?(_fS_.safeSet(_fJ_[1],92),
                            _fJ_[1]+=
                            1,
                            _fS_.safeSet(_fJ_[1],_fW_),
                            0)
                          :1;
                      if(_fY_)
                       if(caml_is_printable(_fW_))
                        _fS_.safeSet(_fJ_[1],_fW_);
                       else
                        {_fS_.safeSet(_fJ_[1],92);
                         _fJ_[1]+=1;
                         _fS_.safeSet(_fJ_[1],48+(_fW_/100|0)|0);
                         _fJ_[1]+=1;
                         _fS_.safeSet(_fJ_[1],48+((_fW_/10|0)%10|0)|0);
                         _fJ_[1]+=1;
                         _fS_.safeSet(_fJ_[1],48+(_fW_%10|0)|0);}
                      _fJ_[1]+=1;
                      var _f0_=_fV_+1|0;
                      if(_fU_!==_fV_){var _fV_=_f0_;continue;}
                      break;}}
                  var _fR_=_fS_;}
                var _fI_=_a0_(_ax_,_a0_(_fR_,_ay_));}
              if(_e8_===(_fn_+1|0))
               var _f1_=_fI_;
              else
               {var _f2_=_cW_(_e9_,_fn_,_e8_,_fc_);
                try
                 {var _f3_=0,_f4_=1;
                  for(;;)
                   {if(_f2_.getLen()<=_f4_)
                     var _f5_=[0,0,_f3_];
                    else
                     {var _f6_=_f2_.safeGet(_f4_);
                      if(49<=_f6_)
                       if(58<=_f6_)
                        var _f7_=0;
                       else
                        {var
                          _f5_=
                           [0,
                            caml_int_of_string
                             (_bL_(_f2_,_f4_,(_f2_.getLen()-_f4_|0)-1|0)),
                            _f3_],
                          _f7_=1;}
                      else
                       {if(45===_f6_)
                         {var _f9_=_f4_+1|0,_f8_=1,_f3_=_f8_,_f4_=_f9_;continue;}
                        var _f7_=0;}
                      if(!_f7_){var _f__=_f4_+1|0,_f4_=_f__;continue;}}
                    var _f$_=_f5_;
                    break;}}
                catch(_ga_)
                 {if(_ga_[1]!==_a_)throw _ga_;var _f$_=_cz_(_f2_,0,115);}
                var
                 _gb_=_f$_[1],
                 _gc_=_fI_.getLen(),
                 _gd_=0,
                 _gh_=_f$_[2],
                 _gg_=32;
                if(_gb_===_gc_&&0===_gd_)
                 {var _ge_=_fI_,_gf_=1;}
                else
                 var _gf_=0;
                if(!_gf_)
                 if(_gb_<=_gc_)
                  var _ge_=_bL_(_fI_,_gd_,_gc_);
                 else
                  {var _gi_=_bK_(_gb_,_gg_);
                   if(_gh_)
                    _bM_(_fI_,_gd_,_gi_,0,_gc_);
                   else
                    _bM_(_fI_,_gd_,_gi_,_gb_-_gc_|0,_gc_);
                   var _ge_=_gi_;}
                var _f1_=_ge_;}
              var _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_f1_,_e8_+1|0),_fl_=1;
              break;
             case 67:
             case 99:
              var _gj_=_e$_(_fh_,_fa_);
              if(99===_fk_)
               var _gk_=_bK_(1,_gj_);
              else
               {if(39===_gj_)
                 var _gl_=_aG_;
                else
                 if(92===_gj_)
                  var _gl_=_aH_;
                 else
                  {if(14<=_gj_)
                    var _gm_=0;
                   else
                    switch(_gj_)
                     {case 8:var _gl_=_aL_,_gm_=1;break;
                      case 9:var _gl_=_aK_,_gm_=1;break;
                      case 10:var _gl_=_aJ_,_gm_=1;break;
                      case 13:var _gl_=_aI_,_gm_=1;break;
                      default:var _gm_=0;}
                   if(!_gm_)
                    if(caml_is_printable(_gj_))
                     {var _gn_=caml_create_string(1);
                      _gn_.safeSet(0,_gj_);
                      var _gl_=_gn_;}
                    else
                     {var _go_=caml_create_string(4);
                      _go_.safeSet(0,92);
                      _go_.safeSet(1,48+(_gj_/100|0)|0);
                      _go_.safeSet(2,48+((_gj_/10|0)%10|0)|0);
                      _go_.safeSet(3,48+(_gj_%10|0)|0);
                      var _gl_=_go_;}}
                var _gk_=_a0_(_av_,_a0_(_gl_,_aw_));}
              var _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_gk_,_e8_+1|0),_fl_=1;
              break;
             case 66:
             case 98:
              var
               _gq_=_e8_+1|0,
               _gp_=_e$_(_fh_,_fa_)?_aO_:_aN_,
               _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_gp_,_gq_),
               _fl_=1;
              break;
             case 40:
             case 123:
              var _gr_=_e$_(_fh_,_fa_),_gs_=_di_(_dQ_,_fk_,_e9_,_e8_+1|0);
              if(123===_fk_)
               {var
                 _gt_=_cc_(_gr_.getLen()),
                 _gx_=function(_gv_,_gu_){_ce_(_gt_,_gu_);return _gv_+1|0;};
                _d6_
                 (_gr_,
                  function(_gw_,_gz_,_gy_)
                   {if(_gw_)_cf_(_gt_,_as_);else _ce_(_gt_,37);
                    return _gx_(_gz_,_gy_);},
                  _gx_);
                var
                 _gA_=_cd_(_gt_),
                 _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_gA_,_gs_),
                 _fl_=1;}
              else
               {var _fr_=_di_(_gB_,_fd_(_fh_,_fa_),_gr_,_gs_),_fl_=1;}
              break;
             case 33:var _fr_=_dN_(_gC_,_fa_,_e8_+1|0),_fl_=1;break;
             case 41:var _fr_=_di_(_fq_,_fa_,_aA_,_e8_+1|0),_fl_=1;break;
             case 44:var _fr_=_di_(_fq_,_fa_,_az_,_e8_+1|0),_fl_=1;break;
             case 70:
              var _gD_=_e$_(_fh_,_fa_);
              if(0===_fc_)
               {var
                 _gE_=caml_format_float(_aR_,_gD_),
                 _gF_=0,
                 _gG_=_gE_.getLen();
                for(;;)
                 {if(_gG_<=_gF_)
                   var _gH_=_a0_(_gE_,_aQ_);
                  else
                   {var
                     _gI_=_gE_.safeGet(_gF_),
                     _gJ_=48<=_gI_?58<=_gI_?0:1:45===_gI_?1:0;
                    if(_gJ_){var _gK_=_gF_+1|0,_gF_=_gK_;continue;}
                    var _gH_=_gE_;}
                  var _gL_=_gH_;
                  break;}}
              else
               {var _gM_=_cW_(_e9_,_fn_,_e8_,_fc_);
                if(70===_fk_)_gM_.safeSet(_gM_.getLen()-1|0,103);
                var _gN_=caml_format_float(_gM_,_gD_);
                if(3<=caml_classify_float(_gD_))
                 var _gO_=_gN_;
                else
                 {var _gP_=0,_gQ_=_gN_.getLen();
                  for(;;)
                   {if(_gQ_<=_gP_)
                     var _gR_=_a0_(_gN_,_au_);
                    else
                     {var
                       _gS_=_gN_.safeGet(_gP_)-46|0,
                       _gT_=
                        _gS_<0||23<_gS_
                         ?55===_gS_?1:0
                         :(_gS_-1|0)<0||21<(_gS_-1|0)?1:0;
                      if(!_gT_){var _gU_=_gP_+1|0,_gP_=_gU_;continue;}
                      var _gR_=_gN_;}
                    var _gO_=_gR_;
                    break;}}
                var _gL_=_gO_;}
              var _fr_=_di_(_fq_,_fd_(_fh_,_fa_),_gL_,_e8_+1|0),_fl_=1;
              break;
             case 91:var _fr_=_do_(_e9_,_e8_,_fk_),_fl_=1;break;
             case 97:
              var
               _gV_=_e$_(_fh_,_fa_),
               _gW_=_bd_(_cm_,_e3_(_fh_,_fa_)),
               _gX_=_e$_(0,_gW_),
               _fr_=_gZ_(_gY_,_fd_(_fh_,_gW_),_gV_,_gX_,_e8_+1|0),
               _fl_=1;
              break;
             case 114:var _fr_=_do_(_e9_,_e8_,_fk_),_fl_=1;break;
             case 116:
              var
               _g0_=_e$_(_fh_,_fa_),
               _fr_=_di_(_g1_,_fd_(_fh_,_fa_),_g0_,_e8_+1|0),
               _fl_=1;
              break;
             default:var _fl_=0;}
          if(!_fl_)var _fr_=_do_(_e9_,_e8_,_fk_);
          return _fr_;}}
      var _g6_=_fn_+1|0,_g3_=0;
      return _fi_
              (_e9_,
               function(_g5_,_g2_){return _ff_(_g5_,_g4_,_g3_,_g2_);},
               _g6_);}
    function _hW_(_ht_,_g8_,_hm_,_hp_,_hB_,_hL_,_g7_)
     {var _g9_=_bd_(_g8_,_g7_);
      function _hJ_(_hc_,_hK_,_g__,_hl_)
       {var _hb_=_g__.getLen();
        function _hq_(_hk_,_g$_)
         {var _ha_=_g$_;
          for(;;)
           {if(_hb_<=_ha_)return _bd_(_hc_,_g9_);
            var _hd_=_g__.safeGet(_ha_);
            if(37===_hd_)
             return _he_(_g__,_hl_,_hk_,_ha_,_hj_,_hi_,_hh_,_hg_,_hf_);
            _dN_(_hm_,_g9_,_hd_);
            var _hn_=_ha_+1|0,_ha_=_hn_;
            continue;}}
        function _hj_(_hs_,_ho_,_hr_)
         {_dN_(_hp_,_g9_,_ho_);return _hq_(_hs_,_hr_);}
        function _hi_(_hx_,_hv_,_hu_,_hw_)
         {if(_ht_)_dN_(_hp_,_g9_,_dN_(_hv_,0,_hu_));else _dN_(_hv_,_g9_,_hu_);
          return _hq_(_hx_,_hw_);}
        function _hh_(_hA_,_hy_,_hz_)
         {if(_ht_)_dN_(_hp_,_g9_,_bd_(_hy_,0));else _bd_(_hy_,_g9_);
          return _hq_(_hA_,_hz_);}
        function _hg_(_hD_,_hC_){_bd_(_hB_,_g9_);return _hq_(_hD_,_hC_);}
        function _hf_(_hF_,_hE_,_hG_)
         {var _hH_=_cl_(_d8_(_hE_),_hF_);
          return _hJ_(function(_hI_){return _hq_(_hH_,_hG_);},_hF_,_hE_,_hl_);}
        return _hq_(_hK_,0);}
      return _hM_(_dN_(_hJ_,_hL_,_ck_(0)),_g7_);}
    function _hV_(_hN_){return _cc_(2*_hN_.getLen()|0);}
    function _hS_(_hQ_,_hO_)
     {var _hP_=_cd_(_hO_);_hO_[2]=0;return _bd_(_hQ_,_hP_);}
    function _h0_(_hR_)
     {var _hU_=_bd_(_hS_,_hR_);
      return _hX_(_hW_,1,_hV_,_ce_,_cf_,function(_hT_){return 0;},_hU_);}
    function _h1_(_hZ_){return _dN_(_h0_,function(_hY_){return _hY_;},_hZ_);}
    var _h2_=[0,0];
    function _h8_(_h3_,_h5_)
     {var _h4_=[0,[0,_h3_,0]],_h6_=_h5_[1];
      if(_h6_){var _h7_=_h6_[1];_h5_[1]=_h4_;_h7_[2]=_h4_;return 0;}
      _h5_[1]=_h4_;
      _h5_[2]=_h4_;
      return 0;}
    var _h9_=[0,_T_];
    function _if_(_h__)
     {var _h$_=_h__[2];
      if(_h$_)
       {var _ia_=_h$_[1],_ib_=_ia_[2],_ic_=_ia_[1];
        _h__[2]=_ib_;
        if(0===_ib_)_h__[1]=0;
        return _ic_;}
      throw [0,_h9_];}
    function _ig_(_ie_,_id_)
     {_ie_[13]=_ie_[13]+_id_[3]|0;return _h8_(_id_,_ie_[27]);}
    var _ih_=1000000010;
    function _jd_(_ij_,_ii_){return _di_(_ij_[17],_ii_,0,_ii_.getLen());}
    function _in_(_ik_){return _bd_(_ik_[19],0);}
    function _iu_(_il_,_im_){return _bd_(_il_[20],_im_);}
    function _iv_(_io_,_iq_,_ip_)
     {_in_(_io_);
      _io_[11]=1;
      var
       _ir_=(_io_[6]-_ip_|0)+_iq_|0,
       _is_=_io_[8],
       _it_=caml_lessequal(_is_,_ir_)?_is_:_ir_;
      _io_[10]=_it_;
      _io_[9]=_io_[6]-_io_[10]|0;
      return _iu_(_io_,_io_[10]);}
    function _i__(_ix_,_iw_){return _iv_(_ix_,0,_iw_);}
    function _iP_(_iy_,_iz_){_iy_[9]=_iy_[9]-_iz_|0;return _iu_(_iy_,_iz_);}
    function _jw_(_iA_)
     {try
       {for(;;)
         {var _iB_=_iA_[27][2];
          if(!_iB_)throw [0,_h9_];
          var
           _iC_=_iB_[1][1],
           _iD_=_iC_[1],
           _iE_=_iC_[2],
           _iF_=_iD_<0?1:0,
           _iH_=_iC_[3],
           _iG_=_iF_?(_iA_[13]-_iA_[12]|0)<_iA_[9]?1:0:_iF_,
           _iI_=1-_iG_;
          if(_iI_)
           {_if_(_iA_[27]);
            var _iJ_=0<=_iD_?_iD_:_ih_;
            if(typeof _iE_==="number")
             switch(_iE_)
              {case 1:var _jf_=_iA_[2];if(_jf_)_iA_[2]=_jf_[2];break;
               case 2:var _jg_=_iA_[3];if(_jg_)_iA_[3]=_jg_[2];break;
               case 3:
                var _jh_=_iA_[2];
                if(_jh_)_i__(_iA_,_jh_[1][2]);else _in_(_iA_);
                break;
               case 4:
                if(_iA_[10]!==(_iA_[6]-_iA_[9]|0))
                 {var _ji_=_if_(_iA_[27]),_jj_=_ji_[1];
                  _iA_[12]=_iA_[12]-_ji_[3]|0;
                  _iA_[9]=_iA_[9]+_jj_|0;}
                break;
               case 5:
                var _jk_=_iA_[5];
                if(_jk_)
                 {var _jl_=_jk_[2];
                  _jd_(_iA_,_bd_(_iA_[24],_jk_[1]));
                  _iA_[5]=_jl_;}
                break;
               default:
                var _jm_=_iA_[3];
                if(_jm_)
                 {var
                   _jn_=_jm_[1][1],
                   _jr_=
                    function(_jq_,_jo_)
                     {if(_jo_)
                       {var _jp_=_jo_[1],_js_=_jo_[2];
                        return caml_lessthan(_jq_,_jp_)
                                ?[0,_jq_,_jo_]
                                :[0,_jp_,_jr_(_jq_,_js_)];}
                      return [0,_jq_,0];};
                  _jn_[1]=_jr_(_iA_[6]-_iA_[9]|0,_jn_[1]);}}
            else
             switch(_iE_[0])
              {case 1:
                var _iK_=_iE_[2],_iL_=_iE_[1],_iM_=_iA_[2];
                if(_iM_)
                 {var _iN_=_iM_[1],_iO_=_iN_[2];
                  switch(_iN_[1])
                   {case 1:_iv_(_iA_,_iK_,_iO_);break;
                    case 2:_iv_(_iA_,_iK_,_iO_);break;
                    case 3:
                     if(_iA_[9]<_iJ_)_iv_(_iA_,_iK_,_iO_);else _iP_(_iA_,_iL_);
                     break;
                    case 4:
                     if(_iA_[11])
                      _iP_(_iA_,_iL_);
                     else
                      if(_iA_[9]<_iJ_)
                       _iv_(_iA_,_iK_,_iO_);
                      else
                       if(((_iA_[6]-_iO_|0)+_iK_|0)<_iA_[10])
                        _iv_(_iA_,_iK_,_iO_);
                       else
                        _iP_(_iA_,_iL_);
                     break;
                    case 5:_iP_(_iA_,_iL_);break;
                    default:_iP_(_iA_,_iL_);}}
                break;
               case 2:
                var
                 _iQ_=_iA_[6]-_iA_[9]|0,
                 _iR_=_iA_[3],
                 _i3_=_iE_[2],
                 _i2_=_iE_[1];
                if(_iR_)
                 {var _iS_=_iR_[1][1],_iT_=_iS_[1];
                  if(_iT_)
                   {var _iZ_=_iT_[1];
                    try
                     {var _iU_=_iS_[1];
                      for(;;)
                       {if(!_iU_)throw [0,_c_];
                        var _iV_=_iU_[1],_iX_=_iU_[2];
                        if(!caml_greaterequal(_iV_,_iQ_)){var _iU_=_iX_;continue;}
                        var _iW_=_iV_;
                        break;}}
                    catch(_iY_){if(_iY_[1]!==_c_)throw _iY_;var _iW_=_iZ_;}
                    var _i0_=_iW_;}
                  else
                   var _i0_=_iQ_;
                  var _i1_=_i0_-_iQ_|0;
                  if(0<=_i1_)
                   _iP_(_iA_,_i1_+_i2_|0);
                  else
                   _iv_(_iA_,_i0_+_i3_|0,_iA_[6]);}
                break;
               case 3:
                var _i4_=_iE_[2],_i$_=_iE_[1];
                if(_iA_[8]<(_iA_[6]-_iA_[9]|0))
                 {var _i5_=_iA_[2];
                  if(_i5_)
                   {var
                     _i6_=_i5_[1],
                     _i7_=_i6_[2],
                     _i8_=_i6_[1],
                     _i9_=_iA_[9]<_i7_?0===_i8_?0:5<=_i8_?1:(_i__(_iA_,_i7_),1):0;
                    _i9_;}
                  else
                   _in_(_iA_);}
                var _jb_=_iA_[9]-_i$_|0,_ja_=1===_i4_?1:_iA_[9]<_iJ_?_i4_:5;
                _iA_[2]=[0,[0,_ja_,_jb_],_iA_[2]];
                break;
               case 4:_iA_[3]=[0,_iE_[1],_iA_[3]];break;
               case 5:
                var _jc_=_iE_[1];
                _jd_(_iA_,_bd_(_iA_[23],_jc_));
                _iA_[5]=[0,_jc_,_iA_[5]];
                break;
               default:
                var _je_=_iE_[1];
                _iA_[9]=_iA_[9]-_iJ_|0;
                _jd_(_iA_,_je_);
                _iA_[11]=0;}
            _iA_[12]=_iH_+_iA_[12]|0;
            continue;}
          break;}}
      catch(_jt_){if(_jt_[1]===_h9_)return 0;throw _jt_;}
      return _iI_;}
    function _jD_(_jv_,_ju_){_ig_(_jv_,_ju_);return _jw_(_jv_);}
    function _jB_(_jz_,_jy_,_jx_){return [0,_jz_,_jy_,_jx_];}
    function _jF_(_jE_,_jC_,_jA_){return _jD_(_jE_,_jB_(_jC_,[0,_jA_],_jC_));}
    var _jG_=[0,[0,-1,_jB_(-1,_S_,0)],0];
    function _jO_(_jH_){_jH_[1]=_jG_;return 0;}
    function _jX_(_jI_,_jQ_)
     {var _jJ_=_jI_[1];
      if(_jJ_)
       {var _jK_=_jJ_[1],_jL_=_jK_[2],_jM_=_jL_[1],_jN_=_jJ_[2],_jP_=_jL_[2];
        if(_jK_[1]<_jI_[12])return _jO_(_jI_);
        if(typeof _jP_!=="number")
         switch(_jP_[0])
          {case 1:
           case 2:
            var _jR_=_jQ_?(_jL_[1]=_jI_[13]+_jM_|0,_jI_[1]=_jN_,0):_jQ_;
            return _jR_;
           case 3:
            var
             _jS_=1-_jQ_,
             _jT_=_jS_?(_jL_[1]=_jI_[13]+_jM_|0,_jI_[1]=_jN_,0):_jS_;
            return _jT_;
           default:}
        return 0;}
      return 0;}
    function _j1_(_jV_,_jW_,_jU_)
     {_ig_(_jV_,_jU_);
      if(_jW_)_jX_(_jV_,1);
      _jV_[1]=[0,[0,_jV_[13],_jU_],_jV_[1]];
      return 0;}
    function _kd_(_jY_,_j0_,_jZ_)
     {_jY_[14]=_jY_[14]+1|0;
      if(_jY_[14]<_jY_[15])
       return _j1_(_jY_,0,_jB_(-_jY_[13]|0,[3,_j0_,_jZ_],0));
      var _j2_=_jY_[14]===_jY_[15]?1:0;
      if(_j2_){var _j3_=_jY_[16];return _jF_(_jY_,_j3_.getLen(),_j3_);}
      return _j2_;}
    function _ka_(_j4_,_j7_)
     {var _j5_=1<_j4_[14]?1:0;
      if(_j5_)
       {if(_j4_[14]<_j4_[15]){_ig_(_j4_,[0,0,1,0]);_jX_(_j4_,1);_jX_(_j4_,0);}
        _j4_[14]=_j4_[14]-1|0;
        var _j6_=0;}
      else
       var _j6_=_j5_;
      return _j6_;}
    function _ky_(_j8_,_j9_)
     {if(_j8_[21]){_j8_[4]=[0,_j9_,_j8_[4]];_bd_(_j8_[25],_j9_);}
      var _j__=_j8_[22];
      return _j__?_ig_(_j8_,[0,0,[5,_j9_],0]):_j__;}
    function _km_(_j$_,_kb_)
     {for(;;)
       {if(1<_j$_[14]){_ka_(_j$_,0);continue;}
        _j$_[13]=_ih_;
        _jw_(_j$_);
        if(_kb_)_in_(_j$_);
        _j$_[12]=1;
        _j$_[13]=1;
        var _kc_=_j$_[27];
        _kc_[1]=0;
        _kc_[2]=0;
        _jO_(_j$_);
        _j$_[2]=0;
        _j$_[3]=0;
        _j$_[4]=0;
        _j$_[5]=0;
        _j$_[10]=0;
        _j$_[14]=0;
        _j$_[9]=_j$_[6];
        return _kd_(_j$_,0,3);}}
    function _ki_(_ke_,_kh_,_kg_)
     {var _kf_=_ke_[14]<_ke_[15]?1:0;return _kf_?_jF_(_ke_,_kh_,_kg_):_kf_;}
    function _kz_(_kl_,_kk_,_kj_){return _ki_(_kl_,_kk_,_kj_);}
    function _kA_(_kn_,_ko_){_km_(_kn_,0);return _bd_(_kn_[18],0);}
    function _kt_(_kp_,_ks_,_kr_)
     {var _kq_=_kp_[14]<_kp_[15]?1:0;
      return _kq_?_j1_(_kp_,1,_jB_(-_kp_[13]|0,[1,_ks_,_kr_],_ks_)):_kq_;}
    function _kB_(_ku_,_kv_){return _kt_(_ku_,1,0);}
    function _kD_(_kw_,_kx_){return _di_(_kw_[17],_U_,0,1);}
    var _kC_=_bK_(80,32);
    function _kY_(_kH_,_kE_)
     {var _kF_=_kE_;
      for(;;)
       {var _kG_=0<_kF_?1:0;
        if(_kG_)
         {if(80<_kF_)
           {_di_(_kH_[17],_kC_,0,80);var _kI_=_kF_-80|0,_kF_=_kI_;continue;}
          return _di_(_kH_[17],_kC_,0,_kF_);}
        return _kG_;}}
    function _kU_(_kJ_){return _a0_(_V_,_a0_(_kJ_,_W_));}
    function _kT_(_kK_){return _a0_(_X_,_a0_(_kK_,_Y_));}
    function _kS_(_kL_){return 0;}
    function _k2_(_kW_,_kV_)
     {function _kO_(_kM_){return 0;}
      var _kP_=[0,0,0];
      function _kR_(_kN_){return 0;}
      var _kQ_=_jB_(-1,___,0);
      _h8_(_kQ_,_kP_);
      var
       _kX_=
        [0,
         [0,[0,1,_kQ_],_jG_],
         0,
         0,
         0,
         0,
         78,
         10,
         78-10|0,
         78,
         0,
         1,
         1,
         1,
         1,
         _a1_,
         _Z_,
         _kW_,
         _kV_,
         _kR_,
         _kO_,
         0,
         0,
         _kU_,
         _kT_,
         _kS_,
         _kS_,
         _kP_];
      _kX_[19]=_bd_(_kD_,_kX_);
      _kX_[20]=_bd_(_kY_,_kX_);
      return _kX_;}
    function _k6_(_kZ_)
     {function _k1_(_k0_){return caml_ml_flush(_kZ_);}
      return _k2_(_bd_(_bg_,_kZ_),_k1_);}
    function _k7_(_k4_)
     {function _k5_(_k3_){return 0;}return _k2_(_bd_(_cg_,_k4_),_k5_);}
    var _k8_=_cc_(512),_k9_=_k6_(_ba_);
    _k6_(_a$_);
    _k7_(_k8_);
    var _oy_=_bd_(_kA_,_k9_);
    function _ld_(_lb_,_k__,_k$_)
     {var
       _la_=
        _k$_<_k__.getLen()
         ?_dN_(_h1_,_ab_,_k__.safeGet(_k$_))
         :_dN_(_h1_,_aa_,46);
      return _lc_(_h1_,_$_,_lb_,_cx_(_k__),_k$_,_la_);}
    function _lh_(_lg_,_lf_,_le_){return _aT_(_ld_(_lg_,_lf_,_le_));}
    function _l$_(_lj_,_li_){return _lh_(_ac_,_lj_,_li_);}
    function _lq_(_ll_,_lk_){return _aT_(_ld_(_ad_,_ll_,_lk_));}
    function _nV_(_ls_,_lr_,_lm_)
     {try
       {var _ln_=caml_int_of_string(_lm_),_lo_=_ln_;}
      catch(_lp_){if(_lp_[1]!==_a_)throw _lp_;var _lo_=_lq_(_ls_,_lr_);}
      return _lo_;}
    function _lA_(_lu_,_lt_)
     {_km_(_lt_,0);
      var _lv_=_cd_(_lu_);
      _lu_[2]=0;
      _lu_[1]=_lu_[4];
      _lu_[3]=_lu_[1].getLen();
      return _lv_;}
    function _mF_(_lz_,_ly_)
     {var _lw_=_cc_(512),_lx_=_k7_(_lw_);
      _dN_(_lz_,_lx_,_ly_);
      return _lA_(_lw_,_lx_);}
    function _ms_(_lC_,_lB_)
     {if(_lB_)
       {var _lD_=_bw_([0,_lC_,_lB_]);
        if(_lD_)
         {var _lE_=_lD_[1],_lF_=[0,0],_lG_=[0,0],_lI_=_lD_[2];
          _bx_
           (function(_lH_)
             {_lF_[1]+=1;_lG_[1]=_lG_[1]+_lH_.getLen()|0;return 0;},
            _lD_);
          var
           _lJ_=
            caml_create_string(_lG_[1]+caml_mul(_e_.getLen(),_lF_[1]-1|0)|0);
          caml_blit_string(_lE_,0,_lJ_,0,_lE_.getLen());
          var _lK_=[0,_lE_.getLen()];
          _bx_
           (function(_lL_)
             {caml_blit_string(_e_,0,_lJ_,_lK_[1],_e_.getLen());
              _lK_[1]=_lK_[1]+_e_.getLen()|0;
              caml_blit_string(_lL_,0,_lJ_,_lK_[1],_lL_.getLen());
              _lK_[1]=_lK_[1]+_lL_.getLen()|0;
              return 0;},
            _lI_);
          var _lM_=_lJ_;}
        else
         var _lM_=_aF_;
        return _lM_;}
      return _lC_;}
    function _ou_(_mB_,_lQ_)
     {function _nP_(_l1_,_lN_)
       {var _lO_=_lN_.getLen();
        return _hM_
                (function(_lP_,_l9_)
                  {var _lR_=_bd_(_lQ_,_lP_),_lS_=[0,0];
                   function _ne_(_lU_)
                    {var _lT_=_lS_[1];
                     if(_lT_)
                      {var _lV_=_lT_[1];
                       _ki_(_lR_,_lV_,_bK_(1,_lU_));
                       _lS_[1]=0;
                       return 0;}
                     var _lW_=caml_create_string(1);
                     _lW_.safeSet(0,_lU_);
                     return _kz_(_lR_,1,_lW_);}
                   function _nz_(_lY_)
                    {var _lX_=_lS_[1];
                     return _lX_
                             ?(_ki_(_lR_,_lX_[1],_lY_),_lS_[1]=0,0)
                             :_kz_(_lR_,_lY_.getLen(),_lY_);}
                   function _mh_(_l8_,_lZ_)
                    {var _l0_=_lZ_;
                     for(;;)
                      {if(_lO_<=_l0_)return _bd_(_l1_,_lR_);
                       var _l2_=_lP_.safeGet(_l0_);
                       if(37===_l2_)
                        return _he_(_lP_,_l9_,_l8_,_l0_,_l7_,_l6_,_l5_,_l4_,_l3_);
                       if(64===_l2_)
                        {var _l__=_l0_+1|0;
                         if(_lO_<=_l__)return _l$_(_lP_,_l__);
                         var _ma_=_lP_.safeGet(_l__);
                         if(65<=_ma_)
                          {if(94<=_ma_)
                            {var _mb_=_ma_-123|0;
                             if(!(_mb_<0||2<_mb_))
                              switch(_mb_)
                               {case 1:break;
                                case 2:
                                 if(_lR_[22])_ig_(_lR_,[0,0,5,0]);
                                 if(_lR_[21])
                                  {var _mc_=_lR_[4];
                                   if(_mc_)
                                    {var _md_=_mc_[2];
                                     _bd_(_lR_[26],_mc_[1]);
                                     _lR_[4]=_md_;
                                     var _me_=1;}
                                   else
                                    var _me_=0;}
                                 else
                                  var _me_=0;
                                 _me_;
                                 var _mf_=_l__+1|0,_l0_=_mf_;
                                 continue;
                                default:
                                 var _mg_=_l__+1|0;
                                 if(_lO_<=_mg_)
                                  {_ky_(_lR_,_af_);var _mi_=_mh_(_l8_,_mg_);}
                                 else
                                  if(60===_lP_.safeGet(_mg_))
                                   {var
                                     _mn_=
                                      function(_mj_,_mm_,_ml_)
                                       {_ky_(_lR_,_mj_);return _mh_(_mm_,_mk_(_ml_));},
                                     _mo_=_mg_+1|0,
                                     _my_=
                                      function(_mt_,_mu_,_mr_,_mp_)
                                       {var _mq_=_mp_;
                                        for(;;)
                                         {if(_lO_<=_mq_)
                                           return _mn_
                                                   (_ms_(_cr_(_lP_,_ck_(_mr_),_mq_-_mr_|0),_mt_),_mu_,_mq_);
                                          var _mv_=_lP_.safeGet(_mq_);
                                          if(37===_mv_)
                                           {var
                                             _mw_=_cr_(_lP_,_ck_(_mr_),_mq_-_mr_|0),
                                             _mU_=
                                              function(_mA_,_mx_,_mz_)
                                               {return _my_([0,_mx_,[0,_mw_,_mt_]],_mA_,_mz_,_mz_);},
                                             _mV_=
                                              function(_mH_,_mD_,_mC_,_mG_)
                                               {var _mE_=_mB_?_dN_(_mD_,0,_mC_):_mF_(_mD_,_mC_);
                                                return _my_([0,_mE_,[0,_mw_,_mt_]],_mH_,_mG_,_mG_);},
                                             _mW_=
                                              function(_mO_,_mI_,_mN_)
                                               {if(_mB_)
                                                 var _mJ_=_bd_(_mI_,0);
                                                else
                                                 {var
                                                   _mM_=0,
                                                   _mJ_=_mF_(function(_mK_,_mL_){return _bd_(_mI_,_mK_);},_mM_);}
                                                return _my_([0,_mJ_,[0,_mw_,_mt_]],_mO_,_mN_,_mN_);},
                                             _mX_=function(_mQ_,_mP_){return _lh_(_ag_,_lP_,_mP_);};
                                            return _he_
                                                    (_lP_,
                                                     _l9_,
                                                     _mu_,
                                                     _mq_,
                                                     _mU_,
                                                     _mV_,
                                                     _mW_,
                                                     _mX_,
                                                     function(_mS_,_mT_,_mR_){return _lh_(_ah_,_lP_,_mR_);});}
                                          if(62===_mv_)
                                           return _mn_
                                                   (_ms_(_cr_(_lP_,_ck_(_mr_),_mq_-_mr_|0),_mt_),_mu_,_mq_);
                                          var _mY_=_mq_+1|0,_mq_=_mY_;
                                          continue;}},
                                     _mi_=_my_(0,_l8_,_mo_,_mo_);}
                                  else
                                   {_ky_(_lR_,_ae_);var _mi_=_mh_(_l8_,_mg_);}
                                 return _mi_;}}
                           else
                            if(91<=_ma_)
                             switch(_ma_-91|0)
                              {case 1:break;
                               case 2:_ka_(_lR_,0);var _mZ_=_l__+1|0,_l0_=_mZ_;continue;
                               default:
                                var _m0_=_l__+1|0;
                                if(_lO_<=_m0_)
                                 {_kd_(_lR_,0,4);var _m1_=_mh_(_l8_,_m0_);}
                                else
                                 if(60===_lP_.safeGet(_m0_))
                                  {var _m2_=_m0_+1|0;
                                   if(_lO_<=_m2_)
                                    var _m3_=[0,4,_m2_];
                                   else
                                    {var _m4_=_lP_.safeGet(_m2_);
                                     if(98===_m4_)
                                      var _m3_=[0,4,_m2_+1|0];
                                     else
                                      if(104===_m4_)
                                       {var _m5_=_m2_+1|0;
                                        if(_lO_<=_m5_)
                                         var _m3_=[0,0,_m5_];
                                        else
                                         {var _m6_=_lP_.safeGet(_m5_);
                                          if(111===_m6_)
                                           {var _m7_=_m5_+1|0;
                                            if(_lO_<=_m7_)
                                             var _m3_=_lh_(_aj_,_lP_,_m7_);
                                            else
                                             {var
                                               _m8_=_lP_.safeGet(_m7_),
                                               _m3_=
                                                118===_m8_
                                                 ?[0,3,_m7_+1|0]
                                                 :_lh_(_a0_(_ai_,_bK_(1,_m8_)),_lP_,_m7_);}}
                                          else
                                           var _m3_=118===_m6_?[0,2,_m5_+1|0]:[0,0,_m5_];}}
                                      else
                                       var _m3_=118===_m4_?[0,1,_m2_+1|0]:[0,4,_m2_];}
                                   var
                                    _nb_=_m3_[2],
                                    _m9_=_m3_[1],
                                    _m1_=
                                     _nc_
                                      (_l8_,
                                       _nb_,
                                       function(_m__,_na_,_m$_)
                                        {_kd_(_lR_,_m__,_m9_);return _mh_(_na_,_mk_(_m$_));});}
                                 else
                                  {_kd_(_lR_,0,4);var _m1_=_mh_(_l8_,_m0_);}
                                return _m1_;}}
                         else
                          {if(10===_ma_)
                            {if(_lR_[14]<_lR_[15])_jD_(_lR_,_jB_(0,3,0));
                             var _nd_=_l__+1|0,_l0_=_nd_;
                             continue;}
                           if(32<=_ma_)
                            switch(_ma_-32|0)
                             {case 5:
                              case 32:_ne_(_ma_);var _nf_=_l__+1|0,_l0_=_nf_;continue;
                              case 0:_kB_(_lR_,0);var _ng_=_l__+1|0,_l0_=_ng_;continue;
                              case 12:_kt_(_lR_,0,0);var _nh_=_l__+1|0,_l0_=_nh_;continue;
                              case 14:
                               _km_(_lR_,1);
                               _bd_(_lR_[18],0);
                               var _ni_=_l__+1|0,_l0_=_ni_;
                               continue;
                              case 27:
                               var _nj_=_l__+1|0;
                               if(_lO_<=_nj_)
                                {_kB_(_lR_,0);var _nk_=_mh_(_l8_,_nj_);}
                               else
                                if(60===_lP_.safeGet(_nj_))
                                 {var
                                   _nt_=
                                    function(_nl_,_no_,_nn_)
                                     {return _nc_(_no_,_nn_,_bd_(_nm_,_nl_));},
                                   _nm_=
                                    function(_nq_,_np_,_ns_,_nr_)
                                     {_kt_(_lR_,_nq_,_np_);return _mh_(_ns_,_mk_(_nr_));},
                                   _nk_=_nc_(_l8_,_nj_+1|0,_nt_);}
                                else
                                 {_kB_(_lR_,0);var _nk_=_mh_(_l8_,_nj_);}
                               return _nk_;
                              case 28:
                               return _nc_
                                       (_l8_,
                                        _l__+1|0,
                                        function(_nu_,_nw_,_nv_)
                                         {_lS_[1]=[0,_nu_];return _mh_(_nw_,_mk_(_nv_));});
                              case 31:_kA_(_lR_,0);var _nx_=_l__+1|0,_l0_=_nx_;continue;
                              default:}}
                         return _l$_(_lP_,_l__);}
                       _ne_(_l2_);
                       var _ny_=_l0_+1|0,_l0_=_ny_;
                       continue;}}
                   function _l7_(_nC_,_nA_,_nB_)
                    {_nz_(_nA_);return _mh_(_nC_,_nB_);}
                   function _l6_(_nG_,_nE_,_nD_,_nF_)
                    {if(_mB_)_nz_(_dN_(_nE_,0,_nD_));else _dN_(_nE_,_lR_,_nD_);
                     return _mh_(_nG_,_nF_);}
                   function _l5_(_nJ_,_nH_,_nI_)
                    {if(_mB_)_nz_(_bd_(_nH_,0));else _bd_(_nH_,_lR_);
                     return _mh_(_nJ_,_nI_);}
                   function _l4_(_nL_,_nK_)
                    {_kA_(_lR_,0);return _mh_(_nL_,_nK_);}
                   function _l3_(_nN_,_nQ_,_nM_)
                    {return _nP_(function(_nO_){return _mh_(_nN_,_nM_);},_nQ_);}
                   function _nc_(_oe_,_nR_,_nZ_)
                    {var _nS_=_nR_;
                     for(;;)
                      {if(_lO_<=_nS_)return _lq_(_lP_,_nS_);
                       var _nT_=_lP_.safeGet(_nS_);
                       if(32===_nT_){var _nU_=_nS_+1|0,_nS_=_nU_;continue;}
                       if(37===_nT_)
                        {var
                          _oa_=
                           function(_nY_,_nW_,_nX_)
                            {return _di_(_nZ_,_nV_(_lP_,_nX_,_nW_),_nY_,_nX_);},
                          _ob_=function(_n1_,_n2_,_n3_,_n0_){return _lq_(_lP_,_n0_);},
                          _oc_=function(_n5_,_n6_,_n4_){return _lq_(_lP_,_n4_);},
                          _od_=function(_n8_,_n7_){return _lq_(_lP_,_n7_);};
                         return _he_
                                 (_lP_,
                                  _l9_,
                                  _oe_,
                                  _nS_,
                                  _oa_,
                                  _ob_,
                                  _oc_,
                                  _od_,
                                  function(_n__,_n$_,_n9_){return _lq_(_lP_,_n9_);});}
                       var _of_=_nS_;
                       for(;;)
                        {if(_lO_<=_of_)
                          var _og_=_lq_(_lP_,_of_);
                         else
                          {var
                            _oh_=_lP_.safeGet(_of_),
                            _oi_=48<=_oh_?58<=_oh_?0:1:45===_oh_?1:0;
                           if(_oi_){var _oj_=_of_+1|0,_of_=_oj_;continue;}
                           var
                            _ok_=
                             _of_===_nS_
                              ?0
                              :_nV_(_lP_,_of_,_cr_(_lP_,_ck_(_nS_),_of_-_nS_|0)),
                            _og_=_di_(_nZ_,_ok_,_oe_,_of_);}
                         return _og_;}}}
                   function _mk_(_ol_)
                    {var _om_=_ol_;
                     for(;;)
                      {if(_lO_<=_om_)return _l$_(_lP_,_om_);
                       var _on_=_lP_.safeGet(_om_);
                       if(32===_on_){var _oo_=_om_+1|0,_om_=_oo_;continue;}
                       return 62===_on_?_om_+1|0:_l$_(_lP_,_om_);}}
                   return _mh_(_ck_(0),0);},
                 _lN_);}
      return _nP_;}
    function _ox_(_or_)
     {var _op_=_cc_(512);
      function _ot_(_oq_){return _bd_(_or_,_lA_(_op_,_oq_));}
      return _di_(_ou_,1,function(_os_){return _k7_(_op_);},_ot_);}
    function _oz_(_ow_){return _dN_(_ox_,function(_ov_){return _ov_;},_ow_);}
    var _oA_=_bc_[1];
    _bc_[1]=function(_oB_){_bd_(_oy_,0);return _bd_(_oA_,0);};
    var _oC_=null,_oD_=undefined;
    function _oF_(_oE_){return 1-(_oE_==_oC_?1:0);}
    var _oG_=false,_oH_=Date,_oJ_=Array;
    function _oK_(_oI_)
     {return _oI_ instanceof _oJ_?0:[0,new MlWrappedString(_oI_.toString())];}
    _h2_[1]=[0,_oK_,_h2_[1]];
    function _oN_(_oL_,_oM_){_oL_.appendChild(_oM_);return 0;}
    var
     _oO_=this,
     _oP_=_oO_.document,
     _oV_=this.Int16Array,
     _oU_=this.Int32Array,
     _oT_=this.Float32Array;
    function _oS_(_oR_,_oQ_){return _oR_.createElement(_oQ_.toString());}
    var _oW_=[0,_O_];
    this.HTMLElement===_oD_;
    var
     _pa_=caml_js_get_console(0),
     _pc_=
      caml_js_pure_expr
       (function(_o$_)
         {var
           _oX_=
            [0,
             _oO_.requestAnimationFrame,
             [0,
              _oO_.mozRequestAnimationFrame,
              [0,
               _oO_.webkitRequestAnimationFrame,
               [0,
                _oO_.oRequestAnimationFrame,
                [0,_oO_.msRequestAnimationFrame,0]]]]];
          try
           {var _oY_=_oX_;
            for(;;)
             {if(!_oY_)throw [0,_c_];
              var _oZ_=_oY_[1],_o2_=_oY_[2];
              if(_oZ_===_oD_){var _oY_=_o2_;continue;}
              var _o1_=function(_o0_){return _oZ_(_o0_);};
              break;}}
          catch(_o3_)
           {if(_o3_[1]===_c_)
             {var
               _o5_=function(_o4_){return new _oH_().getTime();},
               _o6_=[0,_o5_(0)];
              return function(_o__)
               {var _o7_=_o5_(0),_o8_=_o6_[1]+1000/60-_o7_,_o9_=_o8_<0?0:_o8_;
                _o6_[1]=_o7_;
                _oO_.setTimeout(_o__,_o9_);
                return 0;};}
            throw _o3_;}
          return _o1_;}),
     _pb_=_oS_(_oP_,_P_);
    _pb_.style.position=_o_.toString();
    _pb_.style.top=_n_.toString();
    _pb_.style.left=_m_.toString();
    _pb_.style.lineHeight=_l_.toString();
    _pb_.style.color=_k_.toString();
    var _pd_=_oS_(_oP_,_Q_);
    _oN_(_pb_,_pd_);
    function _pf_(_pe_){return new _oH_().getTime();}
    var _pg_=1;
    function _q7_(_pk_,_pl_,_pj_,_pq_,_ph_,_pn_)
     {var
       _pi_=_ph_?_ph_[1]:0,
       _pm_=_pk_.getAttribLocation(_pl_,_pj_.toString()),
       _po_=_pk_.createBuffer(),
       _pp_=new _oT_(caml_js_from_array(_pn_));
      _pk_.bindBuffer(_pk_.ARRAY_BUFFER,_po_);
      _pk_.bufferData(_pk_.ARRAY_BUFFER,_pp_,_pk_.STATIC_DRAW);
      _pk_.enableVertexAttribArray(_pm_);
      return _pk_.vertexAttribPointer(_pm_,_pq_,_pk_.FLOAT,_oG_,0,_pi_);}
    function _rn_(_rm_)
     {var _pr_=_oS_(_oO_.document,_R_),_pt_=600,_ps_=1000;
      if(_oF_(_pr_.getContext))
       {_pr_.width=_ps_;
        _pr_.height=_pt_;
        _oN_(_oP_.body,_pr_);
        var _pu_=_pr_.getContext(_q_.toString(),{"antialias":_oG_});
        if(_pu_==_oC_)throw [0,_d_,_M_];
        var _pv_=_oF_(_pu_.getExtension(_L_.toString()));
        _pu_.viewport(0,0,_pr_.width,_pr_.height);
        if(_pg_)_pu_.clearColor(1,1,1,1);else _pu_.clearColor(0,0,0,0);
        var _pw_=_pu_.createShader(_pu_.VERTEX_SHADER);
        _pu_.shaderSource(_pw_,_j_.toString());
        _pu_.compileShader(_pw_);
        var _px_=_pu_.createShader(_pu_.FRAGMENT_SHADER);
        _pu_.shaderSource(_px_,_i_.toString());
        _pu_.compileShader(_px_);
        var _py_=_pu_.createProgram();
        _pu_.attachShader(_py_,_pw_);
        _pu_.attachShader(_py_,_px_);
        _pu_.linkProgram(_py_);
        if(1-(_pu_.getShaderParameter(_pw_,_pu_.COMPILE_STATUS)|0))
         _pa_.log(_pu_.getShaderInfoLog(_pw_));
        if(1-(_pu_.getShaderParameter(_px_,_pu_.COMPILE_STATUS)|0))
         _pa_.log(_pu_.getShaderInfoLog(_px_));
        if(1-(_pu_.getProgramParameter(_py_,_pu_.LINK_STATUS)|0))
         _pa_.log(_pu_.getProgramInfoLog(_py_));
        _pu_.useProgram(_py_);
        var _pz_=_pu_.getUniformLocation(_py_,_K_.toString());
        _pu_.uniform2f(_pz_,2/_pr_.width,2/_pr_.height);
        var _pA_=_pu_.getUniformLocation(_py_,_J_.toString());
        _pu_.uniformMatrix3fv(_pA_,_oG_,caml_js_from_array(_I_.slice()));
        if(_pg_)
         _pu_.blendFunc(_pu_.ONE,_pu_.ONE_MINUS_SRC_ALPHA);
        else
         _pu_.blendFunc(_pu_.ONE_MINUS_DST_ALPHA,_pu_.ONE);
        _pu_.enable(_pu_.BLEND);
        var _pB_=_pr_.height,_pC_=scene,_pD_=0;
        _pa_.log(_dN_(_h1_,_p_,_pC_.length-1).toString());
        var _pE_=0,_pF_=_pC_.length-1-1|0;
        if(_pF_<_pE_)
         var _pG_=_pD_;
        else
         {var _pH_=_pE_,_pI_=_pD_;
          for(;;)
           {var _pJ_=caml_array_get(_pC_,_pH_);
            if(caml_array_get(_pJ_,0)==0)
             {var
               _pK_=caml_array_get(_pJ_,4),
               _pL_=
                [254,
                 caml_array_get(_pJ_,1),
                 caml_array_get(_pJ_,2),
                 caml_array_get(_pJ_,3),
                 1],
               _pM_=caml_array_get(_pJ_,5)==1?1:0,
               _pN_=caml_array_get(_pJ_,6),
               _pO_=_pN_!=1?_pN_!=2?-752251131:271466201:724911760,
               _pP_=caml_array_get(_pJ_,7),
               _pQ_=_pP_!=1?_pP_!=2?192195741:-346804178:736157203,
               _pR_=
                [0,
                 caml_array_get(_pJ_,8),
                 _pB_-caml_array_get(_pJ_,9),
                 _pQ_,
                 _pK_,
                 _pL_,
                 _pL_],
               _pS_=[0,_pR_,0],
               _pT_=0,
               _pU_=_pM_?1:2,
               _pV_=((_pJ_.length-1-10|0)/2|0)-_pU_|0;
              if(_pV_<_pT_)
               var _pW_=_pS_;
              else
               {var _pX_=_pT_,_pY_=_pS_;
                for(;;)
                 {var
                   _pZ_=
                    [0,
                     [0,
                      caml_array_get(_pJ_,(2*_pX_|0)+10|0),
                      _pB_-caml_array_get(_pJ_,(2*_pX_|0)+11|0),
                      _pO_,
                      _pK_,
                      _pL_,
                      _pL_],
                     _pY_],
                   _p0_=_pX_+1|0;
                  if(_pV_!==_pX_){var _pX_=_p0_,_pY_=_pZ_;continue;}
                  var _pW_=_pZ_;
                  break;}}
              var
               _p1_=_pJ_.length-1,
               _p2_=
                _pM_
                 ?[0,_pR_,_pW_]
                 :[0,
                   [0,
                    caml_array_get(_pJ_,_p1_-2|0),
                    _pB_-caml_array_get(_pJ_,_p1_-1|0),
                    _pQ_,
                    _pK_,
                    _pL_,
                    _pL_],
                   _pW_],
               _p3_=[0,_p2_,_pI_];}
            else
             var _p3_=_pI_;
            var _p4_=_pH_+1|0;
            if(_pF_!==_pH_){var _pH_=_p4_,_pI_=_p3_;continue;}
            var _pG_=_p3_;
            break;}}
        var _p5_=_bw_(_pG_),_p6_=_pg_?_p5_:_bw_(_p5_),_p7_=0,_p8_=_p6_;
        for(;;)
         {if(_p8_)
           {var _p9_=_p8_[2],_p__=_p7_+_bv_(_p8_[1])|0,_p7_=_p__,_p8_=_p9_;
            continue;}
          var _p$_=_p7_-_bv_(_p6_)|0;
          _pa_.log(_dN_(_oz_,_H_,_p7_).toString());
          _pa_.log(_dN_(_oz_,_G_,_p$_).toString());
          var _qa_=caml_make_vect((8*_p7_|0)+16|0,0),_qb_=[0,8];
          _bx_
           (function(_qg_)
             {return _bx_
                      (function(_qc_)
                        {var _qd_=_qc_[2],_qe_=_qc_[1],_qf_=_qb_[1];
                         _qb_[1]=_qf_+8|0;
                         caml_array_set(_qa_,_qf_+0|0,_qe_);
                         caml_array_set(_qa_,_qf_+1|0,_qd_);
                         caml_array_set(_qa_,_qf_+2|0,_qe_);
                         caml_array_set(_qa_,_qf_+3|0,_qd_);
                         caml_array_set(_qa_,_qf_+4|0,_qe_);
                         caml_array_set(_qa_,_qf_+5|0,_qd_);
                         caml_array_set(_qa_,_qf_+6|0,_qe_);
                         return caml_array_set(_qa_,_qf_+7|0,_qd_);},
                       _qg_);},
            _p6_);
          var _qh_=caml_make_vect(16*_p7_|0,0),_qi_=[0,0];
          _bx_
           (function(_qr_)
             {return _bx_
                      (function(_qj_)
                        {var _qk_=_qj_[6],_ql_=_qj_[5],_qm_=0,_qn_=1;
                         if(!(_qn_<_qm_))
                          {var _qo_=_qm_;
                           for(;;)
                            {var _qp_=_qi_[1];
                             _qi_[1]=_qp_+8|0;
                             caml_array_set(_qh_,_qp_+0|0,caml_array_get(_ql_,0));
                             caml_array_set(_qh_,_qp_+1|0,caml_array_get(_ql_,1));
                             caml_array_set(_qh_,_qp_+2|0,caml_array_get(_ql_,2));
                             caml_array_set(_qh_,_qp_+3|0,caml_array_get(_ql_,3));
                             caml_array_set(_qh_,_qp_+4|0,caml_array_get(_qk_,0));
                             caml_array_set(_qh_,_qp_+5|0,caml_array_get(_qk_,1));
                             caml_array_set(_qh_,_qp_+6|0,caml_array_get(_qk_,2));
                             caml_array_set(_qh_,_qp_+7|0,caml_array_get(_qk_,3));
                             var _qq_=_qo_+1|0;
                             if(_qn_!==_qo_){var _qo_=_qq_;continue;}
                             break;}}
                         return 0;},
                       _qr_);},
            _p6_);
          var _qs_=caml_make_vect(4*_p7_|0,0),_qt_=[0,0];
          _bx_
           (function(_qB_)
             {return _bx_
                      (function(_qu_)
                        {var _qv_=_qt_[1],_qw_=_qu_[4];
                         _qt_[1]=_qv_+4|0;
                         var _qx_=0,_qy_=3;
                         if(!(_qy_<_qx_))
                          {var _qz_=_qx_;
                           for(;;)
                            {caml_array_set(_qs_,_qv_+_qz_|0,_qw_);
                             var _qA_=_qz_+1|0;
                             if(_qy_!==_qz_){var _qz_=_qA_;continue;}
                             break;}}
                         return 0;},
                       _qB_);},
            _p6_);
          var _qC_=caml_make_vect(4*_p7_|0,0),_qD_=[0,0];
          _bx_
           (function(_qM_)
             {return _bx_
                      (function(_qE_)
                        {var _qF_=_qE_[3],_qG_=_qD_[1];
                         _qD_[1]=_qG_+4|0;
                         var
                          _qH_=
                           192195741<=_qF_
                            ?724911760<=_qF_?736157203<=_qF_?1:-1:271466201<=_qF_?-2:3
                            :-346804178<=_qF_?2:-3,
                          _qI_=0,
                          _qJ_=3;
                         if(!(_qJ_<_qI_))
                          {var _qK_=_qI_;
                           for(;;)
                            {caml_array_set(_qC_,_qG_+_qK_|0,_qH_);
                             var _qL_=_qK_+1|0;
                             if(_qJ_!==_qK_){var _qK_=_qL_;continue;}
                             break;}}
                         return 0;},
                       _qM_);},
            _p6_);
          var _qN_=caml_make_vect(4*(_p7_-1|0)|0,1),_qO_=0,_qP_=_p7_-2|0;
          if(!(_qP_<_qO_))
           {var _qQ_=_qO_;
            for(;;)
             {caml_array_set(_qN_,(4*_qQ_|0)+1|0,-1);
              caml_array_set(_qN_,(4*_qQ_|0)+3|0,-1);
              var _qR_=_qQ_+1|0;
              if(_qP_!==_qQ_){var _qQ_=_qR_;continue;}
              break;}}
          var _qS_=caml_make_vect(4*(_p7_-1|0)|0,1),_qT_=0,_qU_=_p7_-2|0;
          if(!(_qU_<_qT_))
           {var _qV_=_qT_;
            for(;;)
             {caml_array_set(_qS_,(4*_qV_|0)+2|0,-1);
              caml_array_set(_qS_,(4*_qV_|0)+3|0,-1);
              var _qW_=_qV_+1|0;
              if(_qU_!==_qV_){var _qV_=_qW_;continue;}
              break;}}
          var _qX_=caml_make_vect(6*_p$_|0,0),_qY_=[0,0],_qZ_=[0,0];
          _bx_
           (function(_q0_)
             {var _q1_=0,_q2_=(_bv_(_q0_)-1|0)-1|0;
              if(!(_q2_<_q1_))
               {var _q3_=_q1_;
                for(;;)
                 {var _q4_=_qY_[1];
                  _qY_[1]=_q4_+6|0;
                  var _q5_=_qZ_[1];
                  _qZ_[1]=_q5_+4|0;
                  caml_array_set(_qX_,_q4_+0|0,_q5_+0|0);
                  caml_array_set(_qX_,_q4_+1|0,_q5_+1|0);
                  caml_array_set(_qX_,_q4_+2|0,_q5_+2|0);
                  caml_array_set(_qX_,_q4_+3|0,_q5_+2|0);
                  caml_array_set(_qX_,_q4_+4|0,_q5_+1|0);
                  caml_array_set(_qX_,_q4_+5|0,_q5_+3|0);
                  var _q6_=_q3_+1|0;
                  if(_q2_!==_q3_){var _q3_=_q6_;continue;}
                  break;}}
              _qZ_[1]=_qZ_[1]+4|0;
              return 0;},
            _p6_);
          _pa_.log(_qX_.length-1);
          _q7_(_pu_,_py_,_E_,2,_F_,_qa_);
          _q7_(_pu_,_py_,_C_,2,_D_,_qa_);
          _q7_(_pu_,_py_,_A_,2,_B_,_qa_);
          _q7_(_pu_,_py_,_y_,2,_z_,_qa_);
          _q7_(_pu_,_py_,_x_,1,[0,2*4|0],_qs_);
          _q7_(_pu_,_py_,_w_,1,0,_qS_);
          _q7_(_pu_,_py_,_v_,1,0,_qN_);
          _q7_(_pu_,_py_,_u_,4,[0,(4*2|0)*4|0],_qh_);
          _q7_(_pu_,_py_,_t_,1,0,_qC_);
          _q7_(_pu_,_py_,_r_,1,_s_,_qC_);
          var _q8_=_pu_.createBuffer();
          _pu_.bindBuffer(_pu_.ELEMENT_ARRAY_BUFFER,_q8_);
          if(_pv_)
           {var _q9_=new _oU_(caml_js_from_array(_qX_));
            _pu_.bufferData(_pu_.ELEMENT_ARRAY_BUFFER,_q9_,_pu_.STATIC_DRAW);}
          else
           {var _q__=new _oV_(caml_js_from_array(_qX_));
            _pu_.bufferData(_pu_.ELEMENT_ARRAY_BUFFER,_q__,_pu_.STATIC_DRAW);}
          _pa_.log(_pu_.getContextAttributes());
          _pa_.log(_pu_.getParameter(_pu_.SAMPLES));
          var
           _q$_=[0,0],
           _ra_=[0,_pf_(0)],
           _rb_=[0,15],
           _rc_=[0,15],
           _rd_=[0,0],
           _rg_=
            function(_rl_)
             {var _re_=_pf_(0);
              _rb_[1]=0.9*_rb_[1]+0.1*(_re_-_ra_[1]);
              _rd_[1]+=1;
              if(0===(_rd_[1]%60|0))
               {var _rf_=_di_(_oz_,_N_,1000/_rb_[1],_rc_[1]);
                _oN_(_oP_.body,_pb_);
                _pd_.innerHTML=_rf_.toString();}
              _ra_[1]=_re_;
              _bd_(_pc_,caml_js_wrap_callback(_rg_));
              _q$_[1]=_q$_[1]+1;
              var _rh_=0.5*(1.8+0.8*Math.cos(_q$_[1]*3.14/180));
              _pu_.uniformMatrix3fv
               (_pA_,_oG_,caml_js_from_array([254,_rh_,0,0,0,_rh_,0,0,0,1]));
              _pu_.clear(_pu_.COLOR_BUFFER_BIT);
              var _rj_=0,_ri_=_pv_?_pu_.UNSIGNED_INT:_pu_.UNSIGNED_SHORT;
              _pu_.drawElements(_pu_.TRIANGLES,_qX_.length-1,_ri_,_rj_);
              _pu_.finish();
              var _rk_=0.1*(_pf_(0)-_re_);
              _rc_[1]=0.9*_rc_[1]+_rk_;
              return 0;};
          _rg_(0);
          return _oG_;}}
      throw [0,_oW_];}
    _oO_.onload=
    caml_js_wrap_callback
     (function(_ro_)
       {if(_ro_)
         {var _rp_=_rn_(_ro_);if(!(_rp_|0))_ro_.preventDefault();return _rp_;}
        var _rq_=event,_rr_=_rn_(_rq_);
        _rq_.returnValue=_rr_;
        return _rr_;});
    _bf_(0);
    return;}
  ());
