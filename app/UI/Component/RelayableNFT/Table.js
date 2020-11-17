exports.ellipsize = function(startChars) {
  return function(endChars) {
    return function(rawStr) {
      const str = rawStr.indexOf("0x") == 0 || rawStr.indexOf("0X") == 0 ? rawStr.substr(2) : rawStr;
      const l = str.length;
      if (startChars > l || endChars > l || startChars + endChars >= l) {
        return str;
      } else {
        const s = str.substring(0, startChars);
        const e = str.substring(l - endChars);
        return s + "…" + e;
      }
    }
  }
}