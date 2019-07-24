"use strict";

exports.draw = function(element) {
  return function(picture) {
    return function() {
      var ctx = element.getContext("2d")
      ctx.moveTo(0,0);
      ctx.lineTo(300,300);
      ctx.stroke();
    }
  };
};
