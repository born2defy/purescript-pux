exports.sendToPuxAttr = function(mapEvt){
  var send = function(a){ return mapEvt(a); }
  return new PuxSendString(send);
};

function PuxSendString (send) {
  this.send = send;
};

PuxSendString.prototype.toString = function () {
  return '';
};
