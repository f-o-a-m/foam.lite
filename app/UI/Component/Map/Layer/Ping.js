const PingLayer =  require("~/ping-layer");

exports.makePingLayer = function (props) {
    return new PingLayer.default(props);
}

exports.defaultPingProps = PingLayer.default.defaultProps