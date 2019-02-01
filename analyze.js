var fs = require('fs');
var s = fs.readFileSync("./PetroffMain.png").toString();

var lines = s.split("\n");

var i =0;
var ii = lines.length;
var line;
var gs;

var makeFreshGameState = function () {
 var g = [["wR","wKN","wB","wQ","wK","wB","wKN","wR"],["wp","wp","wp","wp","wp","wp","wp","wp"],[],[],[],[],["wp","wp","wp","wp","wp","wp","wp","wp"],[["wR","wKN","wB","wQ","wK","wB","wKN","wR"]
};
