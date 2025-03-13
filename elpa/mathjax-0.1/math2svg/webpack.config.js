const path = require("path")

module.exports = {
  target: "node",
  entry: "./main.js",
  output: {
    filename: "math2svg.js",
    path: path.resolve(__dirname, ".."),
  },
}
