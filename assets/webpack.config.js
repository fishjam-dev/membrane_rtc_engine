const path = require("path");

module.exports = {
  entry: "./js/index.ts",
  output: {
    filename: "index.js",
    path: path.resolve(__dirname, "../priv/static"),
    library: "membrane-sfu",
    libraryTarget: "umd",
    globalObject: "this",
  },
  resolve: {
    extensions: [".ts", ".js"],
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        loader: "ts-loader",
        exclude: /node_modules/,
      },
    ],
  },
};
