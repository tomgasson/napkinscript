module.exports = {
  setupFiles: ["<rootDir>/tests/runner.js"],
  testRegex: "parse\\.spec\\.js$",
  testEnvironment: "node",
  resolver: require.resolve("jest-pnp-resolver")
};
