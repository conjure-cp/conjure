module.exports = {
  moduleFileExtensions: ["ts", "tsx", "js"],
  transform: {
    "^.+\\.(ts|tsx)$": "ts-jest"
  },
  globals: {
    "ts-jest": {
      tsConfig: "tsconfig.json"
    }
  },
  testMatch: ["**/src/webclient/__tests__/*.+(ts|tsx|js)"],
  automock: false,
  setupFiles: ["./src/webclient/src/setupJest.ts"],
  globals: {
    "ts-jest": {
      diagnostics: { warnOnly: true }
    }
  }
}
