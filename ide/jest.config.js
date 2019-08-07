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
  preset: "ts-jest",
  testMatch: ["**/__tests__/*.+(ts|tsx)"],
  automock: false,
  setupFiles: ["./src/webclient/src/setupJest.ts"],
  globals: {
    "ts-jest": {
      diagnostics: { warnOnly: true }
    }
  },
  moduleNameMapper: {
    "\\.(css|less)$": "identity-obj-proxy"
  }
}
