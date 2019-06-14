/**
 * Fake function used so that compilation doesn't fail during testing.
 */

function acquireVsCodeApi() {
  let obj = {};
  obj.postMessage = function(message) {};
  return obj;
}
