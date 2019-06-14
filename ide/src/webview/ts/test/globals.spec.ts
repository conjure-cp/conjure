import Globals from "../src/modules/Globals";
import State from "../src/modules/State";
import * as sinon from "sinon";

declare var d3: any;

var vscodeApi: any = {
  postMessage: function(message: any) {
    return;
  }
};
var spy: any = sinon.spy(vscodeApi, "postMessage");
let prettyMessage = {
  command: "prettyDomains",
  nodeId: 0,
  wantExpressions: false,
  paths: ""
};
let simpleMessage = {
  command: "simpleDomains",
  nodeId: 0,
  wantExpressions: false
};
let loadNodeMessage = { command: "loadNodes", amount: 1, start: 0 };

describe("Test the Globals class", function() {
  // var dom: any;
  beforeEach(function() {
    State.id2Node = {};
    State.solAncestorIds = [];
    State.solNodIds = [];
    State.rootId = 0;
    State.selectedId = State.rootId;
    State.waiting = false;
    State.pretty = true;

    $("body").empty();

    d3.select("body")
      .append("input")
      .attr("checked", true)
      .attr("id", "expressions");
  });

  afterEach(function() {
    spy.resetHistory();
  });

  describe("Initialising", function() {
    it("should make two calls to the api", function() {
      Globals.initialize(vscodeApi);

      // vscodeApiSpy.postMessage();

      let initMessage = { command: "init" };
      let lbv = { command: "longestBranchingVariable" };

      sinon.assert.calledTwice(spy);
      sinon.assert.calledWith(spy, initMessage);
      sinon.assert.calledWith(spy, lbv);
    });
  });

  describe("Send pretty request", function() {
    it("should  make appropriate call to api", function() {
      Globals.sendPrettyRequest(vscodeApi);
      sinon.assert.calledOnce(spy);
      sinon.assert.calledWith(spy, prettyMessage);
    });
  });

  describe("Send simple request", function() {
    it("should  make appropriate call to api", function() {
      Globals.sendSimpleRequest(vscodeApi);
      sinon.assert.calledOnce(spy);
      sinon.assert.calledWith(spy, simpleMessage);
    });
  });

  describe("Load nodes", function() {
    it("Should send the request to the api", function() {
      Globals.loadNodes(vscodeApi);

      expect(State.waiting);

      sinon.assert.called(spy);
      sinon.assert.calledWith(spy, loadNodeMessage);
    });

    it("Should not send the request to the api if waiting", function() {
      State.waiting = true;

      Globals.loadNodes(vscodeApi);

      expect(State.waiting);

      sinon.assert.notCalled(spy);
    });
  });

  describe("Call loadDomains ", function() {
    it("should call loadPretty", function() {
      Globals.loadDomains(vscodeApi);

      sinon.assert.called(spy);
      sinon.assert.calledWith(spy, prettyMessage);
    });

    it("should call loadSimple", function() {
      State.pretty = false;

      Globals.loadDomains(vscodeApi);
      sinon.assert.called(spy);
      sinon.assert.calledWith(spy, simpleMessage);
    });
  });

  describe("Call upNode ", function() {
    it("should not go up because we are at the root", function() {});

    it("should call loadSimple", function() {
      State.pretty = false;

      Globals.loadDomains(vscodeApi);
      sinon.assert.called(spy);
      sinon.assert.calledWith(spy, simpleMessage);
    });
  });
});
