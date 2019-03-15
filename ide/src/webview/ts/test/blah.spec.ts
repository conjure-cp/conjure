import "jasmine";
import * as d3 from "d3";
import Temp from '../src/testable/Temp';


describe('Test D3.js with jasmine ', function () {
    // var dom: any;

    // beforeEach(function() {
    // //   c = barChart();
    // //   c.render();
    //     dom = new JSDOM(`<!DOCTYPE html><p>Hello world</p>`);
    // });

    // afterEach(function() {
    //   d3.selectAll('svg').remove();
    // });

    describe('the svg', function () {
        it('should be created', function () {
            // d3.select("body").append("h1").text("ASDASDADASAD");
            // console.log("HERE")
            let t = new Temp(50, 100);
            expect($("#theTree").width()).toBe(50);
            expect($("#theTree").height()).toBe(100);
            

            // expect(d3.select("#theTree"))
            // console.log(Tree.duration);

            // expect(getSvg()).not.toBeNull();
        });
    });

    function getSvg() {
      return d3.select('h1');
    }

});
