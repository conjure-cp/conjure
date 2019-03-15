import * as d3 from "d3";
import Node from './Node';

export default class Temp {

    private duration: number;
    private viewerWidth: number;
    private viewerHeight: number;
    private margin = { top: 40, right: 30, bottom: 50, left: 30 };
    private width: number;
    private height: number;
    private nodeHeight: number;
    private tree: any;
    private svg: SVGSVGElement;

    public constructor(width: number, height: number){
        this.width = width;
        this.height = height;

        d3.select("body")
        .append("svg")
        // .call(d3.zoom() .on("zoom", ()))
        .attr("id", "theTree")
        .attr("width", this.width)
        .attr("height", this.height)
        .append("g");
    }


    // private tree = d3.layout.tree().size([Tree.height, Tree.width]) .nodeSize([200, 0]);



}