import * as d3 from "d3"

let d3Tree: any = {};
d3Tree.create = function (el: any, props: any, state: any) {
    let svg: any = d3.select(el).append('svg')
        // .call(d3.behavior.zoom().on("zoom", function () {
        //     svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
        // }))
        .attr('width', props.width)
        .attr('height', props.height)

    svg = svg.append("g").attr("id", "poop")

    this.width = props.width;
    this.height = props.height;

    this.update(el, state);
}

d3Tree.update = function (el: any, state: any) {
    this._drawTree(el, state.data);
};

d3Tree._drawTree = function (el: any, data: any) {
    let tree = d3.tree().size([500, 250])
    let svg = d3.select(el).select('svg g')
    let rootNode = d3.hierarchy(data)
    let nodes = tree(rootNode).descendants()
    let g = svg.selectAll('g.node')
    let node = g.data(nodes)
    node.enter().append('g')
        .attr('class', 'node')
        .attr('transform', (d) => {
            return `translate(${d.x},${d.y + 10})`
        })
        .append("svg:circle")
        .attr("r", 6)

    node.transition().duration(0).attr('transform', (d) => `translate(${d.x},${d.y})`);

    node.exit().remove()

    let p = svg.selectAll('path.link');
    let links = tree(rootNode).links()
    let link = p.data(links)
    link.enter().insert("svg:path", "g")
        .attr('class', 'link')
        .attr('d', drawLink);

    link.transition().attr('d', drawLink)

    link.exit().remove()
}

const drawLink = (d: any) => {
    if (d.children && d.children.length > 0) {
        return "M" + d.y + "," + d.x
            + "C" + (d.y + d.parent.y) / 2 + "," + d.x
            + " " + (d.y + d.parent.y) / 2 + "," + d.parent.x
            + " " + d.parent.y + "," + d.parent.x;
    }

    return null
}