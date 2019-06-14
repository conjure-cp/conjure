declare var jsPanel: any;
declare var d3: any;

export function showLegend() {
  console.log("here");
  let h = 50;
  let w = 50;
  let panel = jsPanel.create({
    theme:
      getComputedStyle(document.documentElement).getPropertyValue(
        "--vscode-editor-background"
      ) + " filled",
    headerTitle: "Legend",
    position: "left-top 0 58",
    contentSize: {
      width: () => {
        return 550;
      },
      height: () => {
        return 500;
      }
    },
    content: `
        <div id="legendPane"> </div>`
  });

  $("#wantSVG").prop("checked", true);

  let svg = d3
    .select("#legendPane")
    .append("svg")
    .attr("width", 550)
    .attr("height", 500);

  let list = [
    {
      kind: "",
      r: 10,
      x: w,
      y: h,
      text: "Node"
    },
    {
      kind: "selected",
      r: 10,
      x: w,
      y: h + 40,
      text: "Selected Node"
    },
    {
      kind: "solution",
      r: 10,
      x: w,
      y: h + 80,
      text: "Solution Node"
    },
    {
      kind: "hasOthers",
      r: 25,
      x: w,
      y: h + 140,
      text: "Node in solution branch with hidden children"
    },
    {
      kind: "hasOthers red",
      r: 25,
      x: w,
      y: h + 210,
      text: "Node in failed branch with hidden children"
    }
  ];

  let elements = svg.selectAll("g.node").data(list);
  let nodeEnter = elements
    .enter()
    .append("g")
    .attr("class", "node");

  nodeEnter
    .append("circle")
    .attr("class", (d: any) => d.kind)
    .attr("r", (d: any) => d.r)
    .attr("cx", (d: any) => d.x)
    .attr("cy", (d: any) => d.y)
    .style("stroke", (d: any) => {
      if (d.kind === "selected") return "coral";
    });

  nodeEnter
    .append("text")
    .attr("y", (d: any) => d.y)
    .attr("x", (d: any) => d.x + 50)
    .attr("dy", ".35em")
    .attr("text-anchor", "left")
    .text((d: any) => d.text);

  // let linkDomObjects = svg.selectAll("path.link").data(links);

  let diagonal = d3.svg.diagonal().projection((d: any) => {
    return [d.x, d.y];
  });

  svg
    .append("g")
    .attr("class", "node")
    .attr("id", "l1")
    .append("path")
    .attr("class", "link")
    .attr(
      "d",
      d3.svg
        .diagonal()
        .source({ x: w, y: h + 260 })
        .target({ x: w + 80, y: h + 400 })
    );

  d3.select("#l1")
    .append("text")
    .attr("y", h + 330)
    .attr("x", w + 80)
    .attr("dy", ".35em")
    .attr("text-anchor", "left")
    .text("Solution branch");

  svg
    .append("g")
    .attr("class", "node")
    .attr("id", "l2")
    .append("path")
    .attr("class", "link red")
    .attr(
      "d",
      d3.svg
        .diagonal()
        .source({ x: w + 250, y: h + 260 })
        .target({ x: w + 330, y: h + 400 })
    );

  d3.select("#l2")
    .append("text")
    .attr("y", h + 330)
    .attr("x", w + 320)
    .attr("dy", ".35em")
    .attr("text-anchor", "left")
    .text("Failed branch");
}
