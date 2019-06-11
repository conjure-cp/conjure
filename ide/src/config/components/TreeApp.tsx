import * as React from "react"
import * as ReactDOM from "react-dom"

// import TreeChart from "./TreeChart"

interface State {
    data : any
}

class App extends React.Component<any, State> {

    constructor(props: any) {
        super(props)
        this.state = {
            data: {
                "children": [{
                    "children": [{}, {}]
                }, {
                    "children": [{}, {}, {}]
                }, {
                    "children": [{
                        "children": [{}, {}]
                    }]
                }]
            }
        };
        this.addNode = this.addNode.bind(this);
        this.removeNode = this.removeNode.bind(this);
    }

    addNode() {
        this.state.data.children = this.state.data.children || [];
        this.state.data.children.push({});
        this.setState({
            data: this.state.data
        });
    }

    removeNode() {
        this.state.data.children.pop();
        this.setState({
            data: this.state.data
        });
    }
    render() {
        return (
            <div className="App">
                <TreeChart data={this.state.data} />
                <button onClick={this.addNode}>add node</button>
                <button onClick={this.removeNode}>remove node</button>
            </div>
        );
    }
}