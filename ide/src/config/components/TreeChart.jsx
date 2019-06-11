import * as React from "react"
import * as ReactDOM from "react-dom"

class TreeChart extends React.Component {

    componentDidMount() {
        var el = ReactDOM.findDOMNode(this);
        d3Tree.create(el, {
            width: '100%',
            height: '300px'
        }, this.getChartState());
    }

    componentDidUpdate() {
        var el = ReactDOM.findDOMNode(this);
        d3Tree.update(el, this.getChartState());
    }

    getChartState() {
        return {
            data: this.props.data
        };
    }

    render() {
        return (
            <div className="TreeChart"></div>
        );
    }
}

export default TreeChart