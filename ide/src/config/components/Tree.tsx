import * as React from "react"
import * as ReactDOM from "react-dom"

interface Props {
}

interface State {
}

class Tree extends React.Component<Props, State>{

    constructor(props: any) {
        super(props)
        this.state = {
        }
    }

    render() {
        return (
            <div>
                This is the tree
            </div>
        )
    }
}

export default Tree