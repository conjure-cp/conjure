import * as React from "react"
import * as ReactDOM from "react-dom"

interface Props {
    message: string
}

const Error = (props: Props) => 
    (
        <p style={{"color": "red"}}>{props.message}</p>
    )


export default Error