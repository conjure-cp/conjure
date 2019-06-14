import * as React from "react";
import * as ReactDOM from "react-dom";

interface Props {
  collapseHandler?: () => void;
  startCollapsed?: boolean;
  title: string;
  id: string;
  children: JSX.Element | JSX.Element[];
}

const StageHeader = (props: Props) => {
  const id = props.id;

  const outerClass = props.startCollapsed ? "collapsed" : "";
  const innerClass = props.startCollapsed ? "collapse" : "collapse show";

  return (
    <div className="card mb-4">
      <h5 className="card-header">
        <a
          className={outerClass}
          data-toggle="collapse"
          data-target={"#" + id}
          onClick={() => {
            if (props.collapseHandler) {
              props.collapseHandler();
            }
            console.log("!!!!!!!!");
          }}
        >
          {props.title} Options
        </a>
      </h5>
      <div className={innerClass} id={id}>
        <div className="card-body">{props.children}</div>
      </div>
    </div>
  );
};

export default StageHeader;
