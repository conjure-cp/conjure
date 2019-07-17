import * as React from "react";

interface Props {
  clickHandler: () => void;
  x: number;
  playing: boolean;
}

const Play = ({ clickHandler, x, playing }: Props) => {
  let polys = !playing ? (
    <polygon points="0,0 50,30 0,60" />
  ) : (
    [
      <polygon key="0" points="0,0 15,0 15,60 0,60" />,
      <polygon key="1" points="25,0 40,0 40,60 25,60" />
    ]
  );
  return (
    <svg
      className="button"
      transform={`translate(${x},0) scale(0.5)`}
      width="50"
      height="60"
      onClick={clickHandler}
    >
      {polys}
    </svg>
  );
};

export default Play;
