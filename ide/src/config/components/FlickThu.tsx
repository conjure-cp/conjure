import * as React from "react";

interface Props {
  nextHandler: () => void;
  prevHandler: () => void;
}

const FlickThru = ({ nextHandler, prevHandler }: Props) => {
  return (
    <>
      <button className="btn btn-light" onClick={prevHandler}>
        🢀
      </button>
      <button className="btn btn-light" onClick={nextHandler}>
        🢂
      </button>
    </>
  );
};

export default FlickThru;
