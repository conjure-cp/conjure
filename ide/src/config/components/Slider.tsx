import { Slider, Handles, Tracks, Rail } from "react-compound-slider"
import * as React from "react"

export function Handle({
  handle: { id, value, percent },
  getHandleProps
}: any) {
  return (
    <div
      style={{
        left: `${percent}%`,
        position: "absolute",
        marginLeft: -15,
        marginTop: 25,
        zIndex: 2,
        width: 30,
        height: 30,
        border: 0,
        textAlign: "center",
        cursor: "pointer",
        borderRadius: "50%",
        backgroundColor: "#2C4870",
        color: "#333"
      }}
      {...getHandleProps(id)}
    >
      <div style={{ fontFamily: "Roboto", fontSize: 11, marginTop: -35 }}>
        {value}
      </div>
    </div>
  )
}

export function Track({ source, target, getTrackProps }: any) {
  // your own track component
  return (
    <div
      style={{
        position: "absolute",
        height: 10,
        zIndex: 1,
        marginTop: 35,
        backgroundColor: "#546C91",
        borderRadius: 5,
        cursor: "pointer",
        left: `${source.percent}%`,
        width: `${target.percent - source.percent}%`
      }}
      {...getTrackProps()} // this will set up events if you want it to be clickeable (optional)
    />
  )
}

export const sliderStyle = {
  // Give the slider some width
  position: "relative",
  width: "100%",
  height: 80,
  border: "1px solid steelblue"
} as React.CSSProperties

export const railStyle = {
  position: "absolute",
  width: "100%",
  height: 10,
  marginTop: 35,
  borderRadius: 5,
  backgroundColor: "#8B9CB6"
} as React.CSSProperties

interface Props {
  sliderChangeHandler: (val: number) => void
}

export function MySlider(props: Props) {
  return (
    <Slider
      rootStyle={sliderStyle}
      domain={[1, 5]}
      step={1}
      mode={2}
      values={[1]}
      onChange={values => {
        console.log("SLIDINg", values)
        props.sliderChangeHandler(values[0])
      }}
    >
      <Rail>
        {(
          { getRailProps } // adding the rail props sets up events on the rail
        ) => <div style={railStyle} {...getRailProps()} />}
      </Rail>
      <Handles>
        {({ handles, getHandleProps }) => (
          <div className="slider-handles">
            {handles.map(handle => (
              <Handle
                key={handle.id}
                handle={handle}
                getHandleProps={getHandleProps}
              />
            ))}
          </div>
        )}
      </Handles>
      <Tracks right={false}>
        {({ tracks, getTrackProps }) => (
          <div className="slider-tracks">
            {tracks.map(({ id, source, target }) => (
              <Track
                key={id}
                source={source}
                target={target}
                getTrackProps={getTrackProps}
              />
            ))}
          </div>
        )}
      </Tracks>
    </Slider>
  )
}
