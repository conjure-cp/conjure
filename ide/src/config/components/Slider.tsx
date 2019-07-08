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
        marginLeft: 0,
        marginTop: 35,
        zIndex: 2,
        width: 10,
        height: 10,
        border: 0,
        textAlign: "center",
        cursor: "pointer",
        borderRadius: "50%",
        backgroundColor: "red",
        color: "black"
      }}
      {...getHandleProps(id)}
    >
      <div style={{ fontFamily: "Roboto", fontSize: 20, marginTop: -35 }}>
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
  backgroundColor: "skyblue"
} as React.CSSProperties

interface Props {
  sliderChangeHandler: (val: number) => void
  domain: number[]
  values: number[]
}

export function MySlider(props: Props) {
  return (
    <Slider
      rootStyle={sliderStyle}
      domain={props.domain}
      step={1}
      mode={2}
      values={props.values}
      onChange={values => {
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
