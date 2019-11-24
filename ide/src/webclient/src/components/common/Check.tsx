import * as React from 'react'

interface Props {
	checked: boolean
	onChange: () => void
	title: string
}

export const Check = (props: Props) => {
	const id = `${props.title.replace(' ', '')}-check`

	return (
		<div className='input-group mb-3'>
			<div className='input-group-prepend'>
				<div className='input-group-text'>
					<input id={id} type='checkbox' checked={props.checked} onChange={props.onChange} />
				</div>
			</div>
			<label className='form-control' htmlFor={id}>
				{props.title}
			</label>
		</div>
	)
}
