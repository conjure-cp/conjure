import * as React from "react";
import * as ReactDOM from "react-dom";
import { FormikProps, FieldProps, Field } from "formik";

const Checkbox = (props: any) => {
  return (
    <Field name={props.name}>
      {({ field, form }: FieldProps) => (
        <div className="input-group mb-3">
          <div className="input-group-prepend">
            <div className="input-group-text">
              <input
                type="checkbox"
                {...props}
                checked={field.value.includes(props.value)}
                onChange={() => {
                  if (field.value.includes(props.value)) {
                    const nextValue = field.value.filter(
                      (value: string) => value !== props.value
                    );
                    form.setFieldValue(props.name, nextValue);
                  } else {
                    const nextValue = field.value.concat(props.value);
                    form.setFieldValue(props.name, nextValue);
                  }
                }}
              />
            </div>
          </div>
          <label className="form-control">{props.label}</label>
        </div>
      )}
    </Field>
  );
};

export default Checkbox;
