import 'package:flutter/material.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:form_builder_validators/form_builder_validators.dart';

// Form for an action
class ActionForm extends StatefulWidget {
  // Name of the action
  final String name;
  // Names of the parameters
  final List<String> parametersNames;
  // Initial values of the fields
  final Map<String, String> initValues;

  // On validate callback
  final void Function(Map<String, String>) onValidate;

  const ActionForm(
      {Key? key,
      required this.name,
      required this.parametersNames,
      required this.onValidate,
      this.initValues = const {}})
      : super(key: key);

  @override
  State<ActionForm> createState() => _ActionFormState();
}

class _ActionFormState extends State<ActionForm> {
  final _formKey = GlobalKey<FormBuilderState>();

  @override
  Widget build(BuildContext context) {
    return FormBuilder(
        key: _formKey,
        child: Column(children: [
          ...widget.parametersNames.map((name) => FormBuilderTextField(
                initialValue: (widget.initValues.containsKey(name))
                    ? widget.initValues[name] as String
                    : null,
                name: name,
                decoration: InputDecoration(
                  labelText: name,
                ),
                validator: FormBuilderValidators.compose([
                  FormBuilderValidators.required(context),
                ]),
              )),
          ...[
            ElevatedButton(
              child: const Text("Save"),
              onPressed: () {
                _formKey.currentState!.save();
                if (_formKey.currentState!.validate()) {
                  widget.onValidate(_formKey.currentState!.value
                      .map((key, value) => MapEntry(key, value)));
                }
              },
            ),
            const SizedBox(height: 10)
          ]
        ]
      )
    );
  }
}
