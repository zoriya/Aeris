import 'package:aeris/src/models/action_parameter.dart';
import 'package:flutter/material.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:recase/recase.dart';

/// Form for an action
class ActionForm extends StatefulWidget {
  /// Name of the action
  final String name;
  /// List of parameters, 'values' are used as default values
  final List<ActionParameter> parameters;
  /// What the action does
  final String description;

  /// On validate callback
  final void Function(Map<String, String>) onValidate;

  const ActionForm(
      {Key? key,
      required this.name,
      required this.description,
      required this.parameters,
      required this.onValidate})
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
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          Text(widget.description, textAlign: TextAlign.left, style: TextStyle(color: Theme.of(context).colorScheme.onSurface)),
          ...widget.parameters.map((param) => FormBuilderTextField(
            initialValue: param.value?.toString(),
            name: param.name,
            decoration: InputDecoration(
              labelText: ReCase(param.name).titleCase,
              helperText: param.description
            ),
            validator: FormBuilderValidators.compose([
              FormBuilderValidators.required(context),
            ]),
          )),
          ...[
            ElevatedButton(
              child: Text(AppLocalizations.of(context).save),
              onPressed: () {
                _formKey.currentState!.save();
                if (_formKey.currentState!.validate()) {
                  widget.onValidate(_formKey.currentState!.value.map((key, value) => MapEntry(key, value)));
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
