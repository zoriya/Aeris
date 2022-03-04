import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/action.dart' as aeris;
import 'package:aeris/src/models/trigger.dart';
import 'package:aeris/src/providers/action_catalogue_provider.dart';
import 'package:flutter/material.dart';
import 'package:flutter_typeahead/flutter_typeahead.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:provider/provider.dart';
import 'package:recase/recase.dart';
import 'package:tuple/tuple.dart';

/// Form for an action
class ActionForm extends StatefulWidget {
  /// Name of the action
  final String name;
  /// List of parameters, 'values' are used as default values
  final List<ActionParameter> parameters;
  /// What the action does
  final String description;

  /// The Action that will be eventually filled by the form
  final aeris.Action candidate;
  /// The trigger candidate in the parent form page
  final Trigger? triggerCandidate;
  /// The trigger candidate in the parent form page
  final List<Reaction> reactionsCandidates;

  /// On validate callback
  final void Function(Map<String, String>) onValidate;

  const ActionForm(
      {Key? key,
      required this.name,
      required this.description,
      required this.parameters,
      required this.onValidate,
      required this.candidate,
      this.triggerCandidate,
      required this.reactionsCandidates,
      })
      : super(key: key);

  @override
  State<ActionForm> createState() => _ActionFormState();
}

class _ActionFormState extends State<ActionForm> {
  final _formKey = GlobalKey<FormState>();
  Map<String, String> formValues = {};

  List getSuggestions(String pattern, ActionCatalogueProvider catalogue) {
    List<Tuple3<int, ActionParameter, ActionTemplate>> suggestions = [];
    if (pattern.endsWith("#") == false) return suggestions;
    print(widget.candidate.runtimeType);
    if (widget.candidate is Trigger) return suggestions;
    if (widget.triggerCandidate != null) {
      Trigger trigger = widget.triggerCandidate!;
      var triggerTemplate = catalogue.triggerTemplates[trigger.service]!.firstWhere(
        (element) => element.name == trigger.name
      );
      for (var parameter in triggerTemplate.returnedValues) {
        suggestions.add(Tuple3(0, parameter, triggerTemplate));
      }
    }
    int index = 1;
    for (var reactionCandidate in widget.reactionsCandidates) {
      var reactionTemplate = catalogue.triggerTemplates[reactionCandidate.service]!.firstWhere(
        (element) => element.name == reactionCandidate.name
      );
      for (var parameter in reactionTemplate.returnedValues) {
        suggestions.add(Tuple3(index, parameter, reactionTemplate));
      }
      index++;
    }
    print(index);
    suggestions.forEach((element) => print(element.item2.name));
    return suggestions;
  }

  @override
  Widget build(BuildContext context) {
    return Consumer<ActionCatalogueProvider>(
      builder: (__, catalogue, _) => Form(
      key: _formKey,
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          Text(widget.description, textAlign: TextAlign.left, style: TextStyle(color: Theme.of(context).colorScheme.onSurface)),
          ...widget.parameters.map((param) {
            return TypeAheadFormField(
              initialValue: formValues[param.name] ?? param.value?.toString(),
              validator: FormBuilderValidators.compose([
                FormBuilderValidators.required(context),
              ]),
              hideOnEmpty: true,
              autoFlipDirection: true,
              textFieldConfiguration: TextFieldConfiguration(
                decoration: InputDecoration(
                  labelText: ReCase(param.name).titleCase,
                  helperText: param.description
                ),
              ),
              suggestionsCallback: (suggestion) => getSuggestions(suggestion, catalogue),
              noItemsFoundBuilder: (_) => Container(),
              onSuggestionSelected: (suggestion) {
                var parameterSuggestion = suggestion as Tuple3<int, ActionParameter, ActionTemplate>;
                String content = formValues[param.name]!;
                content += "{${parameterSuggestion.item2}@${parameterSuggestion.item1}}";
                print(content);
                setState(() {
                  formValues[param.name] = content;
                });
              },
              itemBuilder: (context, input) {
                var suggestion = input as Tuple3<int, ActionParameter, ActionTemplate>;
                return ListTile(
                  isThreeLine: true,
                  dense: true,
                  leading: suggestion.item3.service.getLogo(logoSize: 30),
                  title: Text(suggestion.item2.name),
                  subtitle: Text("${suggestion.item2.description}, from '${suggestion.item3.displayName()}'")
                );
              },
              onSaved: (value) => setState(() {
                formValues[param.name] = value!;
              }),
            );
          }),
          ...[
            ElevatedButton(
              child: Text(AppLocalizations.of(context).save),
              onPressed: () {
                if (_formKey.currentState!.validate()) {
                  _formKey.currentState!.save();
                  widget.onValidate(formValues.map((key, value) => MapEntry(key, value)));
                }
              },
            ),
            const SizedBox(height: 10)
          ]
        ]
      )
    ));
  }
}
