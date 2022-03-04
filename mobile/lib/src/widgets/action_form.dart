import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/action.dart' as aeris;
import 'package:aeris/src/models/trigger.dart';
import 'package:aeris/src/providers/action_catalogue_provider.dart';
import 'package:flutter/material.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:provider/provider.dart';
import 'package:recase/recase.dart';
import 'package:tuple/tuple.dart';
import 'package:flutter_typeahead/flutter_typeahead.dart';

class Suggestion extends Tuple3<int, ActionParameter, ActionTemplate> {
  Suggestion(int item1, ActionParameter item2, ActionTemplate item3) : super(item1, item2, item3);

  // Overriding show method
   
  @override
  String toString() {
    return "{${item2.name}@$item1}";
  }
}

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

  List<Suggestion> getSuggestions(String pattern, ActionCatalogueProvider catalogue) {
    List<Suggestion> suggestions = [];
    if (pattern.endsWith("#") == false) return suggestions;
    if (widget.candidate is Trigger) return suggestions;
    if (widget.triggerCandidate != null) {
      Trigger trigger = widget.triggerCandidate!;
      var triggerTemplate = catalogue.triggerTemplates[trigger.service]!.firstWhere(
        (element) => element.name == trigger.name
      );
      for (var parameter in triggerTemplate.returnedValues) {
        suggestions.add(Suggestion(0, parameter, triggerTemplate));
      }
    }
    int index = 1;
    int indexOfCandidate = widget.reactionsCandidates.indexOf(widget.candidate as Reaction);
    for (var reactionCandidate in widget.reactionsCandidates) {
      if (index == indexOfCandidate + 1) break;
      var reactionTemplate = catalogue.reactionTemplates[reactionCandidate.service]!.firstWhere(
        (element) => element.name == reactionCandidate.name
      );
      for (var parameter in reactionTemplate.returnedValues) {
        suggestions.add(Suggestion(index, parameter, reactionTemplate));
      }
      index++;
    }
    return suggestions;
  }

  Map<String, String> values = {};

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
            final textEditingController = TextEditingController(text: values[param.name] ?? param.value?.toString());
            return TypeAheadFormField<Suggestion>(
              key: Key(param.description),
              textFieldConfiguration: TextFieldConfiguration(
                  controller: textEditingController,
                  enableSuggestions: widget.candidate is Reaction,
                  decoration: InputDecoration(
                    labelText: ReCase(param.name).titleCase,
                    helperText: param.description
                  ),
              ),
              onSaved: (value) {
                values[param.name] = value!;
              },
              validator: FormBuilderValidators.compose([
                FormBuilderValidators.required(context),
              ]),
              hideOnEmpty: true,
              suggestionsCallback: (suggestion) => getSuggestions(suggestion, catalogue),
              onSuggestionSelected: (suggestion) {
                textEditingController.text += suggestion.toString();
                values[param.name] = textEditingController.text;
              },
              itemBuilder: (context, suggestion) => ListTile(
                //onTap: () => onSelected(suggestion),
                isThreeLine: true,
                dense: true,
                leading: suggestion.item3.service.getLogo(logoSize: 30),
                title: Text(suggestion.item2.name),
                subtitle: Text("${suggestion.item2.description}, from '${suggestion.item3.displayName()}'")
              ));}),
          ...[
            ElevatedButton(
              child: Text(AppLocalizations.of(context).save),
              onPressed: () {
                print("Pressed");
                if (_formKey.currentState!.validate()) {
                  print("Validated");
                  _formKey.currentState!.save();
                  print(values);
                  widget.onValidate(values);
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
