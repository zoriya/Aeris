import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/action.dart' as aeris;
import 'package:aeris/src/models/trigger.dart';
import 'package:aeris/src/providers/action_catalogue_provider.dart';
import 'package:flutter/material.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:flutter_typeahead/flutter_typeahead.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:provider/provider.dart';
import 'package:recase/recase.dart';
import 'package:tuple/tuple.dart';


typedef Suggestion = Tuple3<int, ActionParameter, ActionTemplate>;

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
  final _formKey = GlobalKey<FormBuilderState>();

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
    print(index);
    suggestions.forEach((element) => print(element.item2.name));
    return suggestions;
  }

  @override
  Widget build(BuildContext context) {
    return Consumer<ActionCatalogueProvider>(
      builder: (__, catalogue, _) => FormBuilder(
      key: _formKey,
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          Text(widget.description, textAlign: TextAlign.left, style: TextStyle(color: Theme.of(context).colorScheme.onSurface)),
          ...widget.parameters.map((param) {
            return Autocomplete<Suggestion>(
              initialValue: TextEditingValue(text: param.value?.toString() ?? ''),
              fieldViewBuilder: (context, textEditingController, focusNode, onSubmit) => 
                FormBuilderTextField(
                  name: param.name,
                  controller: textEditingController,
                  focusNode: focusNode,
                  validator: FormBuilderValidators.compose([
                    FormBuilderValidators.required(context),
                  ]),
                  decoration: InputDecoration(
                    labelText: ReCase(param.name).titleCase,
                    helperText: param.description
                  ),

              ),
              optionsBuilder: (suggestion) => getSuggestions(suggestion.text, catalogue),
              // onSelected: (suggestion) {
              //   var parameterSuggestion = suggestion;
              //   String content = _formKey.currentState!.value[param.name];
              //   print(content);
              //   content = "{${parameterSuggestion.item2}@${parameterSuggestion.item1}}";
              //   print(content);
              // },
              displayStringForOption: (suggestion) {
                var parameterSuggestion = suggestion;
                String content = _formKey.currentState!.value[param.name];
                print(content);
                content = "{${parameterSuggestion.item2}@${parameterSuggestion.item1}}";
                print(content);
                return content;
              },
              optionsViewBuilder: (context, onSelected, inputs) => Align(
                alignment: Alignment.topLeft,
                child: Material(
                  shape: const RoundedRectangleBorder(
                    borderRadius: BorderRadius.all(Radius.circular(10.0)),
                  ),
                  child: SizedBox(
                    height: 60.0 * inputs.length,
                    width: MediaQuery. of(context). size. width * 0.7, // <-- Right here !
                    child: ListView.builder(
                      padding: EdgeInsets.zero,
                      itemCount: inputs.length,
                      shrinkWrap: false,
                      itemBuilder: (BuildContext context, int index) {
                        Suggestion suggestion = inputs.elementAt(index);
                        return ListTile(
                          onTap: () => onSelected(suggestion),
                          isThreeLine: true,
                          dense: true,
                          leading: suggestion.item3.service.getLogo(logoSize: 30),
                          title: Text(suggestion.item2.name),
                          subtitle: Text("${suggestion.item2.description}, from '${suggestion.item3.displayName()}'")
                        );
                      }
                    ),
                  )
                )
            ));}),
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
    ));
  }
}
