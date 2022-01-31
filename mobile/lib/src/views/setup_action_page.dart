import 'package:flutter/material.dart';
import 'package:mobile/src/models/action.dart' as aeris;
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:expandable/expandable.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:form_builder_validators/form_builder_validators.dart';

/// Class to get the action in route's arguments
class SetupActionPageArguments {
  final aeris.Action action;

  SetupActionPageArguments(this.action);
}

///Page to setup an action
class SetupActionPage extends StatefulWidget {
  const SetupActionPage({Key? key}) : super(key: key);

  @override
  State<SetupActionPage> createState() => _SetupActionPageState();
}

class _SetupActionPageState extends State<SetupActionPage> {
  @override
  Widget build(BuildContext context) {
    final SetupActionPageArguments arguments =
        ModalRoute.of(context)!.settings.arguments as SetupActionPageArguments;
    aeris.Action action = arguments.action;

    // TODO Call provider
    List<aeris.Action> availableActions = [
      for (int i = 0; i <= 10; i++)
        Trigger(
            last: DateTime.now(),
            service: action.service,
            action: "action",
            parameters: {'key1': 'value1', 'key2': null})
    ];

    final Widget serviceDropdown = DropdownButton<Service>(
      value: action.service,
      elevation: 8,
      underline: Container(
        height: 1,
        color: Theme.of(context).colorScheme.secondary,
      ),
      onChanged: (service) {
        print("Selected ${service!.name}");
        setState(() {
          // TODO Setstate + call api to get available actions
        });
      },
      items: Service.all().map<DropdownMenuItem<Service>>((Service service) {
        return DropdownMenuItem<Service>(
          value: service,
          child: Row(children: [
            service.getLogo(logoSize: 40),
            const SizedBox(
              width: 10,
              height: 100,
            ),
            Text(service.name, style: TextStyle(fontSize: 20))
          ]),
        );
      }).toList(),
    );
    final _formKey = GlobalKey<FormBuilderState>();

    return AerisCardPage(
        body: ListView(
      children: [
        const Text("Setup Action",
            style: TextStyle(
              fontSize: 25,
            )),
        const SizedBox(height: 60),
        Align(alignment: Alignment.centerRight, child: serviceDropdown),
        const SizedBox(height: 10),
        Align(
            alignment: Alignment.centerLeft,
            child: Text("${availableActions.length} available actions")),
        const SizedBox(height: 20),
        for (aeris.Action action in availableActions)
          ExpandablePanel(
              header: Text(action.name),
              collapsed: const SizedBox(height: 10),
              expanded: FormBuilder(
                  key: _formKey,
                  child: Column(children: [
                    ...action.parameters
                        .map((key, value) => MapEntry(
                            key,
                            FormBuilderTextField(
                              initialValue:
                                  (action.parameters.containsKey(key) &&
                                          action.parameters[key] != null)
                                      ? action.parameters[key] as String
                                      : null,
                              name: key,
                              decoration: InputDecoration(
                                labelText: key,
                              ),
                              onChanged: (value) {
                                print(value);
                              } /*TODO apply on object*/,
                              validator: FormBuilderValidators.compose([
                                FormBuilderValidators.required(context),
                              ]),
                            )))
                        .values
                        .toList(),
                    ...[
                      ElevatedButton(
                        child: Text("Save"),
                        onPressed: () {
                          _formKey.currentState!.save();
                          if (_formKey.currentState!.validate()) {
                            print(_formKey.currentState!.value);
                          } else {
                            print("validation failed");
                          }
                        },
                      ),
                      const SizedBox(height: 10)
                    ]
                  ]))),
        const SizedBox(height: 20)
      ],
    ));
  }
}
