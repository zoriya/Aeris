import 'package:flutter/material.dart';
import 'package:mobile/src/models/action.dart' as aeris;
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';
import 'package:mobile/src/widgets/action_card.dart';
import 'package:mobile/src/widgets/action_form.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:expandable/expandable.dart';

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
      underline: Container(),
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
            service.getLogo(logoSize: 30),
            const SizedBox(
              width: 10,
              height: 100,
            ),
            Text(service.name, style: const TextStyle(fontSize: 20))
          ]),
        );
      }).toList(),
    );

    return AerisCardPage(
        body: Padding(
      padding: const EdgeInsets.only(bottom: 20, left: 10, right: 10),
      child: ListView(
        children: [
          const Text("Setup Action",
              style: TextStyle(
                fontSize: 25,
              )),
          const SizedBox(height: 40),
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Align(
                  alignment: Alignment.centerLeft,
                  child: Text(
                    "${availableActions.length} available actions for ",
                  )),
              Align(alignment: Alignment.centerRight, child: serviceDropdown),
            ],
          ),
          const SizedBox(height: 30),
          for (aeris.Action availableAction in availableActions) ...[
            Card(
              elevation: 5,
              child: ExpandablePanel(
                  header: Padding(
                      padding:
                          const EdgeInsets.only(left: 30, top: 20, bottom: 20),
                      child: Text(availableAction.name,
                          style: const TextStyle(fontSize: 15))),
                  collapsed: Container(),
                  expanded: Padding(
                    padding: const EdgeInsets.all(20),
                    child: ActionForm(
                        name: availableAction.name,
                        parametersNames:
                            availableAction.parameters.keys.toList(),
                        onValidate: (parameters) {
                          action.parameters = parameters;
                          print(action.parameters);
                        }),
                  )),
            ),
            const SizedBox(height: 10)
          ]
        ],
      ),
    ));
  }
}
