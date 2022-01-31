import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';
import 'package:mobile/src/models/action.dart' as aeris;
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';

/// Class to get the action in route's arguments
class SetupActionPageArguments {
  final aeris.Action action;

  SetupActionPageArguments(this.action);
}

// Page to setup an action
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

    Map<String, <Map<String, String?>>> availableActions;

    for (int i = 0; i <= 10; i++) {"action $i": {'key1': 'value1', 'key2': null}}
    ;

    aeris.Action action = arguments.action;

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

    return AerisCardPage(
        body: Column(
      children: [
        const Text("Setup Action",
            style: TextStyle(
              fontSize: 25,
            )),
        const SizedBox(height: 60),
        Align(alignment: Alignment.centerRight, child: serviceDropdown),
        Align(
            alignment: Alignment.centerLeft,
            child: Text("${availableActions.length} available actions"))
        const SizedBox(height: 10),
        for (var action in availableActions)
          ExpansionPanel( headerBuilder: (context, _) => Text(""),body: Container())
      ],
    ));
  }
}
