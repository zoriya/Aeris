import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/aeris_api.dart';
import 'package:flutter/material.dart';
import 'package:aeris/src/models/action.dart' as aeris;
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/widgets/action_form.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:expandable/expandable.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:get_it/get_it.dart';

///Page to setup an action
class SetupActionPage extends StatefulWidget {
  const SetupActionPage({Key? key, required this.action}) : super(key: key);

  /// Action to setup
  final aeris.Action action;

  @override
  State<SetupActionPage> createState() => _SetupActionPageState();
}

class _SetupActionPageState extends State<SetupActionPage> {
  Service? serviceState;
  late List<ActionTemplate> availableActions;

  @override
  void initState() {
    super.initState();
    availableActions = [];
  }

  @override
  Widget build(BuildContext context) {
    serviceState ??= widget.action.service;

    GetIt.I<AerisAPI>().getActionsFor(serviceState!, widget.action).then((actions) => setState(() {
      availableActions = actions;
    }));


    final Widget serviceDropdown = DropdownButton<Service>(
      value: serviceState,
      elevation: 8,
      underline: Container(),
      onChanged: (service) {
        setState(() {
          serviceState = service;
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
            Text(service.name, style: const TextStyle(fontSize: 15))
          ]),
        );
      }).toList(),
    );

    return AerisCardPage(
        body: Padding(
      padding: const EdgeInsets.only(bottom: 20, left: 10, right: 10),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(AppLocalizations.of(context).setupAction,
              style: const TextStyle(
                fontSize: 25,
              )),
          const SizedBox(height: 40),
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Expanded(
                child: Align(
                    alignment: Alignment.centerLeft,
                    child: Text(
                      "${availableActions.length} ${AppLocalizations.of(context).avalableActionsFor} ",
                    )),
              ),
              Expanded(
                child: Padding(
                  padding: const EdgeInsets.all(8.0),
                  child: Align(
                      alignment: Alignment.centerRight, child: serviceDropdown),
                ),
              ),
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
                        initValues: widget.action.parameters,
                        onValidate: (parameters) {
                          widget.action.service = serviceState!;
                          widget.action.parameters = parameters;
                          widget.action.name = availableAction.name;
                          Navigator.of(context).pop();
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
