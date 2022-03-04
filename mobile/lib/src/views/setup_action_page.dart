import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/aeris_api.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/trigger.dart';
import 'package:aeris/src/providers/services_provider.dart';
import 'package:aeris/src/views/service_page.dart';
import 'package:aeris/src/widgets/colored_clickable_card.dart';
import 'package:flutter/material.dart';
import 'package:aeris/src/models/action.dart' as aeris;
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/widgets/action_form.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:expandable/expandable.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:get_it/get_it.dart';
import 'package:provider/provider.dart';
import 'package:skeleton_loader/skeleton_loader.dart';

///Page to setup an action
class SetupActionPage extends StatefulWidget {
  const SetupActionPage({Key? key, required this.action, required this.parentReactions, this.parentTrigger}) : super(key: key);

  /// Action to setup
  final aeris.Action action;
  /// Trigger of Parent of the action to setup
  final Trigger? parentTrigger;

  /// reactions of Parent of the action to setup
  final List<Reaction> parentReactions;

  @override
  State<SetupActionPage> createState() => _SetupActionPageState();
}

class _SetupActionPageState extends State<SetupActionPage> {
  Service? serviceState;
  List<ActionTemplate>? availableActions;

  @override
  void initState() {
    super.initState();
    serviceState = widget.action.service;
    availableActions = GetIt.I<AerisAPI>().getActionsFor(serviceState!, widget.action);
  }

  Widget serviceDropdown(List<Service> services) {
    return DropdownButton<Service>(
      value: serviceState,
      elevation: 8,
      underline: Container(),
      onChanged: (service) {
        setState(() {
          serviceState = service;
          availableActions = GetIt.I<AerisAPI>().getActionsFor(service!, widget.action);
        });
      },
      items: services.map<DropdownMenuItem<Service>>((Service service) {
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
  }

  @override
  Widget build(BuildContext context) {

    var cardShape = const RoundedRectangleBorder(
        borderRadius: BorderRadius.all(Radius.circular(10)));

    return AerisCardPage(
        body: Consumer<ServiceProvider>(builder: (context, services, _) => Padding(
      padding: const EdgeInsets.only(bottom: 20, left: 10, right: 10),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: services.connectedServices.isEmpty ?
          ([
            Text(AppLocalizations.of(context).authenticatedToNoService,
              style: const TextStyle(
                fontSize: 25,
              )
            ),
            Padding(padding: const EdgeInsets.all(30), 
              child: ColoredClickableCard(
                color: Theme.of(context)
                    .colorScheme
                    .secondaryContainer,
                text:AppLocalizations.of(context).connectService,
                onTap: () => showAerisCardPage(
                  context, (_) => const ServicePage()).then((value) => setState((){}) //TODO check, might be useless
                )
              )
            )
          ])
          : [ 
            Text(widget.action is Trigger 
                ? AppLocalizations.of(context).setupTrigger
                : AppLocalizations.of(context).setupReaction,
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
                      "${availableActions == null ? 0 : availableActions!.length} ${AppLocalizations.of(context).avalableActionsFor} ",
                    )),
              ),
              Expanded(
                child: Padding(
                  padding: const EdgeInsets.all(8.0),
                  child: Align(
                      alignment: Alignment.centerRight, child: serviceDropdown(services.connectedServices)),
                ),
              ),
            ],
          ),
          const SizedBox(height: 20),
          Text(AppLocalizations.of(context).paramInheritTip),
          const SizedBox(height: 20),
          if (availableActions == null)
            SkeletonLoader(
                builder: Card(shape: cardShape, child: const SizedBox(height: 40), elevation: 5),
                items: 15,
                highlightColor: Theme.of(context).colorScheme.secondary
            )
          else 
            ...[for (ActionTemplate availableAction in availableActions!)
            Card(
              elevation: 5,
                shape: cardShape,
                child: ExpandableNotifier(
                  child: ScrollOnExpand(child: ExpandablePanel(
                  header: Padding(
                      padding:
                          const EdgeInsets.only(left: 30, top: 20, bottom: 20),
                      child: Text(availableAction.displayName(),
                          style: const TextStyle(fontSize: 15))),
                  collapsed: Container(),
                  expanded: Padding(
                    padding: const EdgeInsets.all(20),
                    child: ActionForm(
                        reactionsCandidates: widget.parentReactions,
                        triggerCandidate: widget.parentTrigger,
                        candidate: widget.action,
                        key: Key("${availableAction.name}${availableAction.description}${availableAction.service}"),
                        description: availableAction.description!,
                        name: availableAction.name,
                        parameters: availableAction.parameters.map((param) {
                          if (widget.action.service.name == serviceState!.name && widget.action.name == availableAction.name) {
                            var previousParams = widget.action.parameters.where((element) => element.name == param.name);
                            if (previousParams.isNotEmpty) {
                              param.value = previousParams.first.value;
                            }
                          }
                          return param;
                        }).toList(),
                        onValidate: (parameters) {
                          widget.action.service = serviceState!;
                          widget.action.parameters = ActionParameter.fromJSON(parameters);
                          widget.action.name = availableAction.name;
                          Navigator.of(context).pop();
                        }),
                  )),
            ))),
            const SizedBox(height: 10)
          ]
        ],
      ),
    )));
  }
}
