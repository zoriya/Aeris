import 'package:aeris/src/models/action.dart' as aeris;
import 'package:aeris/src/models/action_parameter.dart';
import 'package:expandable/expandable.dart';
import 'package:flutter/material.dart';

class ActionDetailCard extends StatelessWidget {
  ///The source of information
  final aeris.Action action;
  ///The popup menu to update action
  final Widget popupMenu;

  const ActionDetailCard(
      {Key? key,
      required this.action,
      required this.popupMenu})
      : super(key: key);
  
  Widget paramView(BuildContext context, ActionParameter parameter) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            Chip(
              backgroundColor: Theme.of(context).colorScheme.secondary,
              label: Text(parameter.name,
                style: TextStyle(
                  color: Theme.of(context).colorScheme.onSecondary
                )
              ),
            ),
            Flexible(child: Padding(
              padding: const EdgeInsets.only(left: 10),
              child: Text(parameter.value!.toString(), overflow: TextOverflow.ellipsis)
            ))
          ]
        ),
        Text(parameter.description,
          style: TextStyle(
            color: Theme.of(context).colorScheme.onSurface,
          )
        ),
    ]);
  }

  @override
  Widget build(BuildContext context) {
    return Card(
      elevation: 5,
      shape: const RoundedRectangleBorder(
        borderRadius: BorderRadius.all(Radius.circular(20))
      ),
      child: ExpandableNotifier(
        child: ScrollOnExpand(
          child: ExpandablePanel(
            collapsed: Container(),
            header: Padding(
              padding: const EdgeInsets.only(bottom: 15, top: 15, left: 15, right: 6),
              child: Row(
                children: [
                  Expanded(child: action.service.getLogo(logoSize: 50), flex: 2),
                  Expanded(
                    child: Text(
                      action.displayName,
                      style: TextStyle(
                        color: Theme.of(context).colorScheme.onSurface,
                        fontSize: 15
                      ),
                      textAlign: TextAlign.center,
                    ),
                    flex: 8
                  ),
                ],
              ),
            ),
            expanded: Padding(
              padding: const EdgeInsets.only(left: 30, right: 30, bottom: 20, top: 10),
              child: action.parameters.isEmpty
              ? Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Text("No parameter"), //TODO translta
                    popupMenu
                  ],
              )
              : Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Align(
                    alignment: Alignment.centerRight,
                    child: popupMenu,
                  ),
                 ...[for (var parameter in action.parameters) 
                    Padding(
                      padding: const EdgeInsets.only(top: 10),
                      child: paramView(context, parameter)
                    )]
                ]
              )
            )
        )
      )
    ));
  }
}
