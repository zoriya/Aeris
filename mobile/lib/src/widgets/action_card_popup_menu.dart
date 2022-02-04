import 'package:flutter/material.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/views/setup_action_page.dart';
import 'package:mobile/src/widgets/aeris_popup_menu.dart';
import 'package:mobile/src/widgets/aeris_popup_menu_item.dart';
import 'package:mobile/src/models/action.dart' as aeris;

class ActionCardPopupMenu extends StatelessWidget {
  const ActionCardPopupMenu({
    Key? key,
    required this.action,
    required this.then,
  }) : super(key: key);

  final aeris.Action action;

  final void Function() then;

  @override
  Widget build(BuildContext context) {
    return AerisPopupMenu(
        onSelected: (value) {
          Map object = value as Map;
          Navigator.pushNamed(context, object['route'] as String,
                  arguments: object['params'])
              .then((r) {
            then();
            return r;
          });
        },
        icon: Icons.more_vert,
        itemBuilder: (context) => [
              AerisPopupMenuItem(
                  context: context,
                  icon: Icons.settings,
                  title: "Modify",
                  value: {
                    'route': "/pipeline/action/mod",
                    'params': SetupActionPageArguments(action),
                  } /* TODO Define mod route*/),
              AerisPopupMenuItem(
                  context: context,
                  icon: Icons.delete,
                  title: "Delete",
                  value: "/pipeline/action/del",
                  enabled: action is Reaction
                  // TODO Delete from parent pipeline
                  /* TODO Define delete route*/
                  ),
            ]);
  }
}
