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
  }) : super(key: key);

  final aeris.Action action;

  @override
  Widget build(BuildContext context) {
    return AerisPopupMenu(
        onSelected: (value) {
          Map object = value as Map;
          Navigator.pushNamed(context, object['route'] as String,
              arguments: object['params']);
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
                enabled: action is Reaction, /* TODO Define delete route*/
              ),
            ]);
  }
}
