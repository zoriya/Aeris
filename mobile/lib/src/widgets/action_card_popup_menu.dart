import 'package:flutter/material.dart';
import 'package:mobile/src/views/setup_action_page.dart';
import 'package:mobile/src/widgets/aeris_popup_menu.dart';
import 'package:mobile/src/widgets/aeris_popup_menu_item.dart';
import 'package:mobile/src/models/action.dart' as aeris;

/// [StatelessWidget] displayed as a PopupMenu
class ActionCardPopupMenu extends StatelessWidget {
  const ActionCardPopupMenu({
    Key? key,
    required this.action,
    required this.then,
    required this.deletable,
  }) : super(key: key);

  /// Action to trigger
  final aeris.Action action;

  /// Function to trigger when PopupMenu option is selected
  final void Function() then;

  /// Deletable characteristic
  final bool deletable;

  @override
  Widget build(BuildContext context) {
    return AerisPopupMenu(
      onSelected: (value) {
        Map object = value as Map;
        Navigator.pushNamed(context, object['route'] as String, arguments: object['params']).then((r) {
          then();
          return r;
        });
      },
      icon: Icons.more_vert,
      itemBuilder: (context) => [
        AerisPopupMenuItem(
          context: context,
          icon: Icons.settings,
           ///TODO translate
          title: "Modify",
          value: {
            'route': "/pipeline/action/mod",
            'params': SetupActionPageArguments(action),
          }),
        AerisPopupMenuItem(
          context: context,
          icon: Icons.delete,
           ///TODO translate
          title: "Delete",
          value: "/pipeline/action/del",
          enabled: deletable,
          // TODO Delete from parent pipeline
          /* TODO Define delete route*/
          ),
      ]
    );
  }
}
